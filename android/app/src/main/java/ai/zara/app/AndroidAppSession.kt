package ai.zara.app

import ai.zara.app.assistant.AndroidAssistantRolePlatform
import ai.zara.app.assistant.AssistantRoleController
import ai.zara.app.assistant.AssistantRoleVoiceGuard
import ai.zara.app.assistant.AssistantVoiceOwnership
import ai.zara.app.auth.AndroidEnrollmentRepository
import ai.zara.app.auth.EnrollmentRepository
import ai.zara.app.auth.EnrollmentState
import ai.zara.app.auth.JeroMqCurveKeyCodec
import ai.zara.app.device.AndroidAppLauncher
import ai.zara.app.device.AndroidUriLauncher
import ai.zara.app.device.DeviceCapabilityRegistry
import ai.zara.app.device.OpenAppAdapter
import ai.zara.app.device.OpenUriAdapter
import ai.zara.app.device.RegistryDeviceActionHandler
import ai.zara.app.diagnostics.LocalRuntimeDiagnostics
import ai.zara.app.localai.LocalAiServiceClient
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalGenerationRequest
import ai.zara.app.localai.LocalTtsState
import ai.zara.app.runtime.AndroidTextSessionController
import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.AudioOutputFormat
import ai.zara.app.runtime.ClientStateStore
import ai.zara.app.runtime.ConnectedTextSession
import ai.zara.app.runtime.JeroMqTextDealerFactory
import ai.zara.app.runtime.LocalQueryResult
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalServerState
import ai.zara.app.runtime.LocalZaraServer
import ai.zara.app.runtime.RestorableClientState
import ai.zara.app.runtime.RuntimeEvent
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.runtime.ServerProfile
import ai.zara.app.runtime.TextTurnResult
import ai.zara.app.runtime.ZaraTextClientActor
import ai.zara.app.runtime.reduce
import ai.zara.app.runtime.toRuntimeReadiness
import ai.zara.app.prolog.AndroidPortableSemanticAssetSource
import ai.zara.app.prolog.NativeTreallaBridge
import ai.zara.app.prolog.PortableSemanticAssetStager
import ai.zara.app.prolog.PrologDocument
import ai.zara.app.prolog.LocalPrologCommand
import ai.zara.app.prolog.LocalNaturalLanguageExpertRouter
import ai.zara.app.prolog.PrologExampleCatalog
import ai.zara.app.prolog.PrologSource
import ai.zara.app.prolog.PrologSourceAnalyzer
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.prolog.PrologWorkspaceCatalog
import ai.zara.app.voice.AndroidAudioFocusPlatform
import ai.zara.app.voice.AndroidAudioRoutePlatform
import ai.zara.app.voice.AndroidPcmOutput
import ai.zara.app.voice.AndroidPcmRecorder
import ai.zara.app.voice.AudioFocusController
import ai.zara.app.voice.AudioRouteController
import ai.zara.app.voice.AudioRouteSnapshot
import ai.zara.app.voice.AuthenticatedVoiceIngress
import ai.zara.app.voice.ManualVoiceCapture
import ai.zara.app.voice.ManualVoiceSessionCoordinator
import ai.zara.app.voice.ManualVoiceState
import ai.zara.app.voice.PushToTalkController
import ai.zara.app.voice.VoiceDiagnosticFailure
import ai.zara.app.voice.VoicePlaybackController
import ai.zara.app.voice.VoiceStreamSinkActor
import ai.zara.app.voice.VoiceStreamState
import android.content.Context
import android.content.Intent
import java.io.File
import java.util.UUID
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class AndroidAppSession(context: Context) : AutoCloseable {
    private val enrollment: EnrollmentRepository = AndroidEnrollmentRepository.create(context)
    private val stateStore = ClientStateStore(File(context.noBackupFilesDir, "zara/client-state.bin"))
    private val actor: ZaraTextClientActor
    private val controller: AndroidTextSessionController
    private val assistantRolePlatform: AndroidAssistantRolePlatform
    private val assistantRoleController: AssistantRoleController
    private val assistantVoiceGuard: AssistantRoleVoiceGuard
    private val voice: ManualVoiceSessionCoordinator
    private val voiceStreamSink: VoiceStreamSinkActor
    private val audioRouteController: AudioRouteController
    private val prologWorkspace = PrologWorkspace(File(context.filesDir, "prolog-workspace"))
    private val diagnostics = LocalRuntimeDiagnostics(
        File(context.noBackupFilesDir, "zara/diagnostics/local-runtime.log"),
    )
    private val localServer: LocalZaraServer
    private val localAi = LocalAiServiceClient(context)
    @Volatile private var latestVoiceStreamState: VoiceStreamState? = null
    @Volatile private var latestVoiceStreamFailure: String? = null
    @Volatile private var latestAudioRoute: AudioRouteSnapshot? = null
    @Volatile private var voiceStreamObserver: ((VoiceStreamState?, String?) -> Unit)? = null
    @Volatile private var runtimeStateObserver: ((RuntimeState) -> Unit)? = null
    @Volatile private var playbackRuntimeSessionId: String? = null
    @Volatile private var runtimeMode: RuntimeMode = RuntimeMode.Auto
    private val voiceExecutor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-android-voice-control").apply { isDaemon = true }
    }

    init {
        diagnostics.record(
            "session.init",
            mapOf(
                "version" to BuildConfig.VERSION_NAME,
                "version_code" to BuildConfig.VERSION_CODE,
                "source_sha" to BuildConfig.SOURCE_SHA,
            ),
        )
        prologWorkspace.seedExamples(PrologExampleCatalog.examples)
        diagnostics.record(
            "prolog_workspace.seeded",
            mapOf("sources" to prologWorkspace.listSources().joinToString(",") { it.name }),
        )
        val stagedSemanticAssets = PortableSemanticAssetStager(
            File(context.noBackupFilesDir, "zara/prolog-runtime"),
        ).stageAll(AndroidPortableSemanticAssetSource(context.assets))
        diagnostics.record(
            "semantic_assets.staged",
            mapOf("core" to stagedSemanticAssets.coreFile.name),
        )
        localServer = LocalZaraServer(
            bridge = NativeTreallaBridge(),
            corePath = stagedSemanticAssets.coreFile.absolutePath,
            workspace = prologWorkspace,
            diagnostics = diagnostics::record,
        )
        localServer.start().whenComplete { state, error ->
            if (error != null) {
                diagnostics.record("local_server.start.failed", emptyMap(), error)
            } else if (state != null) {
                diagnostics.record(
                    "local_server.start.complete",
                    mapOf(
                        "phase" to state.phase.name.lowercase(),
                        "generation" to state.generation,
                        "failure" to (state.failure ?: "none"),
                    ),
                )
            }
        }
        val restored = stateStore.load()
        var initial = restored?.let(RuntimeState::fromRestored) ?: RuntimeState.initial()
        initial = reduce(
            initial,
            RuntimeEvent.EnrollmentObserved(enrollment.state().toRuntimeReadiness()),
        )
        val deviceActionHandler = RegistryDeviceActionHandler(
            DeviceCapabilityRegistry(
                listOf(
                    OpenUriAdapter(AndroidUriLauncher(context)),
                    OpenAppAdapter(AndroidAppLauncher(context)),
                )
            )
        )
        actor = ZaraTextClientActor(
            dealerFactory = JeroMqTextDealerFactory(enrollment),
            audioOutputFormats = listOf(AudioOutputFormat.pcmS16leMono(24_000)),
            deviceCapabilities = deviceActionHandler::availableCapabilities,
            deviceActionHandler = deviceActionHandler,
        )
        controller = AndroidTextSessionController(initial, actor)
        voice = ManualVoiceSessionCoordinator(
            PushToTalkController(
                capture = ManualVoiceCapture(AuthenticatedVoiceIngress(actor)),
                recorder = AndroidPcmRecorder(),
                onRecorderFailure = ::reportVoiceStreamFailure,
            )
        )
        assistantVoiceGuard = AssistantRoleVoiceGuard(::cancelAssistantCaptureForRoleLoss)
        assistantRolePlatform = AndroidAssistantRolePlatform(context.applicationContext)
        assistantRoleController = AssistantRoleController(
            platform = assistantRolePlatform,
            outcomeObserver = { roleOutcome ->
                controller.observeAssistantRole(roleOutcome)
                val role = state().assistantRole
                assistantVoiceGuard.onRoleChanged(role)
            },
        )
        voiceStreamSink = VoiceStreamSinkActor(
            playbackFactory = { sessionId ->
                val audioFocus = AudioFocusController(
                    platform = AndroidAudioFocusPlatform(context.applicationContext),
                    onLoss = { interruptVoicePlayback() },
                )
                VoicePlaybackController(
                    output = AndroidPcmOutput(),
                    sessionId = sessionId,
                    audioFocus = audioFocus,
                )
            },
            stateObserver = { streamState ->
                latestVoiceStreamState = streamState
                latestVoiceStreamFailure = null
                voiceStreamObserver?.invoke(streamState, null)
            },
            failureObserver = { error -> reportVoiceStreamFailure(error) },
        )
        audioRouteController = AudioRouteController(
            platform = AndroidAudioRoutePlatform(context.applicationContext),
            onChanged = { route -> latestAudioRoute = route },
            onRouteInterrupted = { _, _ -> interruptVoicePlayback() },
        )
        try {
            audioRouteController.start()
        } catch (error: Throwable) {
            reportVoiceStreamFailure(error)
        }
        actor.setVoiceStreamObserver { event ->
            voiceStreamSink.accept(event)
        }
        actor.setVoiceStreamFailureObserver(::reportVoiceStreamFailure)
        controller.setStateObserver(::observeRuntimeState)
    }

    fun state(): RuntimeState = controller.state()

    fun runtimeMode(): RuntimeMode = runtimeMode

    fun setRuntimeMode(mode: RuntimeMode) {
        val previous = runtimeMode
        if (mode == RuntimeMode.Local && previous != RuntimeMode.Local) {
            controller.suspendRemoteForLocalMode()
        }
        runtimeMode = mode
        diagnostics.record(
            "runtime_mode.changed",
            mapOf("from" to previous.name.lowercase(), "to" to mode.name.lowercase()),
        )
    }

    fun localServerState(): LocalServerState = localServer.state()

    fun setLocalServerObserver(observer: ((LocalServerState) -> Unit)?) {
        localServer.setStateObserver(observer)
    }

    fun localAiState(): CompletableFuture<LocalAiState> = localAi.state()

    fun exportDiagnostics(): String {
        val server = localServer.state()
        val aiFuture = localAi.state()
        val aiState = if (
            aiFuture.isDone &&
            !aiFuture.isCompletedExceptionally &&
            !aiFuture.isCancelled
        ) {
            runCatching { aiFuture.get() }.getOrNull()
        } else {
            null
        }
        diagnostics.record(
            "diagnostics.export",
            mapOf(
                "local_server_phase" to server.phase.name.lowercase(),
                "local_ai_phase" to (aiState?.phase?.name?.lowercase() ?: "unknown"),
            ),
        )
        return diagnostics.export(
            mapOf(
                "version" to BuildConfig.VERSION_NAME,
                "version_code" to BuildConfig.VERSION_CODE,
                "source_sha" to BuildConfig.SOURCE_SHA,
                "runtime_mode" to runtimeMode.name.lowercase(),
                "local_server_phase" to server.phase.name.lowercase(),
                "local_server_generation" to server.generation,
                "local_server_sources" to server.loadedSources.joinToString(","),
                "local_server_failure" to (server.failure ?: "none"),
                "local_ai_phase" to (aiState?.phase?.name?.lowercase() ?: "unknown"),
                "local_ai_generation" to (aiState?.generation ?: -1),
                "local_ai_model" to (aiState?.model?.let { "${it.id}@${it.version}" } ?: "none"),
                "local_ai_quantization" to (aiState?.model?.quantization?.wireName ?: "none"),
                "enrollment" to state().enrollment.toString(),
                "connection" to state().server::class.java.simpleName,
            )
        )
    }

    fun clearDiagnostics() {
        diagnostics.clear()
    }

    fun localTtsState(): CompletableFuture<LocalTtsState> = localAi.ttsState()

    fun speakLocal(text: String): CompletableFuture<Unit> = localAi.speak(text)

    fun stopLocalSpeech() = localAi.stopSpeech()

    fun prologSources(): List<PrologSource> = prologWorkspace.listSources()

    fun exportPrologWorkspace(): String = prologWorkspace.exportBundle()

    fun renamePrologSource(from: String, to: String): CompletableFuture<List<PrologSource>> =
        mutatePrologWorkspace { prologWorkspace.renameSource(from, to) }

    fun deletePrologSource(name: String): CompletableFuture<List<PrologSource>> =
        mutatePrologWorkspace {
            check(prologWorkspace.deleteSource(name)) { "Prolog source could not be deleted" }
        }

    fun importPrologWorkspace(bundle: String): CompletableFuture<List<PrologSource>> =
        mutatePrologWorkspace { prologWorkspace.importBundle(bundle) }

    fun analyzePrologSource(name: String, text: String): PrologDocument =
        PrologSourceAnalyzer.analyze(name, text)

    fun savePrologSource(name: String, text: String): CompletableFuture<PrologDocument> {
        val document = PrologSourceAnalyzer.analyze(name, text)
        if (document.diagnostics.isNotEmpty()) {
            return CompletableFuture.failedFuture(
                IllegalArgumentException(document.diagnostics.joinToString("; ") { it.message }),
            )
        }
        val before = prologWorkspace.listSources().firstOrNull { it.name == name }
        prologWorkspace.saveSource(name, text)
        return localServer.reload().thenCompose { state ->
            if (state.phase == LocalServerPhase.READY) {
                CompletableFuture.completedFuture(document)
            } else {
                if (before == null) {
                    check(prologWorkspace.deleteSource(name)) { "Invalid source could not be rolled back" }
                } else {
                    prologWorkspace.saveSource(before.name, before.text)
                }
                localServer.reload().thenCompose { restored ->
                    val message = state.failure ?: "Prolog source failed to load"
                    if (restored.phase != LocalServerPhase.READY) {
                        CompletableFuture.failedFuture<PrologDocument>(
                            IllegalStateException("$message; previous runtime also failed to restore"),
                        )
                    } else {
                        CompletableFuture.failedFuture<PrologDocument>(IllegalArgumentException(message))
                    }
                }
            }
        }
    }

    fun reloadLocalServer(): CompletableFuture<LocalServerState> = localServer.reload()

    fun queryLocalProlog(query: String): CompletableFuture<LocalQueryResult> =
        localServer.query(query)

    private fun mutatePrologWorkspace(mutation: () -> Unit): CompletableFuture<List<PrologSource>> {
        val before = prologWorkspace.listSources()
        try {
            mutation()
        } catch (error: Throwable) {
            restorePrologWorkspace(before)
            return CompletableFuture.failedFuture(error)
        }
        return localServer.reload().thenCompose { state ->
            if (state.phase == LocalServerPhase.READY) {
                CompletableFuture.completedFuture(prologWorkspace.listSources())
            } else {
                restorePrologWorkspace(before)
                localServer.reload().thenCompose { restored ->
                    val message = state.failure ?: "Prolog workspace failed to load"
                    if (restored.phase == LocalServerPhase.READY) {
                        CompletableFuture.failedFuture<List<PrologSource>>(IllegalArgumentException(message))
                    } else {
                        CompletableFuture.failedFuture<List<PrologSource>>(
                            IllegalStateException("$message; previous workspace also failed to restore"),
                        )
                    }
                }
            }
        }
    }

    private fun restorePrologWorkspace(sources: List<PrologSource>) {
        prologWorkspace.listSources().forEach { prologWorkspace.deleteSource(it.name) }
        sources.forEach { prologWorkspace.saveSource(it.name, it.text) }
    }

    fun voiceState(): ManualVoiceState = voice.state()

    fun voiceStreamState(): VoiceStreamState? = latestVoiceStreamState

    fun voiceStreamFailure(): String? = latestVoiceStreamFailure

    fun audioRouteState(): AudioRouteSnapshot? = latestAudioRoute

    fun setVoiceStreamObserver(observer: ((VoiceStreamState?, String?) -> Unit)?) {
        voiceStreamObserver = observer
        observer?.invoke(latestVoiceStreamState, latestVoiceStreamFailure)
    }

    fun setStateObserver(observer: ((RuntimeState) -> Unit)?) {
        runtimeStateObserver = observer
        observer?.invoke(state())
    }

    fun assessAssistantRole() {
        assistantRoleController.assess()
    }

    fun assistantRoleRequestIntent(): Intent? = assistantRolePlatform.createUserRequestIntent()

    fun completeAssistantRoleRequest() {
        assistantRoleController.completeRequest()
    }

    fun enrollmentPublicKeyZ85(): String? = when (val current = enrollment.state()) {
        EnrollmentState.Unenrolled, is EnrollmentState.Corrupt -> null
        is EnrollmentState.AwaitingServerPin -> JeroMqCurveKeyCodec.encode(current.publicKey)
        is EnrollmentState.Ready -> JeroMqCurveKeyCodec.encode(current.publicKey)
    }

    fun pinnedServerPublicKeyZ85(): String? = enrollment.pinnedServerPublicKeyZ85()

    fun createIdentity(): String {
        val publicKey = enrollment.createIdentityZ85()
        refreshEnrollment()
        return publicKey
    }

    fun pinServer(publicKeyZ85: String) {
        enrollment.pinServerZ85(publicKeyZ85.trim())
        refreshEnrollment()
    }

    fun replaceServerPin(publicKeyZ85: String) {
        state().configuredProfile?.let { profile ->
            stateStore.save(RestorableClientState(profile = profile, selectedConversationId = null))
        }
        enrollment.replaceServerPinZ85(publicKeyZ85.trim())
        controller.serverTrustChanged()
    }

    fun connect(endpoint: String): CompletableFuture<ConnectedTextSession> {
        val profile = ServerProfile.create(endpoint.trim())
        stateStore.save(
            RestorableClientState(
                profile = profile,
                selectedConversationId = state().selectedConversationId,
            )
        )
        return controller.connect(profile)
    }

    fun submitText(
        text: String,
        localConversationId: String = "local-device",
        remoteConversationId: String? = null,
    ): CompletableFuture<TextTurnResult> {
        val remoteConnected = state().server is ServerConnection.Connected
        diagnostics.record(
            "text.submit",
            mapOf(
                "length" to text.length,
                "mode" to runtimeMode.name.lowercase(),
                "remote_connected" to remoteConnected,
                "local_server_phase" to localServer.state().phase.name.lowercase(),
            ),
        )
        when (runtimeMode) {
            RuntimeMode.Local -> return submitLocalText(text, localConversationId)
            RuntimeMode.Remote -> {
                if (!remoteConnected) {
                    return CompletableFuture.failedFuture(
                        IllegalStateException("Remote mode requires an authenticated Zara server"),
                    )
                }
                return submitRemoteText(text, remoteConversationId)
            }
            RuntimeMode.Auto -> return submitAutoRemoteFirst(
                text = text,
                remoteConnected = remoteConnected,
                localConversationId = localConversationId,
                remoteConversationId = remoteConversationId,
            )
        }
    }

    private fun submitAutoRemoteFirst(
        text: String,
        remoteConnected: Boolean,
        localConversationId: String = "local-device",
        remoteConversationId: String? = null,
    ): CompletableFuture<TextTurnResult> {
        val query = text.trim()
        val explicitSymbolic =
            query.startsWith("?-") || query.startsWith("/prolog ") || query.startsWith("/expert ")
        if (explicitSymbolic) {
            diagnostics.record(
                "auto.local_symbolic",
                mapOf("remote_connected" to remoteConnected),
            )
            return submitLocalText(text, localConversationId)
        }

        if (remoteConnected) {
            diagnostics.record(
                "auto.remote_preferred",
                mapOf("remote_connected" to true),
            )
            return submitRemoteText(text, remoteConversationId)
        }

        diagnostics.record(
            "auto.local_fallback",
            mapOf("remote_connected" to false),
        )
        return submitLocalText(text, localConversationId)
    }

    private fun submitRemoteText(
        text: String,
        conversationId: String? = null,
    ): CompletableFuture<TextTurnResult> {
        val future = controller.submitText(text, conversationId)
        future.thenAccept { result ->
            val profile = state().configuredProfile ?: return@thenAccept
            stateStore.save(
                RestorableClientState(
                    profile = profile,
                    selectedConversationId = result.conversationId ?: state().selectedConversationId,
                )
            )
        }
        return future
    }

    fun submitProjectText(
        text: String,
        projectId: String,
        conversationId: String?,
        localConversationId: String = "local-project:$projectId",
    ): CompletableFuture<TextTurnResult> {
        val normalizedProjectId = projectId.trim()
        require(normalizedProjectId.isNotEmpty()) { "Project id is required" }
        require(normalizedProjectId.length <= 128) { "Project id is too long" }
        require(normalizedProjectId.none(Char::isISOControl)) { "Project id contains control characters" }
        val remoteConnected = state().server is ServerConnection.Connected
        return when (runtimeMode) {
            RuntimeMode.Local -> submitLocalText(text, localConversationId)
            RuntimeMode.Remote -> {
                if (!remoteConnected) {
                    CompletableFuture.failedFuture(
                        IllegalStateException("Remote mode requires an authenticated Zara server"),
                    )
                } else {
                    submitRemoteText(text, conversationId)
                }
            }
            RuntimeMode.Auto -> submitAutoRemoteFirst(
                text = text,
                remoteConnected = remoteConnected,
                localConversationId = localConversationId,
                remoteConversationId = conversationId,
            )
        }
    }

    internal fun submitLocalText(
        text: String,
        conversationId: String = "local-device",
    ): CompletableFuture<TextTurnResult> {
        val query = text.trim()
        val catalog = PrologWorkspaceCatalog.from(prologWorkspace.listSources())
        val explicitSymbolic =
            query.startsWith("?-") || query.startsWith("/prolog ") || query.startsWith("/expert ")
        val route: String
        val future = when {
            query.startsWith("?-") -> {
                route = "explicit_query"
                localServer.query(query)
            }
            query.startsWith("/prolog ") || query.startsWith("/expert ") -> {
                route = "explicit_command"
                val command = try {
                    LocalPrologCommand.parse(query, catalog)
                } catch (error: Throwable) {
                    diagnostics.record(
                        "local_symbolic.command_parse.failed",
                        mapOf("length" to query.length),
                        error,
                    )
                    return CompletableFuture.failedFuture(error)
                }
                localServer.query(command.query)
            }
            else -> {
                val expertQuery = LocalNaturalLanguageExpertRouter.query(query, catalog)
                if (expertQuery != null) {
                    route = "expert_router"
                    localServer.query(expertQuery)
                } else {
                    route = "frame_resolver"
                    localServer.resolve(query)
                }
            }
        }

        diagnostics.record(
            "local_submit.route",
            mapOf(
                "route" to route,
                "explicit_symbolic" to explicitSymbolic,
                "length" to query.length,
                "local_server_phase" to localServer.state().phase.name.lowercase(),
            ),
        )

        if (explicitSymbolic) {
            return future.thenApply { result ->
                diagnostics.record(
                    "local_symbolic.complete",
                    mapOf("route" to route, "terms" to result.terms.size),
                )
                localPrologTurn(result, conversationId)
            }
        }

        return recoverLocalNaturalLanguageTurn(
            symbolic = future,
            onMatch = { result ->
                diagnostics.record(
                    "local_symbolic.matched",
                    mapOf("route" to route, "terms" to result.terms.size),
                )
                localPrologTurn(result, conversationId)
            },
            onFallback = { error ->
                if (error != null) {
                    diagnostics.record(
                        "local_symbolic.failed_fallback",
                        mapOf(
                            "route" to route,
                            "local_server_phase" to localServer.state().phase.name.lowercase(),
                        ),
                        error,
                    )
                } else {
                    diagnostics.record(
                        "local_symbolic.no_match",
                        mapOf("route" to route),
                    )
                }
                generateLocalModelTurn(
                    query = query,
                    symbolicFailure = error,
                    conversationId = conversationId,
                )
            },
        )
    }

    private fun generateLocalModelTurn(
        query: String,
        symbolicFailure: Throwable?,
        conversationId: String,
    ): CompletableFuture<TextTurnResult> {
        diagnostics.record(
            "local_model.generate.begin",
            mapOf(
                "prompt_length" to query.length,
                "symbolic_failure" to (symbolicFailure != null),
                "local_server_phase" to localServer.state().phase.name.lowercase(),
            ),
        )
        return localAi.generate(
            LocalGenerationRequest(query, maxOutputTokens = 256),
        ).handle { generated, error ->
            if (error != null || generated == null) {
                val failure = error ?: IllegalStateException("Local model returned no generation result")
                diagnostics.record(
                    "local_model.generate.failed",
                    mapOf(
                        "symbolic_failure" to (symbolicFailure != null),
                        "local_server_phase" to localServer.state().phase.name.lowercase(),
                    ),
                    failure,
                )
                TextTurnResult(
                    conversationId = conversationId,
                    turnId = UUID.randomUUID().toString(),
                    text = if (symbolicFailure != null) {
                        "The local symbolic runtime failed and no verified local model completed this turn. Diagnostics captured the failure; open Diagnostics and tap Copy diagnostics."
                    } else {
                        "No deterministic local rule matched and no verified local model is ready. Open Diagnostics to inspect or share the local runtime log."
                    },
                    success = false,
                )
            } else {
                val answer = generated.text.trim()
                diagnostics.record(
                    "local_model.generate.complete",
                    mapOf(
                        "model" to "${generated.modelId}@${generated.modelVersion}",
                        "quantization" to generated.quantization.wireName,
                        "generation" to generated.generation,
                        "output_length" to answer.length,
                    ),
                )
                TextTurnResult(
                    conversationId = conversationId,
                    turnId = UUID.randomUUID().toString(),
                    text = answer.ifEmpty { "The local model returned no text." },
                    success = answer.isNotEmpty(),
                )
            }
        }
    }

    private fun localPrologTurn(
        result: LocalQueryResult,
        conversationId: String,
    ): TextTurnResult = TextTurnResult(
        conversationId = conversationId,
        turnId = UUID.randomUUID().toString(),
        text = if (result.terms.isEmpty()) {
            "No deterministic local rule matched."
        } else {
            result.terms.joinToString("\n")
        },
        success = result.terms.isNotEmpty(),
    )

    fun pressToTalk(permissionGranted: Boolean): CompletableFuture<Unit> =
        pressVoice(AssistantVoiceOwnership.Manual, permissionGranted)

    internal fun pressAssistantToTalk(permissionGranted: Boolean): CompletableFuture<Unit> =
        pressVoice(AssistantVoiceOwnership.Assistant, permissionGranted)

    fun releasePushToTalk(): CompletableFuture<Unit> =
        submitVoiceControl {
            try {
                voice.release()
            } finally {
                assistantVoiceGuard.onCaptureStopped()
            }
        }

    fun cancelPushToTalk(): CompletableFuture<Unit> =
        submitVoiceControl {
            try {
                voice.cancel()
            } finally {
                assistantVoiceGuard.onCaptureStopped()
            }
        }

    fun onMicrophonePermissionChanged(granted: Boolean): CompletableFuture<Unit> =
        submitVoiceControl {
            voice.onMicrophonePermissionChanged(granted)
            clearVoiceOwnershipIfIdle()
        }

    fun onHostStopped(): CompletableFuture<Unit> =
        submitVoiceControl {
            voice.onHostStopped()
            clearVoiceOwnershipIfIdle()
        }

    private fun pressVoice(
        ownership: AssistantVoiceOwnership,
        permissionGranted: Boolean,
    ): CompletableFuture<Unit> =
        submitVoiceControl {
            localAi.stopSpeech()
            voiceStreamSink.interrupt().get()
            voice.press(state(), permissionGranted)
            assistantVoiceGuard.onCaptureStarted(ownership)
            if (ownership == AssistantVoiceOwnership.Assistant) {
                val role = state().assistantRole
                assistantVoiceGuard.onRoleChanged(role)
                check(role is AssistantRole.Held) {
                    "Android Assistant role was lost during voice startup"
                }
            }
        }

    private fun refreshEnrollment() {
        controller.observeEnrollment(enrollment.state().toRuntimeReadiness())
    }

    private fun observeRuntimeState(state: RuntimeState) {
        val authenticatedSessionId = state.sessionId.takeIf { state.server is ServerConnection.Connected }
        val staleSessionId = playbackRuntimeSessionId
        if (authenticatedSessionId != staleSessionId) {
            playbackRuntimeSessionId = authenticatedSessionId
            if (staleSessionId != null) {
                try {
                    voiceStreamSink.reset()
                } catch (error: Throwable) {
                    reportVoiceStreamFailure(error)
                }
            }
        }
        try {
            voice.onRuntimeStateChanged(state)
        } catch (error: Throwable) {
            reportVoiceStreamFailure(error)
        }
        runtimeStateObserver?.invoke(state)
    }

    private fun clearVoiceOwnershipIfIdle() {
        if (voice.state() is ManualVoiceState.Idle) {
            assistantVoiceGuard.onCaptureStopped()
        }
    }

    private fun cancelAssistantCaptureForRoleLoss() {
        val future = try {
            submitVoiceControl { voice.cancel() }
        } catch (error: Throwable) {
            reportVoiceStreamFailure(error)
            return
        }
        future.whenComplete { _, error ->
            if (error != null) reportVoiceStreamFailure(error)
        }
    }

    private fun interruptVoicePlayback() {
        localAi.stopSpeech()
        try {
            voiceStreamSink.interrupt()
        } catch (error: Throwable) {
            reportVoiceStreamFailure(error)
        }
    }

    private fun reportVoiceStreamFailure(error: Throwable) {
        val message = VoiceDiagnosticFailure.summarize(error)
        latestVoiceStreamFailure = message
        voiceStreamObserver?.invoke(latestVoiceStreamState, message)
    }

    private fun submitVoiceControl(block: () -> Unit): CompletableFuture<Unit> {
        val future = CompletableFuture<Unit>()
        voiceExecutor.execute {
            try {
                block()
                future.complete(Unit)
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        return future
    }

    override fun close() {
        diagnostics.record("session.close", emptyMap())
        controller.setStateObserver(null)
        runtimeStateObserver = null
        actor.setVoiceStreamObserver(null)
        actor.setVoiceStreamFailureObserver(null)
        localServer.setStateObserver(null)
        assistantVoiceGuard.onCaptureStopped()
        localAi.stopSpeech()
        val routeFailure = runCatching { audioRouteController.stop() }.exceptionOrNull()
        if (routeFailure != null) reportVoiceStreamFailure(routeFailure)
        try {
            voice.close()
        } finally {
            voiceExecutor.shutdownNow()
            try {
                controller.close()
            } finally {
                voiceStreamSink.close()
            }
        }
        localServer.close()
        localAi.close()
        if (routeFailure != null) throw routeFailure
    }
}

internal fun recoverLocalNaturalLanguageTurn(
    symbolic: CompletableFuture<LocalQueryResult>,
    onMatch: (LocalQueryResult) -> TextTurnResult,
    onFallback: (Throwable?) -> CompletableFuture<TextTurnResult>,
): CompletableFuture<TextTurnResult> =
    symbolic.handle { result, error -> result to error }.thenCompose { (result, error) ->
        when {
            error != null -> onFallback(error)
            result != null && result.terms.isNotEmpty() ->
                CompletableFuture.completedFuture(onMatch(result))
            else -> onFallback(null)
        }
    }
