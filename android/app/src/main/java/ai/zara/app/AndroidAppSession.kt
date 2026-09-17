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
import ai.zara.app.prolog.PrologExampleCatalog
import ai.zara.app.prolog.PrologSource
import ai.zara.app.prolog.PrologSourceAnalyzer
import ai.zara.app.prolog.PrologWorkspace
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
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.UUID

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
    private val localServer: LocalZaraServer
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
        prologWorkspace.seedExamples(PrologExampleCatalog.examples)
        val stagedSemanticAssets = PortableSemanticAssetStager(
            File(context.noBackupFilesDir, "zara/prolog-runtime"),
        ).stageAll(AndroidPortableSemanticAssetSource(context.assets))
        localServer = LocalZaraServer(
            bridge = NativeTreallaBridge(),
            corePath = stagedSemanticAssets.coreFile.absolutePath,
            workspace = prologWorkspace,
        )
        localServer.start()
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
        runtimeMode = mode
    }

    fun localServerState(): LocalServerState = localServer.state()

    fun setLocalServerObserver(observer: ((LocalServerState) -> Unit)?) {
        localServer.setStateObserver(observer)
    }

    fun prologSources(): List<PrologSource> = prologWorkspace.listSources()

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

    fun submitText(text: String): CompletableFuture<TextTurnResult> {
        val remoteConnected = state().server is ServerConnection.Connected
        when (runtimeMode) {
            RuntimeMode.Local -> return submitLocalText(text)
            RuntimeMode.Remote -> if (!remoteConnected) {
                return CompletableFuture.failedFuture(
                    IllegalStateException("Remote mode requires an authenticated Zara server"),
                )
            }
            RuntimeMode.Auto -> if (!remoteConnected) return submitLocalText(text)
        }
        val future = controller.submitText(text)
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

    private fun submitLocalText(text: String): CompletableFuture<TextTurnResult> {
        val query = text.trim()
        val future = if (query.startsWith("?-")) {
            localServer.query(query)
        } else {
            localServer.resolve(query)
        }
        return future.thenApply { result ->
            val answer = if (result.terms.isEmpty()) {
                "No deterministic local rule matched. Remote model capabilities are optional; the local Prolog server is still ready."
            } else {
                result.terms.joinToString("\n")
            }
            TextTurnResult(
                conversationId = "local-device",
                turnId = UUID.randomUUID().toString(),
                text = answer,
                success = result.terms.isNotEmpty(),
            )
        }
    }

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
        controller.setStateObserver(null)
        runtimeStateObserver = null
        actor.setVoiceStreamObserver(null)
        actor.setVoiceStreamFailureObserver(null)
        localServer.setStateObserver(null)
        assistantVoiceGuard.onCaptureStopped()
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
        if (routeFailure != null) throw routeFailure
    }
}
