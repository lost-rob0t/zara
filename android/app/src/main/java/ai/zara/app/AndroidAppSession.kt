package ai.zara.app

import ai.zara.app.assistant.AndroidAssistantRolePlatform
import ai.zara.app.assistant.AssistantRoleController
import ai.zara.app.auth.AndroidEnrollmentRepository
import ai.zara.app.auth.EnrollmentRepository
import ai.zara.app.auth.EnrollmentState
import ai.zara.app.auth.JeroMqCurveKeyCodec
import ai.zara.app.device.AndroidUriLauncher
import ai.zara.app.device.DeviceCapabilityRegistry
import ai.zara.app.device.OpenUriAdapter
import ai.zara.app.device.RegistryDeviceActionHandler
import ai.zara.app.runtime.AndroidTextSessionController
import ai.zara.app.runtime.AudioOutputFormat
import ai.zara.app.runtime.ClientStateStore
import ai.zara.app.runtime.ConnectedTextSession
import ai.zara.app.runtime.JeroMqTextDealerFactory
import ai.zara.app.runtime.RestorableClientState
import ai.zara.app.runtime.RuntimeEvent
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerProfile
import ai.zara.app.runtime.TextTurnResult
import ai.zara.app.runtime.ZaraTextClientActor
import ai.zara.app.runtime.reduce
import ai.zara.app.runtime.toRuntimeReadiness
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
import ai.zara.app.voice.VoicePlaybackController
import ai.zara.app.voice.VoiceStreamSinkActor
import ai.zara.app.voice.VoiceStreamState
import android.content.Context
import android.content.Intent
import java.io.File
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
    private val voice: ManualVoiceSessionCoordinator
    private val voiceStreamSink: VoiceStreamSinkActor
    private val audioRouteController: AudioRouteController
    @Volatile private var latestVoiceStreamState: VoiceStreamState? = null
    @Volatile private var latestVoiceStreamFailure: String? = null
    @Volatile private var latestAudioRoute: AudioRouteSnapshot? = null
    @Volatile private var voiceStreamObserver: ((VoiceStreamState?, String?) -> Unit)? = null
    @Volatile private var runtimeStateObserver: ((RuntimeState) -> Unit)? = null
    private val voiceExecutor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-android-voice-control").apply { isDaemon = true }
    }

    init {
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
        assistantRolePlatform = AndroidAssistantRolePlatform(context.applicationContext)
        assistantRoleController = AssistantRoleController(
            platform = assistantRolePlatform,
            outcomeObserver = controller::observeAssistantRole,
        )
        voice = ManualVoiceSessionCoordinator(
            PushToTalkController(
                capture = ManualVoiceCapture(AuthenticatedVoiceIngress(actor)),
                recorder = AndroidPcmRecorder(),
            )
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

    fun createIdentity(): String {
        val publicKey = enrollment.createIdentityZ85()
        refreshEnrollment()
        return publicKey
    }

    fun pinServer(publicKeyZ85: String) {
        enrollment.pinServerZ85(publicKeyZ85.trim())
        refreshEnrollment()
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

    fun pressToTalk(permissionGranted: Boolean): CompletableFuture<Unit> =
        submitVoiceControl {
            voiceStreamSink.interrupt().get()
            voice.press(state(), permissionGranted)
        }

    fun releasePushToTalk(): CompletableFuture<Unit> =
        submitVoiceControl {
            voice.release()
        }

    fun cancelPushToTalk(): CompletableFuture<Unit> =
        submitVoiceControl {
            voice.cancel()
        }

    fun onMicrophonePermissionChanged(granted: Boolean): CompletableFuture<Unit> =
        submitVoiceControl {
            voice.onMicrophonePermissionChanged(granted)
        }

    fun onHostStopped(): CompletableFuture<Unit> =
        submitVoiceControl {
            voice.onHostStopped()
        }

    private fun refreshEnrollment() {
        controller.observeEnrollment(enrollment.state().toRuntimeReadiness())
    }

    private fun observeRuntimeState(state: RuntimeState) {
        try {
            voice.onRuntimeStateChanged(state)
        } catch (error: Throwable) {
            reportVoiceStreamFailure(error)
        }
        runtimeStateObserver?.invoke(state)
    }

    private fun interruptVoicePlayback() {
        try {
            voiceStreamSink.interrupt()
        } catch (error: Throwable) {
            reportVoiceStreamFailure(error)
        }
    }

    private fun reportVoiceStreamFailure(error: Throwable) {
        val message = error.message ?: error::class.java.simpleName
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
        if (routeFailure != null) throw routeFailure
    }
}