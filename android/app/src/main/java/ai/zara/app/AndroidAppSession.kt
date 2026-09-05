package ai.zara.app

import ai.zara.app.auth.AndroidEnrollmentRepository
import ai.zara.app.auth.EnrollmentRepository
import ai.zara.app.auth.EnrollmentState
import ai.zara.app.auth.JeroMqCurveKeyCodec
import ai.zara.app.runtime.AndroidTextSessionController
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
import android.content.Context
import java.io.File
import java.util.concurrent.CompletableFuture

class AndroidAppSession(context: Context) : AutoCloseable {
    private val enrollment: EnrollmentRepository = AndroidEnrollmentRepository.create(context)
    private val stateStore = ClientStateStore(File(context.noBackupFilesDir, "zara/client-state.bin"))
    private val controller: AndroidTextSessionController

    init {
        val restored = stateStore.load()
        var initial = restored?.let(RuntimeState::fromRestored) ?: RuntimeState.initial()
        initial = reduce(
            initial,
            RuntimeEvent.EnrollmentObserved(enrollment.state().toRuntimeReadiness()),
        )
        val actor = ZaraTextClientActor(JeroMqTextDealerFactory(enrollment))
        controller = AndroidTextSessionController(initial, actor)
    }

    fun state(): RuntimeState = controller.state()

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

    private fun refreshEnrollment() {
        controller.observeEnrollment(enrollment.state().toRuntimeReadiness())
    }

    override fun close() {
        controller.close()
    }
}
