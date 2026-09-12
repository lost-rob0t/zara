package ai.zara.app

import ai.zara.app.auth.AndroidEnrollmentRepository
import ai.zara.app.auth.PairingClient
import ai.zara.app.auth.PairingPayload
import ai.zara.app.auth.PairingProgress
import ai.zara.app.runtime.ConnectedTextSession
import android.content.Context
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class AndroidPairingCoordinator(
    context: Context,
    private val appSession: AndroidAppSession,
) : AutoCloseable {
    private val pairingClient = PairingClient(AndroidEnrollmentRepository.create(context.applicationContext))
    private val executor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-android-pairing").apply { isDaemon = true }
    }

    fun pair(
        rawPayload: String,
        onProgress: (PairingProgress) -> Unit,
    ): CompletableFuture<ConnectedTextSession> {
        val future = CompletableFuture<ConnectedTextSession>()
        executor.execute {
            try {
                val payload = PairingPayload.parse(rawPayload)
                val outcome = pairingClient.pair(rawPayload, onProgress)
                appSession.pinServer(payload.serverKey)
                appSession.connect(outcome.endpoint).whenComplete { session, error ->
                    if (error != null) {
                        future.completeExceptionally(error)
                    } else if (session != null) {
                        future.complete(session)
                    } else {
                        future.completeExceptionally(
                            IllegalStateException("pairing connected without a Zara session")
                        )
                    }
                }
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        return future
    }

    override fun close() {
        executor.shutdownNow()
    }
}
