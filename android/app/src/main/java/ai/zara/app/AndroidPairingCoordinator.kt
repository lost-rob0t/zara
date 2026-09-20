package ai.zara.app

import ai.zara.app.auth.AndroidEnrollmentRepository
import ai.zara.app.auth.PairingClient
import ai.zara.app.auth.PairingException
import ai.zara.app.auth.PairingPayload
import ai.zara.app.auth.PairingProgress
import ai.zara.app.runtime.ConnectedTextSession
import android.content.Context
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class AndroidPairingCoordinator private constructor(
    private val pairingClient: PairingClient,
    private val pinServer: (String) -> Unit,
    private val connect: (String) -> CompletableFuture<ConnectedTextSession>,
    private val executor: ExecutorService,
) : AutoCloseable {
    private val lifecycleLock = Any()
    private var generation = 0L
    private var closed = false
    private var activeFuture: CompletableFuture<ConnectedTextSession>? = null

    constructor(
        context: Context,
        appSession: AndroidAppSession,
    ) : this(
        pairingClient = PairingClient(AndroidEnrollmentRepository.create(context.applicationContext)),
        pinServer = appSession::pinServer,
        connect = appSession::connect,
        executor = newPairingExecutor(),
    )

    internal constructor(
        pairingClient: PairingClient,
        pinServer: (String) -> Unit,
        connect: (String) -> CompletableFuture<ConnectedTextSession>,
    ) : this(
        pairingClient = pairingClient,
        pinServer = pinServer,
        connect = connect,
        executor = newPairingExecutor(),
    )

    fun pair(
        rawPayload: String,
        onProgress: (PairingProgress) -> Unit,
    ): CompletableFuture<ConnectedTextSession> {
        val future = CompletableFuture<ConnectedTextSession>()
        val pairingGeneration: Long
        val previousFuture: CompletableFuture<ConnectedTextSession>?
        synchronized(lifecycleLock) {
            if (closed) {
                future.completeExceptionally(PairingException("pairing coordinator is closed"))
                return future
            }
            generation += 1
            pairingGeneration = generation
            previousFuture = activeFuture
            activeFuture = future
        }

        val clientGeneration = try {
            pairingClient.reservePairing()
        } catch (error: Throwable) {
            previousFuture?.cancel(true)
            synchronized(lifecycleLock) {
                if (isCurrentLocked(pairingGeneration, future)) {
                    activeFuture = null
                    future.completeExceptionally(error)
                }
            }
            return future
        }
        previousFuture?.cancel(true)

        try {
            executor.execute {
                try {
                    synchronized(lifecycleLock) {
                        requireCurrentLocked(pairingGeneration, future)
                    }
                    val payload = PairingPayload.parse(rawPayload)
                    val outcome = pairingClient.pairReserved(
                        pairingGeneration = clientGeneration,
                        rawPayload = rawPayload,
                    ) { progress ->
                        synchronized(lifecycleLock) {
                            if (isCurrentLocked(pairingGeneration, future)) {
                                onProgress(progress)
                            }
                        }
                    }
                    val connection = synchronized(lifecycleLock) {
                        requireCurrentLocked(pairingGeneration, future)
                        pinServer(payload.serverKey)
                        requireCurrentLocked(pairingGeneration, future)
                        connect(outcome.endpoint)
                    }
                    connection.whenComplete { session, error ->
                        synchronized(lifecycleLock) {
                            if (!isCurrentLocked(pairingGeneration, future)) {
                                return@whenComplete
                            }
                            activeFuture = null
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
                    }
                } catch (error: Throwable) {
                    synchronized(lifecycleLock) {
                        if (isCurrentLocked(pairingGeneration, future)) {
                            activeFuture = null
                            future.completeExceptionally(error)
                        }
                    }
                }
            }
        } catch (error: Throwable) {
            synchronized(lifecycleLock) {
                if (isCurrentLocked(pairingGeneration, future)) {
                    activeFuture = null
                    future.completeExceptionally(error)
                }
            }
        }
        return future
    }

    override fun close() {
        val future = synchronized(lifecycleLock) {
            if (closed) return
            closed = true
            generation += 1
            activeFuture.also { activeFuture = null }
        }
        pairingClient.close()
        future?.cancel(true)
        executor.shutdownNow()
    }

    private fun isCurrentLocked(
        pairingGeneration: Long,
        future: CompletableFuture<ConnectedTextSession>,
    ): Boolean =
        !closed && generation == pairingGeneration && activeFuture === future && !future.isDone

    private fun requireCurrentLocked(
        pairingGeneration: Long,
        future: CompletableFuture<ConnectedTextSession>,
    ) {
        if (!isCurrentLocked(pairingGeneration, future)) {
            throw PairingException("pairing was cancelled")
        }
    }

    companion object {
        private fun newPairingExecutor(): ExecutorService =
            Executors.newSingleThreadExecutor { runnable ->
                Thread(runnable, "zara-android-pairing").apply { isDaemon = true }
            }
    }
}
