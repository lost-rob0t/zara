package ai.zara.app.peer

import ai.zara.app.R
import ai.zara.app.auth.AndroidEnrollmentRepository
import android.app.Notification
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.Service
import android.content.Context
import android.content.Intent
import android.content.pm.ServiceInfo
import android.os.Binder
import android.os.IBinder
import java.io.File

/**
 * Foreground service hosting the authenticated ZARA/1 peer listener. The
 * durable node identity comes from the Keystore-wrapped CURVE credential plus
 * the peer node identity store, so restarts preserve identity while the
 * listener itself always starts truthfully stopped. Peer enrollment is owned
 * by the in-process registry; the pairing ceremony wires its enrollment path
 * in a later slice. No wake lock, no discovery, no second transport.
 */
class PeerNodeListenerService : Service() {
    private var controller: PeerNodeServiceController? = null
    private val peerRegistry = PeerEnrollmentRegistry()

    override fun onCreate() {
        super.onCreate()
        val appContext = applicationContext
        val enrollment = AndroidEnrollmentRepository.create(appContext)
        val identityStore = PeerNodeIdentityStore(
            File(appContext.noBackupFilesDir, "zara/peer/peer-node.bin"),
        )
        controller = PeerNodeServiceController(
            lifecycle = PeerNodeListenerLifecycle(),
            registry = peerRegistry,
            identityFactory = {
                enrollment.serverCurveKeysZ85()?.let { keys ->
                    val stored = identityStore.ensure()
                    PeerNodeIdentity(
                        nodeId = stored.nodeId,
                        curvePublicKeyZ85 = keys.publicKeyZ85,
                        enrollmentGeneration = stored.enrollmentGeneration,
                    )
                }
            },
            gatewayFactory = { serviceLifecycle, registry, _ ->
                val keys = enrollment.serverCurveKeysZ85()
                PeerNodeGateway(
                    lifecycle = serviceLifecycle,
                    registry = registry,
                    serverSecretKeyZ85 = requireNotNull(keys) {
                        "peer listener requires the node CURVE identity"
                    }.secretKeyZ85,
                    bindEndpoint = DEFAULT_BIND_ENDPOINT,
                )
            },
            enterForeground = { enterForeground() },
        )
    }

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        when (intent?.action) {
            ACTION_START -> controller?.startListener()
            ACTION_STOP -> {
                controller?.stopListener()
                stopSelf()
            }
            else -> if (intent == null) {
                controller?.stopListener()
                stopSelf()
            }
        }
        return START_NOT_STICKY
    }

    override fun onDestroy() {
        controller?.stopListener()
        controller = null
        super.onDestroy()
    }

    override fun onBind(intent: Intent?): IBinder = PeerNodeListenerBinder()

    private fun enterForeground() {
        val manager = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        manager.createNotificationChannel(
            NotificationChannel(
                CHANNEL_ID,
                getString(R.string.peer_listener_channel_name),
                NotificationManager.IMPORTANCE_LOW,
            ),
        )
        val notification = Notification.Builder(this, CHANNEL_ID)
            .setSmallIcon(R.drawable.ic_zara_foreground)
            .setContentTitle(getString(R.string.peer_listener_notification_title))
            .setContentText(getString(R.string.peer_listener_notification_text))
            .setOngoing(true)
            .build()
        startForeground(NOTIFICATION_ID, notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_CONNECTED_DEVICE)
    }

    inner class PeerNodeListenerBinder : Binder() {
        fun snapshot(): PeerListenerSnapshot = requireNotNull(controller).snapshot()

        fun registry(): PeerEnrollmentRegistry = peerRegistry
    }

    companion object {
        const val ACTION_START = "ai.zara.app.peer.action.START_LISTENER"
        const val ACTION_STOP = "ai.zara.app.peer.action.STOP_LISTENER"
        const val DEFAULT_BIND_ENDPOINT = "tcp://0.0.0.0:17865"
        private const val CHANNEL_ID = "zara-peer-listener"
        private const val NOTIFICATION_ID = 421
    }
}
