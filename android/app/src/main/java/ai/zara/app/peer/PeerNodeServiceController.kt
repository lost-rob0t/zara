package ai.zara.app.peer

/**
 * Pure seam binding the Android peer-listener service to the gateway. The
 * service enters the foreground before any socket work, starts and stops are
 * idempotent, a missing identity refuses to start truthfully, and process
 * recreation restores identity while starting stopped. All listener state
 * changes flow through [PeerNodeListenerLifecycle].
 */
class PeerNodeServiceController(
    private val lifecycle: PeerNodeListenerLifecycle,
    private val registry: PeerEnrollmentRegistry,
    private val identityFactory: () -> PeerNodeIdentity?,
    private val gatewayFactory: (
        lifecycle: PeerNodeListenerLifecycle,
        registry: PeerEnrollmentRegistry,
        identity: PeerNodeIdentity,
    ) -> PeerNodeGateway,
    private val enterForeground: () -> Unit,
) {
    private var gateway: PeerNodeGateway? = null

    @Synchronized
    fun startListener(): PeerListenerSnapshot {
        if (gateway != null) {
            val phase = lifecycle.snapshot().phase
            if (phase == PeerListenerPhase.STARTING || phase == PeerListenerPhase.ACTIVE) {
                return lifecycle.snapshot()
            }
            stopListener()
        }
        val identity = identityFactory() ?: return lifecycle.snapshot()
        enterForeground()
        val candidate = gatewayFactory(lifecycle, registry, identity)
        return try {
            candidate.start()
            gateway = candidate
            lifecycle.snapshot()
        } catch (_: Exception) {
            gateway = candidate
            lifecycle.snapshot()
        }
    }

    @Synchronized
    fun stopListener() {
        gateway?.stop()
        gateway = null
    }

    fun snapshot(): PeerListenerSnapshot = lifecycle.snapshot()
}
