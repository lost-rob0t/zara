package ai.zara.app.peer

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Test
import org.zeromq.ZCert

class PeerNodeServiceControllerTest {
    private val serverCertificate = ZCert()
    private val identity = PeerNodeIdentity(
        nodeId = "phone-01",
        curvePublicKeyZ85 = serverCertificate.publicKeyAsZ85,
        enrollmentGeneration = 5,
    )

    private fun newController(
        lifecycle: PeerNodeListenerLifecycle = PeerNodeListenerLifecycle(identity),
        registry: PeerEnrollmentRegistry = PeerEnrollmentRegistry(),
        identityFactory: () -> PeerNodeIdentity? = { identity },
        events: MutableList<String>,
    ): PeerNodeServiceController = PeerNodeServiceController(
        lifecycle = lifecycle,
        registry = registry,
        identityFactory = identityFactory,
        gatewayFactory = { listenerLifecycle, listenerRegistry, nodeIdentity ->
            events += "gateway-created"
            PeerNodeGateway(
                lifecycle = listenerLifecycle,
                registry = listenerRegistry,
                serverSecretKeyZ85 = serverCertificate.secretKeyAsZ85,
                bindEndpoint = "tcp://127.0.0.1:0",
            ).also { assertEquals(identity, nodeIdentity) }
        },
        enterForeground = { events += "foreground" },
    )

    @Test
    fun foregroundRunsBeforeGatewayCreationAndStartIsTruthful() {
        val events = mutableListOf<String>()
        val controller = newController(events = events)

        val snapshot = controller.startListener()

        assertEquals(PeerListenerPhase.ACTIVE, snapshot.phase)
        assertEquals(listOf("foreground", "gateway-created"), events)
        assertEquals(identity, snapshot.identity)
        org.junit.Assert.assertTrue(snapshot.endpoints.isNotEmpty())

        controller.stopListener()
        assertEquals(PeerListenerPhase.STOPPED, controller.snapshot().phase)
    }

    @Test
    fun missingIdentityRefusesToStartTruthfully() {
        val events = mutableListOf<String>()
        val controller = newController(
            lifecycle = PeerNodeListenerLifecycle(),
            identityFactory = { null },
            events = events,
        )

        val snapshot = controller.startListener()

        assertEquals(PeerListenerPhase.STOPPED, snapshot.phase)
        assertNull(snapshot.identity)
        assertEquals(emptyList<String>(), events)

        controller.stopListener()
        assertEquals(PeerListenerPhase.STOPPED, controller.snapshot().phase)
    }

    @Test
    fun processRecreationRestoresIdentityButStartsStopped() {
        val restoredLifecycle = PeerNodeListenerLifecycle(identity)
        val firstEvents = mutableListOf<String>()
        val first = newController(lifecycle = restoredLifecycle, events = firstEvents)
        first.startListener()
        first.stopListener()

        val recreatedEvents = mutableListOf<String>()
        val recreated = newController(lifecycle = restoredLifecycle, events = recreatedEvents)

        val snapshot = recreated.snapshot()

        assertEquals(PeerListenerPhase.STOPPED, snapshot.phase)
        assertEquals(identity, snapshot.identity)
        assertEquals(emptyList<String>(), recreatedEvents)
    }

    @Test
    fun bindFailureLeavesAFailedTruthfulSnapshotWithoutForegroundPromisesLater() {
        val events = mutableListOf<String>()
        val controller = PeerNodeServiceController(
            lifecycle = PeerNodeListenerLifecycle(identity),
            registry = PeerEnrollmentRegistry(),
            identityFactory = { identity },
            gatewayFactory = { listenerLifecycle, listenerRegistry, _ ->
                events += "gateway-created"
                PeerNodeGateway(
                    lifecycle = listenerLifecycle,
                    registry = listenerRegistry,
                    serverSecretKeyZ85 = serverCertificate.secretKeyAsZ85,
                    bindEndpoint = "tcp://256.0.0.1:1",
                )
            },
            enterForeground = { events += "foreground" },
        )

        val snapshot = controller.startListener()

        assertEquals(PeerListenerPhase.FAILED, snapshot.phase)
        assertNotNull(snapshot.failure)
        controller.stopListener()
        assertEquals(PeerListenerPhase.STOPPED, controller.snapshot().phase)
    }
}
