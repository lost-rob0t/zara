package ai.zara.app.peer

import java.util.ArrayDeque
import java.util.UUID
import org.zeromq.SocketType
import org.zeromq.ZAuth
import org.zeromq.ZContext
import org.zeromq.ZMQ
import org.zeromq.ZMsg
import zmq.io.Metadata

private const val ZAP_DOMAIN = "zara"
private const val RECEIVE_TIMEOUT_MILLIS = 100
private const val MAX_WIRE_PARTS = 4
private const val MAX_FAILURE_REASON_CHARS = 256

/**
 * Bounded authenticated ZARA/1 ROUTER listener for the Android peer node.
 *
 * Authentication comes only from CURVE/ZAP: the ZAP layer denies unknown keys
 * before any application reply exists, and every received message is resolved
 * through its ZAP user-id against the live enrollment registry, so revocation
 * fences the next message rather than the next restart. Node documents carried
 * by a hello must match the authenticated enrollment exactly; payload fields
 * never select identity. All lifecycle transitions are routed through
 * [PeerNodeListenerLifecycle] so stale work cannot resurrect a listener.
 */
class PeerNodeGateway(
    private val lifecycle: PeerNodeListenerLifecycle,
    private val registry: PeerEnrollmentRegistry,
    private val serverSecretKeyZ85: String,
    private val bindEndpoint: String,
    private val maxRoutes: Int = 32,
    private val rateMaxMessages: Int = 60,
    private val rateWindowMillis: Long = 1_000,
    private val nanoTime: () -> Long = System::nanoTime,
) {
    private data class RouteState(
        val route: ByteArray,
        val deviceId: String,
        val sessionId: String,
        val node: PeerNodeDocument?,
    )

    private class RegistryCurveAuth(private val registry: PeerEnrollmentRegistry) : ZAuth.Auth {
        override fun configure(msg: ZMsg, verbose: Boolean): Boolean = true

        override fun authorize(request: ZAuth.ZapRequest, verbose: Boolean): Boolean {
            val key = request.clientKey ?: return false
            if (registry.resolve(key) == null) return false
            request.userId = key
            return true
        }
    }

    private var context: ZContext? = null
    private var authenticator: ZAuth? = null
    private var socket: ZMQ.Socket? = null
    private var loop: Thread? = null
    private var running = false
    private var bound: List<String> = emptyList()
    private val routes = HashMap<String, RouteState>()
    private val routesByDevice = HashMap<String, String>()
    private val rateWindows = HashMap<String, ArrayDeque<Long>>()

    @Synchronized
    fun start(): Long {
        check(!running) { "peer listener gateway is already running" }
        val identity = requireNotNull(lifecycle.snapshot().identity) {
            "peer listener requires an enrolled node identity"
        }
        val generation = lifecycle.requestStart(identity)
        val activeContext = ZContext()
        try {
            val activeAuth = ZAuth(
                activeContext,
                "zara-peer-zap",
                mapOf("CURVE" to RegistryCurveAuth(registry)),
            )
            val activeSocket = activeContext.createSocket(SocketType.ROUTER)
            activeSocket.setZapDomain(ZAP_DOMAIN)
            activeSocket.setCurveServer(true)
            activeSocket.setCurveSecretKey(decodeKey(serverSecretKeyZ85))
            activeSocket.setLinger(0)
            activeSocket.setSndHWM(256)
            activeSocket.setRcvHWM(256)
            activeSocket.setHandshakeIvl(5_000)
            if (!activeSocket.bind(bindEndpoint)) {
                throw IllegalStateException("failed to bind peer listener endpoint $bindEndpoint")
            }
            bound = listOf(activeSocket.lastEndpoint)

            context = activeContext
            authenticator = activeAuth
            socket = activeSocket
            running = true
            if (!lifecycle.listenerStarted(generation, bound)) {
                throw IllegalStateException("peer listener generation was fenced during start")
            }
        } catch (error: Exception) {
            running = false
            socket = null
            authenticator?.close()
            authenticator = null
            context?.close()
            context = null
            bound = emptyList()
            lifecycle.listenerFailed(generation, boundedReason(error))
            throw IllegalStateException("peer listener failed to start: ${boundedReason(error)}", error)
        }

        val listener = socket ?: throw IllegalStateException("peer listener socket vanished")
        val thread = Thread({ listen(listener) }, "zara-peer-listener")
        loop = thread
        thread.start()
        return generation
    }

    @Synchronized
    fun stop() {
        val generation = lifecycle.requestStop() ?: return
        running = false
        loop?.join(5_000)
        loop = null
        socket?.close()
        socket = null
        authenticator?.close()
        authenticator = null
        context?.close()
        context = null
        bound = emptyList()
        routes.clear()
        routesByDevice.clear()
        rateWindows.clear()
        lifecycle.listenerStopped(generation)
    }

    fun boundEndpoints(): List<String> = synchronized(this) { bound.toList() }

    fun snapshot(): PeerListenerSnapshot = lifecycle.snapshot()

    fun networkChanged(endpoints: List<String>): Boolean =
        lifecycle.networkChanged(lifecycle.snapshot().generation, endpoints)

    private fun listen(listener: ZMQ.Socket) {
        listener.receiveTimeOut = RECEIVE_TIMEOUT_MILLIS
        while (running) {
            val first = listener.recvMsg(0) ?: continue
            val parts = mutableListOf(first)
            while (listener.hasReceiveMore()) {
                val next = listener.recvMsg(0) ?: break
                parts += next
                if (parts.size > MAX_WIRE_PARTS) break
            }
            try {
                process(listener, parts)
            } catch (_: Exception) {
                continue
            }
        }
    }

    private fun process(listener: ZMQ.Socket, parts: List<zmq.Msg>) {
        val route = parts[0].data()
        val messageParts = parts.drop(1)
        val authenticatedKey = messageParts.firstNotNullOfOrNull { part ->
            part.metadata?.get(Metadata.USER_ID)
        }
        val peer = authenticatedKey?.let(registry::resolve)
        if (peer == null) {
            dropRoute(route)
            reply(
                listener,
                route,
                PeerNodeWire.encodeProtocolError(
                    requestId = null,
                    sessionId = null,
                    code = "authentication_required",
                    message = "peer is not enrolled",
                    retryable = false,
                ),
            )
            return
        }
        if (messageParts.size != 2) {
            reply(
                listener,
                route,
                PeerNodeWire.encodeProtocolError(
                    requestId = null,
                    sessionId = null,
                    code = "invalid_message",
                    message = "invalid ZARA/1 frame layout",
                    retryable = false,
                ),
            )
            return
        }

        val message = try {
            PeerNodeWire.decode(messageParts.map(zmq.Msg::data))
        } catch (error: PeerWireException) {
            reply(
                listener,
                route,
                PeerNodeWire.encodeProtocolError(
                    requestId = null,
                    sessionId = routes[routeKey(route)]?.sessionId,
                    code = error.code,
                    message = error.message ?: "invalid message",
                    retryable = false,
                ),
            )
            return
        }

        when (message) {
            is PeerClientMessage.Hello -> processHello(listener, route, peer, message)
            is PeerClientMessage.Ping -> processBoundRoute(listener, route, message.id) { sessionId ->
                reply(listener, route, PeerNodeWire.encodePong(requestId = message.id, sessionId = sessionId))
            }
            is PeerClientMessage.UnsupportedType -> processBoundRoute(listener, route, message.id) {
                reply(
                    listener,
                    route,
                    PeerNodeWire.encodeProtocolError(
                        requestId = message.id,
                        sessionId = routes[routeKey(route)]?.sessionId,
                        code = "authorization_denied",
                        message = "peer message type is not enabled on this node",
                        retryable = false,
                    ),
                )
            }
        }
    }

    private fun processHello(
        listener: ZMQ.Socket,
        route: ByteArray,
        peer: EnrolledPeer,
        message: PeerClientMessage.Hello,
    ) {
        val node = message.node
        if (node != null &&
            (
                node.curvePublicKeyZ85 != peer.publicKeyZ85 ||
                    node.nodeId != peer.deviceId ||
                    node.enrollmentGeneration != peer.enrollmentGeneration
                )
        ) {
            reply(
                listener,
                route,
                PeerNodeWire.encodeProtocolError(
                    requestId = message.id,
                    sessionId = null,
                    code = "node_authority_mismatch",
                    message = "peer node does not match authenticated identity",
                    retryable = false,
                ),
            )
            return
        }

        val key = routeKey(route)
        val priorRoute = routesByDevice.remove(peer.deviceId)
        if (priorRoute != null && priorRoute != key) {
            routes.remove(priorRoute)
        }
        if (routesByDevice[peer.deviceId] == null && routes.size >= maxRoutes) {
            reply(
                listener,
                route,
                PeerNodeWire.encodeProtocolError(
                    requestId = message.id,
                    sessionId = null,
                    code = "quota_exceeded",
                    message = "peer route table is full",
                    retryable = false,
                ),
            )
            return
        }

        if (!tryChargeRate(peer.deviceId)) {
            reply(
                listener,
                route,
                PeerNodeWire.encodeProtocolError(
                    requestId = message.id,
                    sessionId = routes[key]?.sessionId,
                    code = "quota_exceeded",
                    message = "peer message rate exceeded",
                    retryable = false,
                ),
            )
            return
        }

        val sessionId = UUID.randomUUID().toString()
        routes[key] = RouteState(route.copyOf(), peer.deviceId, sessionId, node)
        routesByDevice[peer.deviceId] = key
        reply(listener, route, PeerNodeWire.encodeHelloOk(requestId = message.id, sessionId = sessionId))
    }

    private fun processBoundRoute(
        listener: ZMQ.Socket,
        route: ByteArray,
        requestId: String,
        respond: (String) -> Unit,
    ) {
        val state = routes[routeKey(route)]
        if (state == null) {
            dropRoute(route)
            reply(
                listener,
                route,
                PeerNodeWire.encodeProtocolError(
                    requestId = requestId,
                    sessionId = null,
                    code = "authentication_required",
                    message = "peer session requires an authenticated hello",
                    retryable = false,
                ),
            )
            return
        }
        if (!tryChargeRate(state.deviceId)) {
            reply(
                listener,
                route,
                PeerNodeWire.encodeProtocolError(
                    requestId = requestId,
                    sessionId = state.sessionId,
                    code = "quota_exceeded",
                    message = "peer message rate exceeded",
                    retryable = false,
                ),
            )
            return
        }
        respond(state.sessionId)
    }

    private fun tryChargeRate(deviceId: String): Boolean {
        val now = nanoTime()
        val window = rateWindows.getOrPut(deviceId) { ArrayDeque() }
        while (window.isNotEmpty() && (now - window.first()) / 1_000_000 >= rateWindowMillis) {
            window.removeFirst()
        }
        if (window.size >= rateMaxMessages) return false
        window.addLast(now)
        return true
    }

    private fun dropRoute(route: ByteArray) {
        val key = routeKey(route)
        val state = routes.remove(key) ?: return
        if (routesByDevice[state.deviceId] == key) {
            routesByDevice.remove(state.deviceId)
        }
    }

    private fun reply(listener: ZMQ.Socket, route: ByteArray, frames: List<ByteArray>) {
        val parts = listOf(route) + frames
        parts.forEachIndexed { index, part ->
            listener.send(part, if (index == parts.lastIndex) 0 else ZMQ.SNDMORE)
        }
    }

    private fun routeKey(route: ByteArray): String =
        route.joinToString("") { byte -> "%02x".format(byte) }

    private fun boundedReason(error: Exception): String {
        val text = (error.message ?: error.javaClass.simpleName).trim()
        val bounded = if (text.length > MAX_FAILURE_REASON_CHARS) text.take(MAX_FAILURE_REASON_CHARS) else text
        return if (bounded.isBlank()) error.javaClass.simpleName else bounded
    }
}

private fun decodeKey(value: String): ByteArray {
    require(value.length == 40) { "CURVE secret key must be one 40-character Z85 value" }
    return ai.zara.app.auth.JeroMqCurveKeyCodec.decode(value)
}
