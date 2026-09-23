package ai.zara.app.runtime

import ai.zara.app.auth.EnrollmentRepository
import ai.zara.app.auth.JeroMqCurveDealerFactory
import ai.zara.app.telemetry.ZaraFailureCodes
import org.zeromq.ZContext
import org.zeromq.ZMQ

class JeroMqTextDealerFactory(
    private val enrollment: EnrollmentRepository,
) : TextDealerFactory {
    override fun create(endpoint: String): TextDealer {
        val context = ZContext()
        try {
            val socket = JeroMqCurveDealerFactory(enrollment).create(context)
            if (!socket.connect(endpoint)) {
                socket.close()
                throw ZaraWireException(
                    "failed to connect Zara DEALER",
                    code = ZaraFailureCodes.TRANSPORT_CONNECT,
                )
            }
            return JeroMqTextDealer(context, socket)
        } catch (error: Exception) {
            context.close()
            throw error
        }
    }
}

private class JeroMqTextDealer(
    private val context: ZContext,
    private val socket: ZMQ.Socket,
) : TextDealer {
    private var closed = false

    override fun send(frames: List<ByteArray>) {
        check(!closed) { "dealer is closed" }
        require(frames.isNotEmpty()) { "ZARA/1 frames are required" }
        frames.forEachIndexed { index, frame ->
            val flags = if (index == frames.lastIndex) 0 else ZMQ.SNDMORE
            try {
                if (!socket.send(frame, flags)) {
                    throw ZaraWireException(
                        "failed to send ZARA/1 frame",
                        code = ZaraFailureCodes.TRANSPORT_CLOSED,
                    )
                }
            } catch (error: org.zeromq.ZMQException) {
                throw transportClosed(error)
            }
        }
    }

    override fun receive(timeoutMillis: Int): List<ByteArray>? {
        check(!closed) { "dealer is closed" }
        require(timeoutMillis > 0) { "receive timeout must be positive" }
        socket.receiveTimeOut = timeoutMillis
        val first = try {
            socket.recv(0) ?: return null
        } catch (error: org.zeromq.ZMQException) {
            throw transportClosed(error)
        }
        val frames = mutableListOf(first)
        while (socket.hasReceiveMore()) {
            frames += try {
                socket.recv(0)
            } catch (error: org.zeromq.ZMQException) {
                throw transportClosed(error)
            } ?: throw ZaraWireException(
                "truncated ZARA/1 multipart",
                code = ZaraFailureCodes.PROTOCOL_MALFORMED,
            )
            if (frames.size > 18) {
                throw ZaraWireException(
                    "ZARA/1 multipart exceeds frame limit",
                    code = ZaraFailureCodes.PROTOCOL_MALFORMED,
                )
            }
        }
        return frames
    }

    override fun close() {
        if (closed) return
        closed = true
        socket.close()
        context.close()
    }

    private fun transportClosed(cause: org.zeromq.ZMQException): ZaraWireException = ZaraWireException(
        "ZARA/1 transport failed: ${cause.errorCode}",
        cause = cause,
        code = ZaraFailureCodes.TRANSPORT_CLOSED,
    )
}
