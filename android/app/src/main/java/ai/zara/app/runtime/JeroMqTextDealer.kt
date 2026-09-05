package ai.zara.app.runtime

import ai.zara.app.auth.EnrollmentRepository
import ai.zara.app.auth.JeroMqCurveDealerFactory
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
                throw ZaraWireException("failed to connect Zara DEALER")
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
            if (!socket.send(frame, flags)) throw ZaraWireException("failed to send ZARA/1 frame")
        }
    }

    override fun receive(timeoutMillis: Int): List<ByteArray>? {
        check(!closed) { "dealer is closed" }
        require(timeoutMillis > 0) { "receive timeout must be positive" }
        socket.receiveTimeOut = timeoutMillis
        val first = socket.recv(0) ?: return null
        val frames = mutableListOf(first)
        while (socket.hasReceiveMore()) {
            frames += socket.recv(0) ?: throw ZaraWireException("truncated ZARA/1 multipart")
            if (frames.size > 18) throw ZaraWireException("ZARA/1 multipart exceeds frame limit")
        }
        return frames
    }

    override fun close() {
        if (closed) return
        closed = true
        socket.close()
        context.close()
    }
}
