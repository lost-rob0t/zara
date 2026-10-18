package ai.zara.app.phone

import ai.zara.app.runtime.ZaraWireException

enum class PhoneEventKind(val wireId: String) {
    SmsReceived("sms.received"),
    CallSuspectedSpam("call.suspected_spam"),
}

object ZaraDeviceEventCodec {
    private val marker = "ZARA/1".encodeToByteArray()
    private const val maxTokenBytes = 128
    private const val maxTextBytes = 4_096

    fun encode(
        requestId: String,
        sessionId: String,
        eventId: String,
        kind: PhoneEventKind,
        remote: String,
        text: String = "",
        timestampNs: Long,
    ): List<ByteArray> {
        require(timestampNs >= 0) { "timestamp must be non-negative" }
        val id = token("id", requestId)
        val session = token("session_id", sessionId)
        val event = token("event_id", eventId)
        val boundedRemote = bounded("remote", remote, maxTokenBytes)
        val body = when (kind) {
            PhoneEventKind.SmsReceived -> {
                val boundedText = bounded("text", text, maxTextBytes)
                "{\"event_id\":${json(event)},\"kind\":${json(kind.wireId)}," +
                    "\"remote\":${json(boundedRemote)},\"text\":${json(boundedText)}}"
            }
            PhoneEventKind.CallSuspectedSpam ->
                "{\"event_id\":${json(event)},\"kind\":${json(kind.wireId)}," +
                    "\"remote\":${json(boundedRemote)}}"
        }
        val envelope =
            "{\"body\":$body,\"id\":${json(id)},\"payload_count\":0," +
                "\"session_id\":${json(session)},\"timestamp_ns\":$timestampNs," +
                "\"type\":\"device.event\"}"
        return listOf(marker.copyOf(), envelope.encodeToByteArray())
    }

    private fun token(name: String, value: String): String {
        if (value.isBlank() || value.encodeToByteArray().size > maxTokenBytes ||
            value.any { it.code !in 0x21..0x7e }
        ) {
            throw ZaraWireException("$name is invalid")
        }
        return value
    }

    private fun bounded(name: String, value: String, maxBytes: Int): String {
        if (value.encodeToByteArray().size > maxBytes) throw ZaraWireException("$name exceeds byte limit")
        if (value.any { it.code < 0x20 || it.code == 0x7f }) {
            throw ZaraWireException("$name contains control characters")
        }
        return value
    }

    private fun json(value: String): String = buildString {
        append('"')
        value.forEach { ch ->
            when (ch) {
                '"' -> append("\\\"")
                '\\' -> append("\\\\")
                '\b' -> append("\\b")
                '\u000C' -> append("\\f")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> if (ch.code < 0x20) {
                    append("\\u")
                    append(ch.code.toString(16).padStart(4, '0'))
                } else append(ch)
            }
        }
        append('"')
    }
}
