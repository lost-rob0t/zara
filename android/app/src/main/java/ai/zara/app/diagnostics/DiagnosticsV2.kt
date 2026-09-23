package ai.zara.app.diagnostics

import ai.zara.app.telemetry.ClientEvent
import ai.zara.app.telemetry.ClientEventJournal
import ai.zara.app.telemetry.FailureIncident
import ai.zara.app.telemetry.ProtocolFailureEvidence
import ai.zara.app.telemetry.ProtocolMessageEvidence
import java.time.Instant
import java.util.TreeMap

enum class VoiceStageKind(val wireName: String) {
    CAPTURE("capture"),
    VAD("vad"),
    STT("stt"),
    SUBMIT("submit"),
    RESPONSE("response"),
    TTS("tts"),
    PLAYBACK("playback"),
}

enum class StageState(val wireName: String) {
    NOT_STARTED("not_started"),
    RUNNING("running"),
    COMPLETE("complete"),
    FAILED("failed"),
    CANCELLED("cancelled"),
    NOT_APPLICABLE("not_applicable"),
}

data class VoiceStageState(
    val kind: VoiceStageKind,
    val state: StageState,
    val code: String?,
    val atMillis: Long?,
)

data class RemoteProtocolContext(
    val transportKind: String,
    val connectGeneration: Long?,
    val lastConnectedAtMillis: Long?,
    val lastDisconnectedAtMillis: Long?,
    val negotiatedProtocolVersion: Int?,
    val clientProtocolVersions: String,
    val serverProtocolVersion: Int?,
    val handshakeState: String,
    val closeCode: String?,
    val closeReason: String?,
    val lastRxMessageType: String?,
    val lastRxMessageSequence: Long?,
    val lastRxMessageBytes: Long?,
    val lastTxMessageType: String?,
    val lastTxMessageSequence: Long?,
    val lastTxMessageBytes: Long?,
    val expectedNextState: String?,
    val pendingRequestCount: Int?,
    val lastCompletedTurnId: String?,
)

data class DiagnosticsSnapshot(
    val version: String,
    val versionCode: Int,
    val sourceSha: String,
    val runtimeMode: String,
    val sessionId: String?,
    val sessionGeneration: Long?,
    val connectionPhase: String,
    val enrollmentPhase: String,
    val incident: FailureIncident?,
    val remoteContext: RemoteProtocolContext?,
    val voiceStages: List<VoiceStageState>,
    val localAiPhase: String,
    val localAiNote: String?,
    val localAiGeneration: Long?,
    val localAiModel: String?,
    val localServerPhase: String,
    val localServerGeneration: Long,
    val localServerFailure: String?,
    val events: List<ClientEvent>,
    val diagnosticId: String,
    val capturedAtMillis: Long,
)

data class DiagnosticsBundle(val text: String, val json: String)

object DiagnosticsV2 {
    private const val MARKER = "ZARA-LOCAL-DIAGNOSTICS/2"
    private const val TIMELINE_WINDOW = 64
    private const val MAX_REASON_CHARS = 256
    private const val PROTOCOL_TRACE_WINDOW = 32

    fun render(snapshot: DiagnosticsSnapshot): DiagnosticsBundle {
        val facts = textFacts(snapshot)
        val evidence = snapshot.incident?.failure?.protocolEvidence
        val text = buildString {
            append(MARKER)
            append('\n')
            for ((key, value) in facts) {
                append(key)
                append('=')
                append(value)
                append('\n')
            }
            append("--- timeline ---\n")
            append(timeline(snapshot.events))
            if (evidence != null) {
                append("--- protocol failure trace ---\n")
                for (message in evidence.messages.takeLast(PROTOCOL_TRACE_WINDOW)) {
                    append(protocolMessageFacts(message).entries.joinToString(" ") { (key, value) ->
                        "$key=${value ?: "unknown"}"
                    })
                    append('\n')
                }
            }
        }
        return DiagnosticsBundle(text = text, json = json(facts, snapshot.events, evidence))
    }

    private fun textFacts(snapshot: DiagnosticsSnapshot): Map<String, String> {
        val facts = TreeMap<String, String>()
        facts["captured_at"] = Instant.ofEpochMilli(snapshot.capturedAtMillis).toString()
        facts["connection_phase"] = snapshot.connectionPhase
        facts["diagnostic_id"] = snapshot.diagnosticId
        facts["diagnostics_version"] = "2"
        facts["enrollment_phase"] = snapshot.enrollmentPhase
        facts["runtime_mode"] = snapshot.runtimeMode
        facts["session_generation"] = snapshot.sessionGeneration?.toString() ?: "unknown"
        facts["session_id"] = snapshot.sessionId ?: "none"
        facts["source_sha"] = snapshot.sourceSha
        facts["version"] = snapshot.version
        facts["version_code"] = snapshot.versionCode.toString()

        val incident = snapshot.incident
        if (incident == null) {
            facts["primary_failure.present"] = "false"
        } else {
            val failure = incident.failure
            facts["primary_failure.present"] = "true"
            facts["primary_failure.subsystem"] = failure.subsystem.name.lowercase()
            facts["primary_failure.operation"] = failure.operation.name.lowercase()
            facts["primary_failure.phase"] = failure.phase ?: "unknown"
            facts["primary_failure.code"] = failure.code
            facts["primary_failure.message"] = sanitize(failure.message)
            facts["primary_failure.cause_class"] = sanitize(failure.causeClass)
            facts["primary_failure.first_seen"] = Instant.ofEpochMilli(incident.firstSeenMillis).toString()
            facts["primary_failure.last_seen"] = Instant.ofEpochMilli(incident.lastSeenMillis).toString()
            facts["primary_failure.request_id"] = failure.requestId ?: "none"
            facts["primary_failure.turn_id"] = failure.turnId ?: "none"
            facts["primary_failure.last_success"] = incident.lastSuccess ?: "none"
            facts["primary_failure.recovery"] = failure.recovery.name.lowercase()
            if (failure.serverCode != null) facts["primary_failure.server_code"] = sanitize(failure.serverCode)
            if (failure.connectionGeneration != null) {
                facts["primary_failure.connection_generation"] = failure.connectionGeneration.toString()
            }
            failure.protocolEvidence?.let { protocolFailureFacts(facts, it) }
        }

        snapshot.remoteContext?.let { remote ->
            facts["remote.transport"] = remote.transportKind
            facts["remote.connect_generation"] = remote.connectGeneration?.toString() ?: "unknown"
            facts["remote.last_connected_at"] = remote.lastConnectedAtMillis?.let { Instant.ofEpochMilli(it).toString() } ?: "unknown"
            facts["remote.last_disconnected_at"] = remote.lastDisconnectedAtMillis?.let { Instant.ofEpochMilli(it).toString() } ?: "unknown"
            facts["remote.negotiated_protocol_version"] = remote.negotiatedProtocolVersion?.toString() ?: "unknown"
            facts["remote.client_protocol_versions"] = remote.clientProtocolVersions
            facts["remote.server_protocol_version"] = remote.serverProtocolVersion?.toString() ?: "unknown"
            facts["remote.handshake_state"] = remote.handshakeState
            facts["remote.close_code"] = remote.closeCode ?: "none"
            facts["remote.close_reason"] = remote.closeReason?.let(::sanitize) ?: "none"
            facts["remote.last_rx_message_type"] = remote.lastRxMessageType ?: "none"
            facts["remote.last_rx_message_seq"] = remote.lastRxMessageSequence?.toString() ?: "none"
            facts["remote.last_rx_message_bytes"] = remote.lastRxMessageBytes?.toString() ?: "none"
            facts["remote.last_tx_message_type"] = remote.lastTxMessageType ?: "none"
            facts["remote.last_tx_message_seq"] = remote.lastTxMessageSequence?.toString() ?: "none"
            facts["remote.last_tx_message_bytes"] = remote.lastTxMessageBytes?.toString() ?: "none"
            facts["remote.expected_next_state"] = remote.expectedNextState ?: "unknown"
            facts["remote.pending_requests"] = remote.pendingRequestCount?.toString() ?: "unknown"
            facts["remote.last_completed_turn_id"] = remote.lastCompletedTurnId ?: "none"
        }

        val byKind = snapshot.voiceStages.associateBy { it.kind }
        for (kind in VoiceStageKind.values()) {
            val stage = byKind[kind]
            facts["voice.${kind.wireName}"] = stage?.state?.wireName ?: StageState.NOT_STARTED.wireName
            if (stage?.code != null) facts["voice.${kind.wireName}.code"] = stage.code
            if (stage?.atMillis != null) facts["voice.${kind.wireName}.at"] = Instant.ofEpochMilli(stage.atMillis).toString()
        }

        facts["local_ai_phase"] = snapshot.localAiPhase
        if (snapshot.localAiNote != null) facts["local_ai_note"] = sanitize(snapshot.localAiNote)
        facts["local_ai_generation"] = snapshot.localAiGeneration?.toString() ?: "not_applicable"
        facts["local_ai_model"] = snapshot.localAiModel ?: "not_applicable"
        facts["local_server_phase"] = snapshot.localServerPhase
        facts["local_server_generation"] = snapshot.localServerGeneration.toString()
        facts["local_server_failure"] = snapshot.localServerFailure ?: "none"

        return facts
    }

    private fun protocolFailureFacts(facts: MutableMap<String, String>, evidence: ProtocolFailureEvidence) {
        val values = linkedMapOf<String, Any?>(
            "evidence_scope" to "failed_text_turn",
            "failure_session_id" to evidence.sessionId,
            "expected_message_type" to evidence.expectedMessageType,
            "actual_message_type" to evidence.lastRx?.messageType,
            "actual_message_id" to evidence.lastRx?.messageId,
            "actual_reply_to" to evidence.lastRx?.replyTo,
            "actual_session_id" to evidence.lastRx?.sessionId,
            "actual_turn_id" to evidence.lastRx?.turnId,
            "actual_message_seq" to evidence.lastRx?.sequence,
            "last_rx_bytes" to evidence.lastRx?.bytes,
            "last_rx_frame_count" to evidence.lastRx?.frameCount,
            "last_rx_state" to evidence.lastRx?.state,
            "last_tx_message_type" to evidence.lastTx?.messageType,
            "last_tx_bytes" to evidence.lastTx?.bytes,
            "last_tx_frame_count" to evidence.lastTx?.frameCount,
            "last_tx_state" to evidence.lastTx?.state,
            "pending_requests" to evidence.pendingRequests,
            "receive_timeout_ms" to evidence.requestTimeoutMillis,
            "turn_elapsed_ms" to evidence.elapsedMillis,
            "trace_dropped_messages" to evidence.droppedMessages,
        )
        for ((key, value) in values) facts["primary_failure.$key"] = value?.toString()?.let(::sanitize) ?: "unknown"
    }

    private fun protocolMessageFacts(message: ProtocolMessageEvidence): Map<String, Any?> = linkedMapOf(
        "direction" to sanitize(message.direction),
        "type" to message.messageType?.let(::sanitize),
        "id" to message.messageId?.let(::sanitize),
        "reply_to" to message.replyTo?.let(::sanitize),
        "session_id" to message.sessionId?.let(::sanitize),
        "turn_id" to message.turnId?.let(::sanitize),
        "seq" to message.sequence,
        "bytes" to message.bytes,
        "frames" to message.frameCount,
        "state" to sanitize(message.state),
    )

    private fun timeline(events: List<ClientEvent>): String {
        if (events.isEmpty()) return "(no recorded events)\n"
        val window = events.takeLast(TIMELINE_WINDOW)
        return buildString {
            for (event in window) {
                append(Instant.ofEpochMilli(event.wallTimeMillis))
                append(" seq=")
                append(event.sequence)
                append(" event=")
                append(event.name)
                if (event.connectionGeneration != null) {
                    append(" generation=")
                    append(event.connectionGeneration)
                }
                if (event.sessionId != null) {
                    append(" session=")
                    append(event.sessionId)
                }
                if (event.requestId != null) {
                    append(" request_id=")
                    append(event.requestId)
                }
                if (event.turnId != null) {
                    append(" turn_id=")
                    append(event.turnId)
                }
                if (event.messageType != null) {
                    append(" message_type=")
                    append(event.messageType)
                    if (event.messageSequence != null) {
                        append(" message_seq=")
                        append(event.messageSequence)
                    }
                    append(" bytes=")
                    append(event.messageBytes)
                }
                if (event.code != null) {
                    append(" code=")
                    append(event.code)
                }
                if (event.outcome != null) append(" outcome=").append(event.outcome.name.lowercase())
                if (event.message != null) {
                    append(" message=")
                    append(sanitize(event.message.take(MAX_REASON_CHARS)))
                }
                append('\n')
            }
        }
    }

    private fun json(
        facts: Map<String, String>,
        events: List<ClientEvent>,
        evidence: ProtocolFailureEvidence?,
    ): String {
        val root = TreeMap<String, Any?>()
        for ((key, value) in facts) {
            if (key == "diagnostics_version") {
                root[key] = 2L
                continue
            }
            val separator = key.indexOf('.')
            if (separator > 0 && key.substring(0, separator) in NESTED_SECTIONS) {
                val section = key.substring(0, separator)
                val child = key.substring(separator + 1)
                @Suppress("UNCHECKED_CAST")
                val nested = root[section] as? TreeMap<String, Any?>
                    ?: TreeMap<String, Any?>().also { root[section] = it }
                nested[child] = value
            } else {
                root[key] = value
            }
        }
        if (evidence != null) {
            root["protocol_failure_trace"] = evidence.messages.takeLast(PROTOCOL_TRACE_WINDOW).map(::protocolMessageFacts)
        }
        root["timeline"] = events.takeLast(TIMELINE_WINDOW).map { event ->
            val entry = TreeMap<String, Any?>()
            entry["event"] = event.name
            entry["monotonic_ns"] = event.monotonicNanos
            entry["seq"] = event.sequence
            entry["timestamp"] = Instant.ofEpochMilli(event.wallTimeMillis).toString()
            if (event.subsystem != null) entry["subsystem"] = event.subsystem.name.lowercase()
            if (event.operation != null) entry["operation"] = event.operation.name.lowercase()
            if (event.phase != null) entry["phase"] = event.phase
            if (event.connectionGeneration != null) entry["generation"] = event.connectionGeneration
            if (event.sessionId != null) entry["session_id"] = event.sessionId
            if (event.requestId != null) entry["request_id"] = event.requestId
            if (event.turnId != null) entry["turn_id"] = event.turnId
            if (event.messageType != null) {
                entry["message_type"] = event.messageType
                if (event.messageSequence != null) entry["message_seq"] = event.messageSequence
                entry["bytes"] = event.messageBytes
            }
            if (event.outcome != null) entry["outcome"] = event.outcome.name.lowercase()
            if (event.code != null) entry["code"] = event.code
            if (event.message != null) entry["message"] = sanitize(event.message.take(MAX_REASON_CHARS))
            entry
        }
        return encode(root)
    }

    private val NESTED_SECTIONS = setOf("primary_failure", "remote", "voice", "local_ai")

    private fun encode(value: Any?): String = when (value) {
        null -> "null"
        is Long -> value.toString()
        is Number -> value.toString()
        is Map<*, *> -> value.entries
            .sortedBy { it.key.toString() }
            .joinToString(prefix = "{", postfix = "}", separator = ",") { entry ->
                "${encode(entry.key.toString())}:${encode(entry.value)}"
            }
        is List<*> -> value.joinToString(prefix = "[", postfix = "]", separator = ",") { encode(it) }
        else -> encodeString(value.toString())
    }

    private fun encodeString(text: String): String = buildString {
        append('"')
        for (character in text) {
            when (character) {
                '"' -> append("\\\"")
                '\\' -> append("\\\\")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> {
                    if (character.code < 0x20) {
                        append("\\u")
                        append(String.format("%04x", character.code))
                    } else {
                        append(character)
                    }
                }
            }
        }
        append('"')
    }

    private fun sanitize(text: String): String = ClientEventJournal.sanitize(text)
}
