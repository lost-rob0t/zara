package ai.zara.app.telemetry

import ai.zara.app.diagnostics.RemoteProtocolContext
import ai.zara.app.diagnostics.StageState
import ai.zara.app.diagnostics.VoiceStageKind
import ai.zara.app.diagnostics.VoiceStageState
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection

enum class VoiceStage {
    CAPTURE,
    VAD,
    STT,
    SUBMIT,
    RESPONSE,
    TTS,
    PLAYBACK,
}

enum class VoiceStageProgress {
    RUNNING,
    COMPLETE,
    FAILED,
    CANCELLED,
    NOT_APPLICABLE,
}

class SessionTelemetry(
    private val journal: ClientEventJournal = ClientEventJournal(),
    val tracker: FailureIncidentTracker = FailureIncidentTracker(),
    private val clock: () -> Long = System::currentTimeMillis,
) {
    private val lock = Any()
    private var lastConnectedAtMillis: Long? = null
    private var lastDisconnectedAtMillis: Long? = null
    private var lastCloseCode: String? = null
    private var lastCloseReason: String? = null
    private var lastRxMessageType: String? = null
    private var lastRxMessageSequence: Long? = null
    private var lastRxMessageBytes: Long? = null
    private var lastTxMessageType: String? = null
    private var lastTxMessageSequence: Long? = null
    private var lastTxMessageBytes: Long? = null
    private var handshakeState: String = "not_started"
    private var negotiatedProtocolVersion: Int? = null
    private var connectGeneration: Long? = null
    private var lastCompletedTurnId: String? = null
    private val stageStates = mutableMapOf<VoiceStage, VoiceStageState>()

    fun journal(): ClientEventJournal = journal

    fun primaryIncident() = tracker.primary()

    fun noteSuccess(phase: String) = tracker.noteSuccess(phase)

    fun onRuntimeStateChanged(previous: RuntimeState, next: RuntimeState) {
        val previousServer = previous.server
        val nextServer = next.server
        if (previousServer != nextServer) {
            val generation = generationOf(nextServer) ?: next.generation
            when {
                previousServer is ServerConnection.Connected && nextServer is ServerConnection.Reconnecting -> {
                    val code = synchronized(lock) { lastCloseCode } ?: ZaraFailureCodes.TRANSPORT_CLOSED
                    synchronized(lock) { lastDisconnectedAtMillis = clock() }
                    journal.record(
                        ClientEventNames.REMOTE_DISCONNECTED,
                        subsystem = ZaraSubsystem.TRANSPORT,
                        connectionGeneration = generation,
                        sessionId = previous.sessionId,
                        code = code,
                        outcome = ClientEventOutcome.FAILURE,
                    )
                }
                previousServer is ServerConnection.Connected && nextServer is ServerConnection.Disconnected -> {
                    synchronized(lock) { lastDisconnectedAtMillis = clock() }
                    journal.record(
                        ClientEventNames.REMOTE_DISCONNECTED,
                        subsystem = ZaraSubsystem.TRANSPORT,
                        connectionGeneration = previousServer.generation,
                        sessionId = previous.sessionId,
                    )
                }
                previousServer is ServerConnection.Connecting && nextServer is ServerConnection.Connected -> {
                    synchronized(lock) {
                        lastConnectedAtMillis = clock()
                        connectGeneration = nextServer.generation
                        handshakeState = "negotiated"
                        negotiatedProtocolVersion = 1
                    }
                    journal.record(
                        ClientEventNames.REMOTE_CONNECT_READY,
                        subsystem = ZaraSubsystem.TRANSPORT,
                        connectionGeneration = nextServer.generation,
                        sessionId = next.sessionId,
                        outcome = ClientEventOutcome.SUCCESS,
                    )
                    tracker.noteSuccess("remote.connect.ready")
                }
                previousServer is ServerConnection.Reconnecting && nextServer is ServerConnection.Connected -> {
                    synchronized(lock) {
                        lastConnectedAtMillis = clock()
                        connectGeneration = nextServer.generation
                        handshakeState = "negotiated"
                        negotiatedProtocolVersion = 1
                    }
                    journal.record(
                        ClientEventNames.REMOTE_RECONNECT_READY,
                        subsystem = ZaraSubsystem.TRANSPORT,
                        connectionGeneration = nextServer.generation,
                        sessionId = next.sessionId,
                        outcome = ClientEventOutcome.SUCCESS,
                    )
                    tracker.noteSuccess("remote.reconnect.ready")
                }
                previousServer is ServerConnection.Disconnected && nextServer is ServerConnection.Connecting -> {
                    journal.record(
                        ClientEventNames.REMOTE_CONNECT_BEGIN,
                        subsystem = ZaraSubsystem.TRANSPORT,
                        connectionGeneration = generation,
                    )
                }
                previousServer is ServerConnection.Reconnecting && nextServer is ServerConnection.Connecting -> {
                    journal.record(
                        ClientEventNames.REMOTE_RECONNECT_BEGIN,
                        subsystem = ZaraSubsystem.TRANSPORT,
                        connectionGeneration = generation,
                    )
                }
                previousServer is ServerConnection.Connecting && nextServer is ServerConnection.Reconnecting -> {
                    journal.record(
                        if (previousServer.generation == nextServer.generation) {
                            ClientEventNames.REMOTE_CONNECT_FAILED
                        } else {
                            ClientEventNames.REMOTE_RECONNECT_FAILED
                        },
                        subsystem = ZaraSubsystem.TRANSPORT,
                        connectionGeneration = previousServer.generation,
                        outcome = ClientEventOutcome.FAILURE,
                    )
                }
                previousServer is ServerConnection.Reconnecting && nextServer is ServerConnection.OfflineDegraded -> {
                    journal.record(
                        ClientEventNames.REMOTE_DISCONNECTED,
                        subsystem = ZaraSubsystem.TRANSPORT,
                        connectionGeneration = previousServer.generation,
                        outcome = ClientEventOutcome.FAILURE,
                    )
                }
                else -> Unit
            }
        }
    }

    fun onConnectionLost(code: String, reason: String?) {
        synchronized(lock) {
            lastCloseCode = code
            lastCloseReason = reason
            lastDisconnectedAtMillis = clock()
        }
    }

    fun onHandshake(state: String, negotiatedVersion: Int?) {
        synchronized(lock) {
            handshakeState = state
            if (negotiatedVersion != null) negotiatedProtocolVersion = negotiatedVersion
        }
    }

    fun onProtocolMessage(
        direction: ClientEventJournal.Direction,
        messageType: String,
        messageSequence: Long?,
        messageBytes: Long,
        connectionGeneration: Long?,
        sessionId: String? = null,
        requestId: String? = null,
        turnId: String? = null,
    ) {
        synchronized(lock) {
            when (direction) {
                ClientEventJournal.Direction.RX -> {
                    lastRxMessageType = messageType
                    lastRxMessageSequence = messageSequence
                    lastRxMessageBytes = messageBytes
                }
                ClientEventJournal.Direction.TX -> {
                    lastTxMessageType = messageType
                    lastTxMessageSequence = messageSequence
                    lastTxMessageBytes = messageBytes
                }
            }
            if (messageType == "turn.completed") lastCompletedTurnId = turnId
        }
        journal.recordProtocolMessage(
            direction = direction,
            messageType = messageType,
            messageSequence = messageSequence,
            messageBytes = messageBytes,
            connectionGeneration = connectionGeneration,
            sessionId = sessionId,
            requestId = requestId,
            turnId = turnId,
        )
    }

    fun onClientFailure(failure: ZaraFailure, eventName: String) {
        journal.record(
            eventName,
            subsystem = failure.subsystem,
            operation = failure.operation,
            phase = failure.phase,
            connectionGeneration = failure.connectionGeneration,
            requestId = failure.requestId,
            turnId = failure.turnId,
            outcome = ClientEventOutcome.FAILURE,
            code = failure.code,
            message = failure.message,
            recovery = failure.recovery,
        )
        synchronized(lock) {
            lastCloseCode = failure.code
            lastCloseReason = failure.message
        }
        tracker.record(failure)
    }

    fun onTurnCompleted(turnId: String?) {
        synchronized(lock) { lastCompletedTurnId = turnId }
        tracker.noteSuccess("turn.completed")
    }

    fun voiceStage(stage: VoiceStage, progress: VoiceStageProgress, code: String? = null) {
        val eventName = when (stage) {
            VoiceStage.CAPTURE -> when (progress) {
                VoiceStageProgress.RUNNING -> ClientEventNames.VOICE_CAPTURE_BEGIN
                VoiceStageProgress.COMPLETE -> ClientEventNames.VOICE_CAPTURE_COMPLETE
                VoiceStageProgress.FAILED -> ClientEventNames.VOICE_CAPTURE_FAILED
                VoiceStageProgress.CANCELLED, VoiceStageProgress.NOT_APPLICABLE -> ClientEventNames.VOICE_CAPTURE_BEGIN
            }
            VoiceStage.STT -> when (progress) {
                VoiceStageProgress.RUNNING -> ClientEventNames.VOICE_STT_BEGIN
                VoiceStageProgress.COMPLETE -> ClientEventNames.VOICE_STT_COMPLETE
                VoiceStageProgress.FAILED -> ClientEventNames.VOICE_STT_FAILED
                VoiceStageProgress.CANCELLED, VoiceStageProgress.NOT_APPLICABLE -> ClientEventNames.VOICE_STT_BEGIN
            }
            VoiceStage.SUBMIT -> when (progress) {
                VoiceStageProgress.RUNNING -> ClientEventNames.VOICE_SUBMIT_BEGIN
                VoiceStageProgress.COMPLETE -> ClientEventNames.VOICE_SUBMIT_COMPLETE
                VoiceStageProgress.FAILED -> ClientEventNames.VOICE_SUBMIT_FAILED
                VoiceStageProgress.CANCELLED, VoiceStageProgress.NOT_APPLICABLE -> ClientEventNames.VOICE_SUBMIT_BEGIN
            }
            VoiceStage.RESPONSE -> when (progress) {
                VoiceStageProgress.RUNNING -> ClientEventNames.VOICE_RESPONSE_FIRST_CHUNK
                VoiceStageProgress.COMPLETE -> ClientEventNames.VOICE_RESPONSE_COMPLETE
                VoiceStageProgress.FAILED -> ClientEventNames.VOICE_RESPONSE_FAILED
                VoiceStageProgress.CANCELLED, VoiceStageProgress.NOT_APPLICABLE -> ClientEventNames.VOICE_RESPONSE_FIRST_CHUNK
            }
            VoiceStage.TTS -> when (progress) {
                VoiceStageProgress.RUNNING -> ClientEventNames.VOICE_TTS_BEGIN
                VoiceStageProgress.COMPLETE -> ClientEventNames.VOICE_TTS_COMPLETE
                VoiceStageProgress.FAILED -> ClientEventNames.VOICE_TTS_FAILED
                VoiceStageProgress.CANCELLED, VoiceStageProgress.NOT_APPLICABLE -> ClientEventNames.VOICE_TTS_BEGIN
            }
            VoiceStage.VAD, VoiceStage.PLAYBACK -> ClientEventNames.VOICE_PLAYBACK_COMPLETE
        }
        journal.record(
            eventName,
            subsystem = ZaraSubsystem.VOICE,
            operation = ZaraOperation.VOICE_TURN,
            outcome = when (progress) {
                VoiceStageProgress.COMPLETE -> ClientEventOutcome.SUCCESS
                VoiceStageProgress.FAILED -> ClientEventOutcome.FAILURE
                VoiceStageProgress.CANCELLED -> ClientEventOutcome.CANCELLED
                else -> null
            },
            code = code,
        )
        synchronized(lock) {
            stageStates[stage] = VoiceStageState(
                kind = stage.toStageKind(),
                state = progress.toStageState(),
                code = code,
                atMillis = clock(),
            )
        }
    }

    fun voiceStages(): Map<VoiceStage, VoiceStageState> = synchronized(lock) { stageStates.toMap() }

    fun remoteContext(): RemoteProtocolContext? = synchronized(lock) {
        RemoteProtocolContext(
            transportKind = "zmq-dealer-tcp-curve",
            connectGeneration = connectGeneration,
            lastConnectedAtMillis = lastConnectedAtMillis,
            lastDisconnectedAtMillis = lastDisconnectedAtMillis,
            negotiatedProtocolVersion = negotiatedProtocolVersion,
            clientProtocolVersions = "1",
            serverProtocolVersion = negotiatedProtocolVersion,
            handshakeState = handshakeState,
            closeCode = lastCloseCode,
            closeReason = lastCloseReason,
            lastRxMessageType = lastRxMessageType,
            lastRxMessageSequence = lastRxMessageSequence,
            lastRxMessageBytes = lastRxMessageBytes,
            lastTxMessageType = lastTxMessageType,
            lastTxMessageSequence = lastTxMessageSequence,
            lastTxMessageBytes = lastTxMessageBytes,
            expectedNextState = null,
            pendingRequestCount = null,
            lastCompletedTurnId = lastCompletedTurnId,
        )
    }

    private fun generationOf(server: ServerConnection): Long? = when (server) {
        is ServerConnection.Connecting -> server.generation
        is ServerConnection.Connected -> server.generation
        is ServerConnection.Reconnecting -> server.generation
        is ServerConnection.Disconnected, is ServerConnection.OfflineDegraded -> null
    }
}

private fun VoiceStage.toStageKind(): VoiceStageKind = when (this) {
    VoiceStage.CAPTURE -> VoiceStageKind.CAPTURE
    VoiceStage.VAD -> VoiceStageKind.VAD
    VoiceStage.STT -> VoiceStageKind.STT
    VoiceStage.SUBMIT -> VoiceStageKind.SUBMIT
    VoiceStage.RESPONSE -> VoiceStageKind.RESPONSE
    VoiceStage.TTS -> VoiceStageKind.TTS
    VoiceStage.PLAYBACK -> VoiceStageKind.PLAYBACK
}

private fun VoiceStageProgress.toStageState(): StageState = when (this) {
    VoiceStageProgress.RUNNING -> StageState.RUNNING
    VoiceStageProgress.COMPLETE -> StageState.COMPLETE
    VoiceStageProgress.FAILED -> StageState.FAILED
    VoiceStageProgress.CANCELLED -> StageState.CANCELLED
    VoiceStageProgress.NOT_APPLICABLE -> StageState.NOT_APPLICABLE
}
