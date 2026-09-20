package ai.zara.app.prolog

private const val SYMBOLIC_DIALOGUE_PROTOCOL = "ZARA-SYMBOLIC-DIALOGUE/1"
private const val SYMBOLIC_DIALOGUE_RENDERER = "symbolic-dcg/v1"
private const val MAX_SHORT_TEXT_CODE_POINTS = 256
private const val MAX_LONG_TEXT_CODE_POINTS = 1024
private const val MAX_CHOICES = 8

/**
 * Android projection of the canonical ZARA-SYMBOLIC-DIALOGUE/1 response-act envelope.
 *
 * This is a validator/typed projection only. It does not route natural language, select an
 * expert, execute an effect, or own conversation history. Those authorities stay in the shared
 * symbolic runtime and existing Zara effect/conversation boundaries.
 *
 * The wire contract is deliberately fail-closed: Android accepts only the pure-symbolic
 * envelope shape with providers disabled and exact zero provider/model usage. A malformed or
 * future incompatible envelope must be handled as unavailable by its caller, never as a reason
 * to invoke a provider.
 */
enum class SymbolicResponseActKind(val wireName: String) {
    GREETING("greeting"),
    HELP("help"),
    ACKNOWLEDGEMENT("acknowledgement"),
    CANCELLED("cancelled"),
    CLARIFY("clarify"),
    CHOOSE("choose"),
    INVALID("invalid"),
    DISPATCH_REQUIRED("dispatch_required"),
    VERIFIED("verified"),
    DENIED("denied"),
    UNAVAILABLE("unavailable"),
    ERROR("error"),
    EXPERT_ANSWER("expert_answer"),
    UNSUPPORTED("unsupported");

    companion object {
        fun fromWire(value: String): SymbolicResponseActKind = entries
            .firstOrNull { it.wireName == value }
            ?: throw IllegalArgumentException("Unsupported symbolic response act: $value")
    }
}

enum class SymbolicAcknowledgementKind(val wireName: String) {
    THANKS("thanks"),
    ACKNOWLEDGED("acknowledged");

    companion object {
        fun fromWire(value: String): SymbolicAcknowledgementKind = entries
            .firstOrNull { it.wireName == value }
            ?: throw IllegalArgumentException("Unsupported symbolic acknowledgement kind: $value")
    }
}

sealed interface SymbolicResponsePayload {
    data object Empty : SymbolicResponsePayload

    data class Acknowledgement(
        val kind: SymbolicAcknowledgementKind,
    ) : SymbolicResponsePayload

    data class Clarification(
        val slot: String? = null,
        val reason: String? = null,
    ) : SymbolicResponsePayload {
        init {
            require((slot == null) != (reason == null)) {
                "Clarification requires exactly one of slot or reason"
            }
        }
    }

    data class Choices(val choices: List<String>) : SymbolicResponsePayload

    data class Invalid(
        val slot: String,
        val reason: String,
    ) : SymbolicResponsePayload

    data class DispatchRequired(val frameRef: String) : SymbolicResponsePayload

    data class Verified(
        val outcome: String,
        val evidenceRef: String,
    ) : SymbolicResponsePayload

    data class Reason(val reason: String) : SymbolicResponsePayload

    data class ExpertAnswer(
        val summary: String,
        val evidenceRef: String,
    ) : SymbolicResponsePayload
}

data class SymbolicResponseUsage(
    val providerCalls: Int,
    val modelCalls: Int,
) {
    init {
        require(providerCalls == 0) { "Pure symbolic response requires provider_calls=0" }
        require(modelCalls == 0) { "Pure symbolic response requires model_calls=0" }
    }
}

data class SymbolicResponseAct private constructor(
    val kind: SymbolicResponseActKind,
    val payload: SymbolicResponsePayload,
    val usage: SymbolicResponseUsage,
) {
    val protocol: String = SYMBOLIC_DIALOGUE_PROTOCOL
    val renderer: String = SYMBOLIC_DIALOGUE_RENDERER
    val providersEnabled: Boolean = false
    val maxModelCalls: Int = 0

    companion object {
        private val requiredEnvelopeKeys = setOf(
            "protocol",
            "act",
            "payload",
            "renderer",
            "providers_enabled",
            "max_model_calls",
            "usage",
        )

        fun fromEnvelope(envelope: Map<String, *>): SymbolicResponseAct {
            requireExactKeys(envelope, requiredEnvelopeKeys, "symbolic response envelope")
            require(envelope["protocol"] == SYMBOLIC_DIALOGUE_PROTOCOL) {
                "Unsupported symbolic dialogue protocol"
            }
            require(envelope["renderer"] == SYMBOLIC_DIALOGUE_RENDERER) {
                "Unsupported symbolic response renderer"
            }
            require(envelope["providers_enabled"] == false) {
                "Pure symbolic response requires providers_enabled=false"
            }
            requireZeroInteger(envelope["max_model_calls"], "max_model_calls")

            val usageMap = requireStringMap(envelope["usage"], "usage")
            requireExactKeys(
                usageMap,
                setOf("provider_calls", "model_calls"),
                "usage",
            )
            val usage = SymbolicResponseUsage(
                providerCalls = requireZeroInteger(usageMap["provider_calls"], "provider_calls"),
                modelCalls = requireZeroInteger(usageMap["model_calls"], "model_calls"),
            )

            val kind = SymbolicResponseActKind.fromWire(
                requireBoundedString(envelope["act"], "act", MAX_SHORT_TEXT_CODE_POINTS),
            )
            val payloadMap = requireStringMap(envelope["payload"], "payload")
            val payload = parsePayload(kind, payloadMap)
            return SymbolicResponseAct(kind = kind, payload = payload, usage = usage)
        }

        private fun parsePayload(
            kind: SymbolicResponseActKind,
            payload: Map<String, *>,
        ): SymbolicResponsePayload = when (kind) {
            SymbolicResponseActKind.GREETING,
            SymbolicResponseActKind.HELP,
            SymbolicResponseActKind.CANCELLED,
            SymbolicResponseActKind.UNSUPPORTED,
            -> {
                requireExactKeys(payload, emptySet(), "${kind.wireName} payload")
                SymbolicResponsePayload.Empty
            }

            SymbolicResponseActKind.ACKNOWLEDGEMENT -> {
                requireExactKeys(payload, setOf("kind"), "acknowledgement payload")
                val wireKind = requireBoundedString(
                    payload["kind"],
                    "acknowledgement.kind",
                    MAX_SHORT_TEXT_CODE_POINTS,
                )
                SymbolicResponsePayload.Acknowledgement(
                    kind = SymbolicAcknowledgementKind.fromWire(wireKind),
                )
            }

            SymbolicResponseActKind.CLARIFY -> {
                require(payload.size == 1 && payload.keys.single() in setOf("slot", "reason")) {
                    "clarify payload requires exactly one of slot or reason"
                }
                if (payload.containsKey("slot")) {
                    SymbolicResponsePayload.Clarification(
                        slot = requireBoundedString(
                            payload["slot"],
                            "clarify.slot",
                            MAX_SHORT_TEXT_CODE_POINTS,
                        ),
                    )
                } else {
                    SymbolicResponsePayload.Clarification(
                        reason = requireBoundedString(
                            payload["reason"],
                            "clarify.reason",
                            MAX_SHORT_TEXT_CODE_POINTS,
                        ),
                    )
                }
            }

            SymbolicResponseActKind.CHOOSE -> {
                requireExactKeys(payload, setOf("choices"), "choose payload")
                val rawChoices = payload["choices"] as? List<*>
                    ?: throw IllegalArgumentException("choose.choices must be an array")
                require(rawChoices.size in 1..MAX_CHOICES) {
                    "choose.choices must contain 1..$MAX_CHOICES items"
                }
                SymbolicResponsePayload.Choices(
                    rawChoices.mapIndexed { index, value ->
                        requireBoundedString(
                            value,
                            "choose.choices[$index]",
                            MAX_SHORT_TEXT_CODE_POINTS,
                        )
                    },
                )
            }

            SymbolicResponseActKind.INVALID -> {
                requireExactKeys(payload, setOf("slot", "reason"), "invalid payload")
                SymbolicResponsePayload.Invalid(
                    slot = requireBoundedString(
                        payload["slot"],
                        "invalid.slot",
                        MAX_SHORT_TEXT_CODE_POINTS,
                    ),
                    reason = requireBoundedString(
                        payload["reason"],
                        "invalid.reason",
                        MAX_SHORT_TEXT_CODE_POINTS,
                    ),
                )
            }

            SymbolicResponseActKind.DISPATCH_REQUIRED -> {
                requireExactKeys(
                    payload,
                    setOf("frame_ref"),
                    "dispatch_required payload",
                )
                SymbolicResponsePayload.DispatchRequired(
                    frameRef = requireBoundedString(
                        payload["frame_ref"],
                        "dispatch_required.frame_ref",
                        MAX_SHORT_TEXT_CODE_POINTS,
                    ),
                )
            }

            SymbolicResponseActKind.VERIFIED -> {
                requireExactKeys(
                    payload,
                    setOf("outcome", "evidence_ref"),
                    "verified payload",
                )
                SymbolicResponsePayload.Verified(
                    outcome = requireBoundedString(
                        payload["outcome"],
                        "verified.outcome",
                        MAX_LONG_TEXT_CODE_POINTS,
                    ),
                    evidenceRef = requireBoundedString(
                        payload["evidence_ref"],
                        "verified.evidence_ref",
                        MAX_LONG_TEXT_CODE_POINTS,
                    ),
                )
            }

            SymbolicResponseActKind.DENIED,
            SymbolicResponseActKind.UNAVAILABLE,
            SymbolicResponseActKind.ERROR,
            -> {
                requireExactKeys(payload, setOf("reason"), "${kind.wireName} payload")
                SymbolicResponsePayload.Reason(
                    reason = requireBoundedString(
                        payload["reason"],
                        "${kind.wireName}.reason",
                        MAX_SHORT_TEXT_CODE_POINTS,
                    ),
                )
            }

            SymbolicResponseActKind.EXPERT_ANSWER -> {
                requireExactKeys(
                    payload,
                    setOf("summary", "evidence_ref"),
                    "expert_answer payload",
                )
                SymbolicResponsePayload.ExpertAnswer(
                    summary = requireBoundedString(
                        payload["summary"],
                        "expert_answer.summary",
                        MAX_LONG_TEXT_CODE_POINTS,
                    ),
                    evidenceRef = requireBoundedString(
                        payload["evidence_ref"],
                        "expert_answer.evidence_ref",
                        MAX_SHORT_TEXT_CODE_POINTS,
                    ),
                )
            }
        }

        private fun requireStringMap(value: Any?, label: String): Map<String, *> {
            require(value is Map<*, *>) { "$label must be an object" }
            require(value.keys.all { it is String }) { "$label keys must be strings" }
            @Suppress("UNCHECKED_CAST")
            return value as Map<String, *>
        }

        private fun requireExactKeys(
            value: Map<String, *>,
            expected: Set<String>,
            label: String,
        ) {
            require(value.keys == expected) {
                "$label keys do not match the canonical contract"
            }
        }

        private fun requireBoundedString(
            value: Any?,
            label: String,
            maxCodePoints: Int,
        ): String {
            require(value is String) { "$label must be a string" }
            val codePoints = value.codePointCount(0, value.length)
            require(codePoints in 1..maxCodePoints) {
                "$label must contain 1..$maxCodePoints code points"
            }
            return value
        }

        private fun requireZeroInteger(value: Any?, label: String): Int {
            val integer = when (value) {
                is Byte -> value.toLong()
                is Short -> value.toLong()
                is Int -> value.toLong()
                is Long -> value
                else -> throw IllegalArgumentException("$label must be an integer")
            }
            require(integer == 0L) { "$label must be exactly zero" }
            return 0
        }
    }
}
