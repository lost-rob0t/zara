package ai.zara.app.expert

const val ZARA_EXPERT_PROTOCOL = "ZARA-EXPERT/1"

private val expertProtocolPattern = Regex("^ZARA-EXPERT/[1-9][0-9]*$")
private val expertIdPattern = Regex("^[!-~]{1,128}$")
private val packageNamespacePattern = Regex("^[a-z][a-z0-9_-]{0,63}$")
private val operationIdPattern = Regex("^[a-z][a-z0-9_.-]{0,63}$")

enum class ReasoningKind(val wire: String) {
    SYMBOLIC("symbolic"), HYBRID("hybrid"), MODEL("model"), SERVICE("service");

    companion object {
        fun fromWire(value: String): ReasoningKind =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown reasoning kind: $value")
    }
}

enum class ExpertAvailability(val wire: String) {
    INSTALLED("installed"), AVAILABLE("available"), READY("ready"),
    UNAVAILABLE("unavailable"), ABSENT("absent");

    companion object {
        fun fromWire(value: String): ExpertAvailability =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown expert availability: $value")
    }
}

enum class ExpertVerdict(val wire: String) {
    SUCCEEDED("succeeded"), FAILED("failed"), UNKNOWN("unknown"), BLOCKED("blocked"),
    UNSUPPORTED("unsupported"), CANCELLED("cancelled"), ERROR("error");

    companion object {
        fun fromWire(value: String): ExpertVerdict =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown expert verdict: $value")
    }
}

enum class LifecycleState(val wire: String) {
    INACTIVE("inactive"), ACTIVATING("activating"), ACTIVE("active"), DRAINING("draining"),
    UNAVAILABLE("unavailable"), FAILED("failed");

    companion object {
        fun fromWire(value: String): LifecycleState =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown lifecycle state: $value")
    }
}

enum class ExpertErrorCode(val wire: String) {
    INVALID_INPUT("invalid_input"), AMBIGUITY("ambiguity"),
    UNSUPPORTED_OPERATION("unsupported_operation"), UNSUPPORTED_BACKEND("unsupported_backend"),
    INCOMPATIBLE_PROTOCOL("incompatible_protocol"), DENIED("denied"),
    APPROVAL_REQUIRED("approval_required"), STALE_GENERATION("stale_generation"),
    UNAVAILABLE("unavailable"), DEADLINE_EXCEEDED("deadline_exceeded"),
    BUDGET_EXCEEDED("budget_exceeded"), CANCELLED("cancelled"),
    INTERRUPTED("interrupted"), UNKNOWN_EXTERNAL_OUTCOME("unknown_external_outcome");

    companion object {
        fun fromWire(value: String): ExpertErrorCode =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown expert error code: $value")
    }
}

data class ExpertLimits(
    val timeoutMs: Int,
    val maxResults: Int,
    val maxOutputBytes: Int,
    val maxModelCalls: Int = 0,
) {
    init {
        require(timeoutMs in 1..ExpertContract.HOST_CEILING_TIMEOUT_MS) {
            "limits timeout_ms is outside host ceilings"
        }
        require(maxResults in 1..ExpertContract.HOST_CEILING_MAX_RESULTS) {
            "limits max_results is outside host ceilings"
        }
        require(maxOutputBytes in 1..ExpertContract.HOST_CEILING_MAX_OUTPUT_BYTES) {
            "limits max_output_bytes is outside host ceilings"
        }
        require(maxModelCalls in 0..ExpertContract.HOST_CEILING_MAX_MODEL_CALLS) {
            "limits max_model_calls is outside host ceilings"
        }
    }

    fun validAgainstCeilings(): Boolean =
        timeoutMs in 1..ExpertContract.HOST_CEILING_TIMEOUT_MS &&
            maxResults in 1..ExpertContract.HOST_CEILING_MAX_RESULTS &&
            maxOutputBytes in 1..ExpertContract.HOST_CEILING_MAX_OUTPUT_BYTES &&
            maxModelCalls in 0..ExpertContract.HOST_CEILING_MAX_MODEL_CALLS
}

data class ExpertCatalogProjection(
    val expertId: String,
    val expertVersion: String,
    val name: String,
    val description: String,
    val reasoningKind: String,
    val operations: List<String>,
    val applicabilityKeywords: List<String>,
    val availability: String,
    val unavailableReason: String,
)

data class ExpertDescriptorRow(
    val protocol: String,
    val expertId: String,
    val expertVersion: String,
    val packageNamespace: String,
    val manifestDigest: String,
    val name: String,
    val description: String,
    val sourceReference: String,
    val reasoningKind: ReasoningKind,
    val operations: List<String>,
    val applicabilityKeywords: List<String>,
    val requiredCapabilities: List<String>,
    val possibleEffects: List<String>,
    val supportedEngines: List<String>,
    val supportedPlatforms: List<String>,
    val fallbackPolicy: String,
    val delegationPolicy: String,
    val availability: ExpertAvailability,
    val unavailableReason: String,
) {
    init {
        requireBoundedText(protocol, "protocol", 32)
        require(expertProtocolPattern.matches(protocol)) {
            "expert '$expertId' declares malformed protocol: $protocol"
        }
        require(protocol == ZARA_EXPERT_PROTOCOL) {
            "expert '$expertId' declares incompatible protocol: $protocol"
        }
        require(expertIdPattern.matches(expertId)) { "invalid expert id: $expertId" }
        requireBoundedText(expertVersion, "expert_version", 64)
        require(packageNamespacePattern.matches(packageNamespace)) {
            "invalid package namespace: $packageNamespace"
        }
        requireBoundedText(manifestDigest, "manifest_digest", 256)
        requireBoundedText(name, "name", 128)
        requireBoundedText(description, "description", 1024)
        requireBoundedText(sourceReference, "source_reference", 512)
        requireTokens(operations, "operations", 64, operationIdPattern)
        requireTokens(applicabilityKeywords, "applicability_keywords", 64)
        requireTokens(requiredCapabilities, "required_capabilities", 64)
        requireTokens(possibleEffects, "possible_effects", 64)
        requireTokens(supportedEngines, "supported_engines", 64)
        requireTokens(supportedPlatforms, "supported_platforms", 64)
        requireBoundedText(fallbackPolicy, "fallback_policy", 64)
        requireBoundedText(delegationPolicy, "delegation_policy", 64)
        if (availability == ExpertAvailability.UNAVAILABLE || availability == ExpertAvailability.ABSENT) {
            require(unavailableReason.isNotEmpty()) {
                "unavailable_reason is required when availability is ${availability.wire}"
            }
        } else {
            require(unavailableReason.isEmpty()) {
                "unavailable_reason must be empty when availability is ${availability.wire}"
            }
        }
    }

    val protocolCompatible: Boolean get() = protocol == ZARA_EXPERT_PROTOCOL

    val selectable: Boolean
        get() = protocolCompatible && (
            availability == ExpertAvailability.INSTALLED ||
                availability == ExpertAvailability.AVAILABLE ||
                availability == ExpertAvailability.READY
            )

    fun catalogProjection(): ExpertCatalogProjection = ExpertCatalogProjection(
        expertId = expertId,
        expertVersion = expertVersion,
        name = name,
        description = description,
        reasoningKind = reasoningKind.wire,
        operations = operations,
        applicabilityKeywords = applicabilityKeywords,
        availability = availability.wire,
        unavailableReason = unavailableReason,
    )

    companion object {
        fun fromTsvRow(row: Map<String, String>): ExpertDescriptorRow {
            val availability = ExpertAvailability.fromWire(row.getValue("availability"))
            val unavailableReason = row.optional("unavailable_reason")
            return ExpertDescriptorRow(
                protocol = row.getValue("protocol"),
                expertId = row.getValue("expert_id"),
                expertVersion = row.getValue("expert_version"),
                packageNamespace = row.getValue("package_namespace"),
                manifestDigest = row.getValue("manifest_digest"),
                name = row.getValue("name"),
                description = row.getValue("description"),
                sourceReference = row.getValue("source_reference"),
                reasoningKind = ReasoningKind.fromWire(row.getValue("reasoning_kind")),
                operations = row.items("operations"),
                applicabilityKeywords = row.items("applicability_keywords"),
                requiredCapabilities = row.items("required_capabilities"),
                possibleEffects = row.items("possible_effects"),
                supportedEngines = row.items("supported_engines"),
                supportedPlatforms = row.items("supported_platforms"),
                fallbackPolicy = row.getValue("fallback_policy"),
                delegationPolicy = row.getValue("delegation_policy"),
                availability = availability,
                unavailableReason = unavailableReason,
            )
        }

        private fun Map<String, String>.items(key: String): List<String> {
            val value = getValue(key)
            return if (value == "-" || value.isEmpty()) emptyList() else value.split(',')
        }

        private fun Map<String, String>.optional(key: String): String {
            val value = getValue(key)
            return if (value == "-") "" else value
        }
    }
}

object ExpertContract {
    const val HOST_CEILING_TIMEOUT_MS = 600_000
    const val HOST_CEILING_MAX_RESULTS = 10_000
    const val HOST_CEILING_MAX_OUTPUT_BYTES = 10_485_760
    const val HOST_CEILING_MAX_MODEL_CALLS = 64

    val activationTransitions: Map<LifecycleState, Set<LifecycleState>> = mapOf(
        LifecycleState.INACTIVE to setOf(LifecycleState.ACTIVATING),
        LifecycleState.ACTIVATING to setOf(LifecycleState.ACTIVE, LifecycleState.FAILED),
        LifecycleState.ACTIVE to setOf(
            LifecycleState.DRAINING,
            LifecycleState.UNAVAILABLE,
            LifecycleState.FAILED,
        ),
        LifecycleState.DRAINING to setOf(LifecycleState.INACTIVE),
        LifecycleState.UNAVAILABLE to setOf(LifecycleState.ACTIVE),
        LifecycleState.FAILED to setOf(LifecycleState.INACTIVE),
    )

    fun transitionValid(from: LifecycleState, to: LifecycleState): Boolean =
        activationTransitions[from]?.contains(to) == true

    fun protocolCompatible(value: String): Boolean =
        value == ZARA_EXPERT_PROTOCOL && expertProtocolPattern.matches(value)
}

private fun requireBoundedText(
    value: String,
    field: String,
    limit: Int,
    allowEmpty: Boolean = false,
) {
    val scalarCount = value.codePointCount(0, value.length)
    require(scalarCount <= limit) { "$field exceeds $limit characters" }
    require(allowEmpty || value.isNotEmpty()) { "$field must not be empty" }
    require(value == value.trim()) { "$field must not contain surrounding whitespace" }
    require(value.none { it.code < 0x20 || it.code == 0x7f }) { "$field contains control characters" }
}

private fun requireTokens(
    values: List<String>,
    field: String,
    limit: Int,
    pattern: Regex? = null,
) {
    require(values.size <= limit) { "$field exceeds $limit entries" }
    require(values.distinct().size == values.size) { "$field contains duplicate entries" }
    values.forEach { value ->
        if (pattern != null) {
            require(pattern.matches(value)) { "invalid $field entry: $value" }
        } else {
            require(value.isNotEmpty()) { "invalid $field entry: $value" }
        }
    }
}
