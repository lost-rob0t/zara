package ai.zara.app.expert

private val activationIdPattern = Regex("^act:[a-f0-9]{32}$")
private val portableWirePattern = Regex("^[!-~]{1,512}$")

val ZARA_EXPERT_OPERATIONS: Set<String> = setOf(
    "expert.list",
    "expert.describe",
    "expert.match",
    "expert.activate",
    "expert.status",
    "expert.invoke",
    "expert.explain",
    "expert.cancel",
    "expert.deactivate",
)

data class ActivationHandle(
    val activationId: String,
    val principal: String,
    val workspace: String,
    val expertId: String,
    val expertVersion: String,
    val manifestDigest: String,
    val registryGeneration: Long,
    val runtimeGeneration: Long,
) {
    init {
        require(activationIdPattern.matches(activationId)) { "invalid activationId: $activationId" }
        requirePortable(principal, "principal", 192)
        requirePortable(workspace, "workspace", 192)
        requirePortable(expertId, "expertId", 192)
        requireBoundedPortableText(expertVersion, "expertVersion", 64)
        requirePortable(manifestDigest, "manifestDigest", 192)
        require(registryGeneration >= 0L) { "registryGeneration must be non-negative" }
        require(runtimeGeneration >= 0L) { "runtimeGeneration must be non-negative" }
    }
}

data class ExpertRequest(
    val requestId: String?,
    val operation: String,
    val activationId: String,
    val expertId: String,
    val expertOperation: String,
    val expectedRegistryGeneration: Long? = null,
    val expectedRuntimeGeneration: Long? = null,
    val input: Map<String, Any?> = emptyMap(),
    val limits: ExpertLimits? = null,
    val idempotencyKey: String? = null,
    val protocol: String = ZARA_EXPERT_PROTOCOL,
) {
    init {
        require(protocol == ZARA_EXPERT_PROTOCOL) {
            "request protocol $protocol is not $ZARA_EXPERT_PROTOCOL"
        }
        require(operation in ZARA_EXPERT_OPERATIONS) { "unknown expert operation: $operation" }
        requestId?.let { requirePortable(it, "requestId", 128) }
        require(activationIdPattern.matches(activationId)) { "invalid activationId: $activationId" }
        requirePortable(expertId, "expertId", 128)
        requirePortable(expertOperation, "expertOperation", 128)
        expectedRegistryGeneration?.let {
            require(it >= 0L) { "expectedRegistryGeneration must be non-negative" }
        }
        expectedRuntimeGeneration?.let {
            require(it >= 0L) { "expectedRuntimeGeneration must be non-negative" }
        }
        idempotencyKey?.let { requirePortable(it, "idempotencyKey", 128) }
        requireBoundedPayload(input, "input")
    }
}

data class ExpertResult(
    val protocol: String,
    val requestId: String,
    val invocationId: String,
    val activationId: String,
    val expertId: String,
    val expertVersion: String,
    val manifestDigest: String,
    val expertOperation: String,
    val resolvedRegistryGeneration: Long,
    val resolvedRuntimeGeneration: Long,
    val verdict: ExpertVerdict,
    val data: Map<String, Any?> = emptyMap(),
    val evidenceRefs: List<String> = emptyList(),
    val usage: Map<String, Any?> = emptyMap(),
    val effectReceipts: List<Map<String, Any?>> = emptyList(),
    val errorCode: ExpertErrorCode? = null,
    val errorMessage: String = "",
    val replayed: Boolean = false,
) {
    init {
        require(protocol == ZARA_EXPERT_PROTOCOL) {
            "result protocol $protocol is not $ZARA_EXPERT_PROTOCOL"
        }
        requirePortable(requestId, "requestId", 128)
        requirePortable(invocationId, "invocationId", 128)
        require(activationIdPattern.matches(activationId)) { "invalid activationId: $activationId" }
        requirePortable(expertId, "expertId", 128)
        requireBoundedPortableText(expertVersion, "expertVersion", 64)
        requirePortable(manifestDigest, "manifestDigest", 256)
        requirePortable(expertOperation, "expertOperation", 128)
        require(resolvedRegistryGeneration >= 0L) { "resolvedRegistryGeneration must be non-negative" }
        require(resolvedRuntimeGeneration >= 0L) { "resolvedRuntimeGeneration must be non-negative" }
        require(evidenceRefs.size <= MAX_EVIDENCE_REFS) { "evidenceRefs exceeds $MAX_EVIDENCE_REFS entries" }
        require(evidenceRefs.distinct().size == evidenceRefs.size) { "evidenceRefs contains duplicate entries" }
        evidenceRefs.forEach { requirePortable(it, "evidenceRef", 128) }
        require(effectReceipts.size <= MAX_EFFECT_RECEIPTS) {
            "effectReceipts exceeds $MAX_EFFECT_RECEIPTS entries"
        }
        requireBoundedPayload(data, "data")
        requireBoundedPayload(usage, "usage")
        effectReceipts.forEachIndexed { index, receipt ->
            requireBoundedPayload(receipt, "effectReceipt[$index]")
        }
        requireBoundedPortableText(errorMessage, "errorMessage", 512, allowEmpty = true)
    }
}

private fun requirePortable(value: String, field: String, limit: Int) {
    require(value.length <= limit && portableWirePattern.matches(value)) {
        "$field is not a bounded portable wire value"
    }
}

private fun requireBoundedPortableText(
    value: String,
    field: String,
    limit: Int,
    allowEmpty: Boolean = false,
) {
    require(value.length <= limit) { "$field exceeds $limit characters" }
    require(allowEmpty || value.isNotEmpty()) { "$field must not be empty" }
    require(value == value.trim()) { "$field must not contain surrounding whitespace" }
    require(value.none { it.code < 0x20 || it.code == 0x7f }) { "$field contains control characters" }
}

private fun requireBoundedPayload(value: Any?, field: String, depth: Int = 0) {
    require(depth <= MAX_PAYLOAD_DEPTH) { "$field exceeds bounded depth" }
    when (value) {
        null, is Boolean, is Int, is Long -> Unit
        is Float -> require(value.isFinite()) { "$field contains non-finite number" }
        is Double -> require(value.isFinite()) { "$field contains non-finite number" }
        is String -> require(value.length <= MAX_PAYLOAD_STRING) { "$field contains oversized string" }
        is List<*> -> {
            require(value.size <= MAX_PAYLOAD_LIST) { "$field contains oversized list" }
            value.forEachIndexed { index, item -> requireBoundedPayload(item, "$field[$index]", depth + 1) }
        }
        is Map<*, *> -> {
            require(value.size <= MAX_PAYLOAD_KEYS) { "$field contains oversized object" }
            value.forEach { (key, item) ->
                require(key is String && key.length <= 64) { "$field contains invalid object key" }
                requireBoundedPayload(item, "$field.$key", depth + 1)
            }
        }
        else -> throw IllegalArgumentException("$field contains unsupported value type")
    }
}

private const val MAX_EVIDENCE_REFS = 32
private const val MAX_EFFECT_RECEIPTS = 32
private const val MAX_PAYLOAD_DEPTH = 8
private const val MAX_PAYLOAD_KEYS = 32
private const val MAX_PAYLOAD_LIST = 64
private const val MAX_PAYLOAD_STRING = 4096
