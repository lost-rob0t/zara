package ai.zara.org.core

import ai.zara.editor.core.OperationFence
import ai.zara.editor.core.OperationToken
import java.util.UUID

enum class OrgExecutionStatus { SUCCEEDED, FAILED, CANCELLED }

data class OrgExecutionRequest(
    val requestId: String = UUID.randomUUID().toString(),
    val documentId: String,
    val sourceRevision: Long,
    val blockId: String,
    val blockHash: String,
    val language: String,
    val body: String,
    val headerArgs: Map<String, String>,
    val principal: String,
    val deadlineEpochMs: Long? = null,
)

data class OrgExecutionResult(
    val requestId: String,
    val sourceRevision: Long,
    val blockHash: String,
    val status: OrgExecutionStatus,
    val stdout: String = "",
    val stderr: String = "",
    val structuredValue: String? = null,
    val durationMs: Long = 0,
    val runtimeGeneration: Long = 0,
)

interface OrgExecutionProvider {
    val language: String
    val supportedHeaderArgs: Set<String>

    /** Invoked only after an explicit run action; parsing and rendering never call this. */
    fun run(request: OrgExecutionRequest): OrgExecutionResult
}

sealed interface OrgExecutionPrepareResult {
    data class Ready(
        val request: OrgExecutionRequest,
        val provider: OrgExecutionProvider,
    ) : OrgExecutionPrepareResult

    data class Rejected(val reason: String) : OrgExecutionPrepareResult
}

class OrgExecutionRegistry(providers: List<OrgExecutionProvider>) {
    private val providersByLanguage = providers.associateBy { it.language.lowercase() }

    fun prepareExplicitRun(
        source: String,
        documentId: String,
        sourceRevision: Long,
        blockId: String,
        principal: String,
        deadlineEpochMs: Long? = null,
    ): OrgExecutionPrepareResult {
        val block = OrgNotebookBlocks.scan(source, documentId).firstOrNull { it.id == blockId }
            ?: return OrgExecutionPrepareResult.Rejected("source block no longer exists")
        val provider = providersByLanguage[block.language]
            ?: return OrgExecutionPrepareResult.Rejected("no execution provider for ${block.language}")
        val unsupported = block.headers.keys - provider.supportedHeaderArgs - NON_EXECUTION_HEADERS
        if (unsupported.isNotEmpty()) {
            return OrgExecutionPrepareResult.Rejected(
                "unsupported header args for ${block.language}: ${unsupported.sorted().joinToString()}",
            )
        }
        return OrgExecutionPrepareResult.Ready(
            request = OrgExecutionRequest(
                documentId = documentId,
                sourceRevision = sourceRevision,
                blockId = block.id,
                blockHash = block.hash,
                language = block.language,
                body = block.body,
                headerArgs = block.headers,
                principal = principal,
                deadlineEpochMs = deadlineEpochMs,
            ),
            provider = provider,
        )
    }

    companion object {
        private val NON_EXECUTION_HEADERS = setOf("tangle", "exports", "comments")
    }
}

data class OrgExecutionToken(
    val editorToken: OperationToken,
    val blockHash: String,
) {
    val generation: Long get() = editorToken.generation
    val sourceRevision: Long get() = editorToken.baseRevision
}

/**
 * Org-specific block-hash fence layered on the canonical editor operation fence.
 * Document revision/generation authority stays in editor-core; this layer only adds
 * source-block identity so notebook execution cannot invent a second revision model.
 */
class OrgExecutionFence {
    private val editorFence = OperationFence()

    fun begin(sourceRevision: Long, blockHash: String): OrgExecutionToken =
        OrgExecutionToken(editorFence.begin(sourceRevision), blockHash)

    fun cancel() {
        editorFence.cancel()
    }

    fun accepts(token: OrgExecutionToken, sourceRevision: Long, blockHash: String): Boolean =
        editorFence.accepts(token.editorToken, sourceRevision) && token.blockHash == blockHash
}
