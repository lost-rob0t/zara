package ai.zara.org.core

import ai.zara.editor.core.RevisionedEditorBuffer
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgNotebookTest {
    private class RecordingProvider(
        override val language: String = "python",
        override val supportedHeaderArgs: Set<String> = setOf("results"),
    ) : OrgExecutionProvider {
        var runs = 0

        override fun run(request: OrgExecutionRequest): OrgExecutionResult {
            runs += 1
            return OrgExecutionResult(
                requestId = request.requestId,
                sourceRevision = request.sourceRevision,
                blockHash = request.blockHash,
                status = OrgExecutionStatus.SUCCEEDED,
                stdout = "hello\nworld",
                runtimeGeneration = 3,
            )
        }
    }

    private val source = """
        #+name: hello
        #+begin_src python :results replace
        print("hello")
        #+end_src
    """.trimIndent()

    @Test
    fun scanningAndPreparingNeverExecuteCode() {
        val provider = RecordingProvider()
        val registry = OrgExecutionRegistry(listOf(provider))
        val block = OrgNotebookBlocks.scan(source, "notes/demo.org").single()

        val prepared = registry.prepareExplicitRun(
            source = source,
            documentId = "notes/demo.org",
            sourceRevision = 8,
            blockId = block.id,
            principal = "local-user",
        )

        assertTrue(prepared is OrgExecutionPrepareResult.Ready)
        assertEquals(0, provider.runs)
    }

    @Test
    fun explicitProviderRunExecutesExactlyOnce() {
        val provider = RecordingProvider()
        val registry = OrgExecutionRegistry(listOf(provider))
        val block = OrgNotebookBlocks.scan(source).single()
        val prepared = registry.prepareExplicitRun(
            source = source,
            documentId = "demo.org",
            sourceRevision = 2,
            blockId = block.id,
            principal = "local-user",
        ) as OrgExecutionPrepareResult.Ready

        val result = prepared.provider.run(prepared.request)

        assertEquals(1, provider.runs)
        assertEquals(OrgExecutionStatus.SUCCEEDED, result.status)
    }

    @Test
    fun namedBlockHasStableIdentityAcrossEarlierTextChanges() {
        val original = OrgNotebookBlocks.scan(source).single()
        val shifted = OrgNotebookBlocks.scan("* Notes\nbody\n$source").single()

        assertEquals("name:hello", original.id)
        assertEquals(original.id, shifted.id)
        assertEquals(original.hash, shifted.hash)
        assertFalse(original.beginLine == shifted.beginLine)
    }

    @Test
    fun staleResultCannotOverwriteNewerDocumentRevision() {
        val provider = RecordingProvider()
        val block = OrgNotebookBlocks.scan(source).single()
        val prepared = OrgExecutionRegistry(listOf(provider)).prepareExplicitRun(
            source = source,
            documentId = "demo.org",
            sourceRevision = 4,
            blockId = block.id,
            principal = "local-user",
        ) as OrgExecutionPrepareResult.Ready
        val result = provider.run(prepared.request)

        val applied = OrgNotebookResults.apply(
            source = source + "\nnewer user edit",
            currentRevision = 5,
            request = prepared.request,
            result = result,
        )

        assertTrue(applied is OrgResultApplyResult.Stale)
    }

    @Test
    fun successfulResultIsInsertedAsCanonicalOrgText() {
        val provider = RecordingProvider()
        val block = OrgNotebookBlocks.scan(source).single()
        val prepared = OrgExecutionRegistry(listOf(provider)).prepareExplicitRun(
            source = source,
            documentId = "demo.org",
            sourceRevision = 4,
            blockId = block.id,
            principal = "local-user",
        ) as OrgExecutionPrepareResult.Ready
        val result = provider.run(prepared.request)

        val applied = OrgNotebookResults.apply(source, 4, prepared.request, result) as OrgResultApplyResult.Applied

        assertTrue(applied.source.contains("#+RESULTS:\n: hello\n: world"))
        assertTrue(applied.source.contains("print(\"hello\")"))
        assertEquals(5L, applied.nextRevision)
    }

    @Test
    fun existingResultIsReplacedNotDuplicated() {
        val withResult = source + "\n#+RESULTS:\n: old"
        val provider = RecordingProvider()
        val block = OrgNotebookBlocks.scan(withResult).single()
        val prepared = OrgExecutionRegistry(listOf(provider)).prepareExplicitRun(
            source = withResult,
            documentId = "demo.org",
            sourceRevision = 9,
            blockId = block.id,
            principal = "local-user",
        ) as OrgExecutionPrepareResult.Ready
        val result = provider.run(prepared.request)

        val applied = OrgNotebookResults.apply(withResult, 9, prepared.request, result) as OrgResultApplyResult.Applied

        assertEquals(1, Regex("(?i)#\\+RESULTS:").findAll(applied.source).count())
        assertFalse(applied.source.contains(": old"))
        assertTrue(applied.source.contains(": hello"))
    }

    @Test
    fun unsupportedHeaderArgIsVisibleAndDoesNotRun() {
        val provider = RecordingProvider()
        val unsafe = """
            #+begin_src python :results replace :session hidden
            print(1)
            #+end_src
        """.trimIndent()
        val block = OrgNotebookBlocks.scan(unsafe).single()

        val prepared = OrgExecutionRegistry(listOf(provider)).prepareExplicitRun(
            source = unsafe,
            documentId = "demo.org",
            sourceRevision = 1,
            blockId = block.id,
            principal = "local-user",
        )

        assertTrue(prepared is OrgExecutionPrepareResult.Rejected)
        assertEquals(0, provider.runs)
    }

    @Test
    fun cancellationFencesLateRuntimeReply() {
        val block = OrgNotebookBlocks.scan(source).single()
        val fence = OrgExecutionFence()
        val token = fence.begin(sourceRevision = 3, blockHash = block.hash)
        assertTrue(fence.accepts(token, 3, block.hash))

        fence.cancel()

        assertFalse(fence.accepts(token, 3, block.hash))
    }

    @Test
    fun editorRevisionChangeFencesLateNotebookReply() {
        val block = OrgNotebookBlocks.scan(source).single()
        val editor = RevisionedEditorBuffer(initialText = source, languageId = "org")
        val fence = OrgExecutionFence()
        val token = fence.begin(editor.snapshot().revision, block.hash)

        editor.replaceFromUser(source, newCursor = 0)

        assertFalse(fence.accepts(token, editor.snapshot().revision, block.hash))
    }
}
