package ai.zara.org.storage

import ai.zara.org.core.*
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import org.junit.Assert.*
import org.junit.Test

class OrgTranscriptMemoryTest {
    private val capture = TranscriptCapture("memo-1", 1700000000000, listOf(TranscriptSpan("s1", 0, 1000, "Buy milk")))
    private val extract = TranscriptExtract(TranscriptExtractKind.TODO, "s1", 0, 8, "Buy milk")

    @Test
    fun persistsReopensAndReusesExistingOrgTodoAndSearchIndexes() = withDirectory { root ->
        val repository = FileRepository(root)
        val saved = repository.saveTranscriptCapture(capture, listOf(extract))
        assertEquals("memory/captures/memo-1.org", saved)
        val reopened = FileRepository(root)
        assertEquals(listOf("Buy milk"), reopened.allTasks().map { it.title })
        val documents = reopened.listOrgFiles().associate { it to reopened.read(it) }
        assertTrue(OrgRoam.build(documents).search("milk").isNotEmpty())
        assertEquals(saved, reopened.saveTranscriptCapture(capture, listOf(extract)))
        assertEquals(0, reopened.writes)
    }

    @Test
    fun retryRejectsDifferentCaptureOrUserEditedDocumentWithoutOverwrite() = withDirectory { root ->
        val repository = FileRepository(root)
        val saved = repository.saveTranscriptCapture(capture, listOf(extract))
        rejects { repository.saveTranscriptCapture(capture.copy(capturedAtEpochMs = 1), listOf(extract)) }
        rejects { repository.saveTranscriptCapture(capture, emptyList()) }
        repository.write(saved, repository.read(saved) + "\nUser annotation\n")
        val edited = repository.read(saved)
        rejects { repository.saveTranscriptCapture(capture, listOf(extract)) }
        assertEquals(edited, repository.read(saved))
    }

    @Test
    fun failureIsVisibleAndAnOfflineRetryCanSucceed() = withDirectory { root ->
        val repository = FileRepository(root)
        repository.failWrites = true
        rejects { repository.saveTranscriptCapture(capture, listOf(extract)) }
        assertTrue(repository.listOrgFiles().isEmpty())
        repository.failWrites = false
        repository.saveTranscriptCapture(capture, listOf(extract))
        assertEquals(1, repository.allTasks().size)
    }

    @Test
    fun concurrentIdenticalIngestsWriteOnceEvenAcrossRepositoryInstances() = withDirectory { root ->
        val first = FileRepository(root)
        val second = FileRepository(root)
        val executor = Executors.newFixedThreadPool(2)
        try {
            val tasks = listOf(first, second).map { repo -> Callable { repo.saveTranscriptCapture(capture, listOf(extract)) } }
            val files = executor.invokeAll(tasks).map { it.get() }
            assertEquals(1, files.distinct().size)
            assertEquals(1, first.writes + second.writes)
        } finally { executor.shutdownNow() }
    }

    @Test
    fun boundsRetainedCaptureCountAndStillAllowsIdenticalRetries() = withDirectory { root ->
        val repository = FileRepository(root)
        repeat(128) { repository.saveTranscriptCapture(capture.copy(id = "memo-$it"), emptyList()) }
        rejects { repository.saveTranscriptCapture(capture.copy(id = "overflow"), emptyList()) }
        repository.saveTranscriptCapture(capture.copy(id = "memo-0"), emptyList())
        assertEquals(128, repository.writes)
    }

    @Test
    fun partialWriteIsDetectedAndRetryDoesNotOverwriteUncertainContent() = withDirectory { root ->
        val repository = FileRepository(root)
        repository.truncateWrites = true
        rejects { repository.saveTranscriptCapture(capture, listOf(extract)) }
        val partial = repository.read(repository.listOrgFiles().single())
        repository.truncateWrites = false
        rejects { repository.saveTranscriptCapture(capture, listOf(extract)) }
        assertEquals(partial, repository.read(repository.listOrgFiles().single()))
        assertEquals(1, repository.writes)
    }

    private fun rejects(block: () -> Unit) {
        try { block(); fail("Expected ingestion failure") } catch (_: IllegalStateException) { }
    }

    private fun withDirectory(block: (Path) -> Unit) {
        val root = Files.createTempDirectory("transcript-memory-test")
        try { block(root) } finally { root.toFile().deleteRecursively() }
    }

    private class FileRepository(private val root: Path) {
        var writes = 0
        var failWrites = false
        var truncateWrites = false
        fun listOrgFiles(): List<String> = Files.walk(root).use { paths ->
            paths.filter { Files.isRegularFile(it) }.map { root.relativize(it).toString() }.toList()
        }
        fun read(path: String): String = Files.readString(root.resolve(path))
        fun write(path: String, text: String) { writeRelative(path, text) }
        fun writeRelative(relativePath: String, text: String) {
            check(!failWrites) { "Simulated unavailable local storage" }
            val path = root.resolve(relativePath)
            Files.createDirectories(path.parent)
            Files.writeString(path, if (truncateWrites) text.take(text.length / 2) else text)
            writes += 1
        }
        fun allTasks(): List<OrgTask> = listOrgFiles().flatMap { OrgParser.parse(read(it), it).tasks }
        fun saveTranscriptCapture(capture: TranscriptCapture, extracts: List<TranscriptExtract>): String =
            OrgTranscriptMemory(::listOrgFiles, ::read, ::writeRelative).save(capture, extracts)
    }
}
