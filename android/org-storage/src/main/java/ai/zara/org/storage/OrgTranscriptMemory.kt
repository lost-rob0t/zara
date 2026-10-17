package ai.zara.org.storage

import ai.zara.org.core.OrgTranscriptCapture
import ai.zara.org.core.TranscriptCapture
import ai.zara.org.core.TranscriptExtract

private val transcriptIngestionLock = Any()

fun OrgRepository.saveTranscriptCapture(
    capture: TranscriptCapture,
    extracts: List<TranscriptExtract> = emptyList(),
): String = OrgTranscriptMemory(
    listPaths = { listOrgFiles().map { it.relativePath } },
    read = { path -> read(listOrgFiles().single { it.relativePath == path }) },
    write = { path, text ->
        writeRelative(path, text)
        Unit
    },
).save(capture, extracts)

internal class OrgTranscriptMemory(
    private val listPaths: () -> List<String>,
    private val read: (String) -> String,
    private val write: (String, String) -> Unit,
) {
    fun save(capture: TranscriptCapture, extracts: List<TranscriptExtract> = emptyList()): String {
        val path = OrgTranscriptCapture.relativePath(capture.id)
        val document = OrgTranscriptCapture.render(capture, extracts)
        return synchronized(transcriptIngestionLock) {
            val paths = listPaths()
            val matches = paths.filter { it == path }
            check(matches.size <= 1) { "Ambiguous capture path; refresh the Org workspace" }
            if (matches.isNotEmpty()) {
                check(read(path) == document) { "Capture conflicts with existing content; nothing was overwritten" }
                return@synchronized path
            }
            check(paths.count { it.startsWith("memory/captures/") } < 128) {
                "Capture retention limit reached; review and delete old captures before saving more"
            }
            write(path, document)
            check(read(path) == document) { "Capture persistence could not be verified" }
            path
        }
    }
}
