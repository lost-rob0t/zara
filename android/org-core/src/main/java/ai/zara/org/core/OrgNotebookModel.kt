package ai.zara.org.core

import java.security.MessageDigest

/** Source-preserving source-block view used by the Android notebook. */
data class OrgNotebookBlock(
    val id: String,
    val hash: String,
    val name: String?,
    val language: String,
    val headers: Map<String, String>,
    val body: String,
    val beginLine: Int,
    val endLine: Int,
)

object OrgNotebookBlocks {
    private val nameLine = Regex("(?i)^#\\+name:\\s*(\\S.*?)\\s*$")

    fun scan(source: String, path: String = ""): List<OrgNotebookBlock> {
        val lines = source.lines()
        val occurrences = mutableMapOf<String, Int>()
        return OrgParser.parse(source, path).sourceBlocks.map { block ->
            val beginIndex = block.startLine - 1
            val name = findName(lines, beginIndex)
            val hash = blockHash(block.language, block.headers, block.body)
            val occurrence = occurrences.getOrDefault(hash, 0)
            occurrences[hash] = occurrence + 1
            val bodyLineCount = if (block.body.isEmpty()) 0 else block.body.lines().size
            OrgNotebookBlock(
                id = name?.let { "name:$it" } ?: "sha256:${hash.take(24)}:$occurrence",
                hash = hash,
                name = name,
                language = block.language,
                headers = block.headers,
                body = block.body,
                beginLine = block.startLine,
                endLine = block.startLine + bodyLineCount + 1,
            )
        }
    }

    fun blockHash(language: String, headers: Map<String, String>, body: String): String {
        val canonicalHeaders = headers.toSortedMap().entries.joinToString("\n") { "${it.key}=${it.value}" }
        val bytes = "$language\n$canonicalHeaders\n$body".toByteArray(Charsets.UTF_8)
        return MessageDigest.getInstance("SHA-256").digest(bytes).joinToString("") { "%02x".format(it) }
    }

    private fun findName(lines: List<String>, beginIndex: Int): String? {
        var index = beginIndex - 1
        while (index >= 0 && lines[index].trim().startsWith("#+header:", ignoreCase = true)) index -= 1
        if (index < 0) return null
        return nameLine.matchEntire(lines[index].trim())?.groupValues?.get(1)?.trim()
    }
}
