package ai.zara.org.core

data class OrgPageBlock(
    val depth: Int,
    val heading: String,
    val body: String,
    val start: Int,
    val end: Int,
    val raw: String,
)

data class OrgPageDocument(
    val title: String?,
    val source: String,
    val blocks: List<OrgPageBlock>,
)

object OrgPageParser {
    private val heading = Regex("^(\\*+)\\s+(.*)$")
    private val title = Regex("(?i)^#\\+title:\\s*(.*)$")
    private val sourceBegin = Regex("(?i)^#\\+begin_(?:src|example)(?:\\s+.*)?$")
    private val sourceEnd = Regex("(?i)^#\\+end_(?:src|example)\\s*$")

    fun parse(source: String): OrgPageDocument {
        val lines = sourceLines(source)
        val headings = mutableListOf<HeadingRange>()
        var documentTitle: String? = null
        var inSourceBlock = false

        lines.forEach { line ->
            title.matchEntire(line.text)?.let { match ->
                if (!inSourceBlock) documentTitle = match.groupValues[1].trim()
            }
            if (sourceEnd.matches(line.text)) {
                inSourceBlock = false
                return@forEach
            }
            if (inSourceBlock) return@forEach
            if (sourceBegin.matches(line.text)) {
                inSourceBlock = true
                return@forEach
            }
            heading.matchEntire(line.text)?.let { match ->
                headings += HeadingRange(
                    depth = match.groupValues[1].length,
                    heading = match.groupValues[2],
                    start = line.start,
                    bodyStart = line.end,
                )
            }
        }

        val blocks = headings.mapIndexed { index, item ->
            val end = headings.getOrNull(index + 1)?.start ?: source.length
            OrgPageBlock(
                depth = item.depth,
                heading = item.heading,
                body = source.substring(item.bodyStart, end).trimEnd('\r', '\n'),
                start = item.start,
                end = end,
                raw = source.substring(item.start, end),
            )
        }
        return OrgPageDocument(documentTitle, source, blocks)
    }

    fun replaceBlock(source: String, block: OrgPageBlock, replacement: String): String {
        require(block.start in 0..source.length && block.end in block.start..source.length) {
            "Block range is outside the source"
        }
        require(source.substring(block.start, block.end) == block.raw) {
            "Block source changed; replacement refused"
        }
        return source.replaceRange(block.start, block.end, replacement)
    }

    private fun sourceLines(source: String): List<SourceLine> {
        val lines = mutableListOf<SourceLine>()
        var start = 0
        while (start < source.length) {
            var contentEnd = start
            while (contentEnd < source.length && source[contentEnd] != '\n' && source[contentEnd] != '\r') {
                contentEnd += 1
            }
            var end = contentEnd
            if (end < source.length && source[end] == '\r') end += 1
            if (end < source.length && source[end] == '\n') end += 1
            lines += SourceLine(start, end, source.substring(start, contentEnd))
            start = end
        }
        if (source.isEmpty()) lines += SourceLine(0, 0, "")
        return lines
    }

    private data class SourceLine(val start: Int, val end: Int, val text: String)
    private data class HeadingRange(val depth: Int, val heading: String, val start: Int, val bodyStart: Int)
}

data class OrgPolicyConfig(val values: Map<String, String>) {
    val permitsUnapprovedEffects: Boolean = false
}

object OrgAppPolicy {
    private val fact = Regex("^app_policy\\(([a-z_]+),\\s*([a-z_]+)\\)\\.$")
    private val allowed = mapOf(
        "appearance_theme" to setOf("outrun", "starintel", "midnight", "terminal", "light", "system"),
        "appearance_density" to setOf("compact", "comfortable"),
        "editor_mode" to setOf("blocks", "raw"),
        "save_mode" to setOf("explicit", "on_blur"),
        "sync_mode" to setOf("manual", "automatic", "off"),
        "automation_policy" to setOf("approval_required", "disabled"),
        "plugin_policy" to setOf("approval_required", "disabled"),
        "runtime_mode" to setOf("local", "remote", "auto"),
    )
    private val defaults = mapOf(
        "appearance_theme" to "outrun",
        "appearance_density" to "comfortable",
        "editor_mode" to "blocks",
        "save_mode" to "explicit",
        "sync_mode" to "manual",
        "automation_policy" to "approval_required",
        "plugin_policy" to "approval_required",
        "runtime_mode" to "local",
    )

    val defaultSource: String = defaults.entries.joinToString("\n", postfix = "\n") { (key, value) ->
        "app_policy($key, $value)."
    }

    fun parse(source: String): OrgPolicyConfig {
        val values = defaults.toMutableMap()
        val seen = mutableSetOf<String>()
        source.lineSequence().forEachIndexed { index, rawLine ->
            val line = rawLine.substringBefore('%').trim()
            if (line.isEmpty()) return@forEachIndexed
            val match = fact.matchEntire(line)
                ?: throw IllegalArgumentException("config.pl line ${index + 1} must be an app_policy/2 fact")
            val key = match.groupValues[1]
            val value = match.groupValues[2]
            val allowedValues = allowed[key]
                ?: throw IllegalArgumentException("config.pl line ${index + 1} has unknown policy $key")
            require(value in allowedValues) {
                "config.pl line ${index + 1} has unsupported $key value $value"
            }
            require(seen.add(key)) { "config.pl line ${index + 1} duplicates policy $key" }
            values[key] = value
        }
        return OrgPolicyConfig(values)
    }
}
