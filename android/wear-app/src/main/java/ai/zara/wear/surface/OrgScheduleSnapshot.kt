package ai.zara.wear.surface

import java.net.URLDecoder
import java.net.URLEncoder
import java.nio.charset.StandardCharsets

data class OrgScheduleAllocation(
    val id: String,
    val title: String,
    val status: String,
    val startMinute: Int,
    val endMinute: Int,
    val priority: String?,
    val tags: List<String>,
    val source: String?,
) {
    init {
        require(id.isNotBlank()) { "Org allocation id must not be blank" }
        require(title.isNotBlank()) { "Org allocation title must not be blank" }
        require(startMinute in 0 until HALF_DAY_MINUTES) { "startMinute must be in [0, 720)" }
        require(endMinute in 1..HALF_DAY_MINUTES) { "endMinute must be in (0, 720]" }
        require(endMinute > startMinute) { "allocation must have positive duration" }
    }

    companion object {
        const val HALF_DAY_MINUTES = 12 * 60
    }
}

data class OrgScheduleSnapshot(
    val generatedAtEpochMillis: Long,
    val allocations: List<OrgScheduleAllocation>,
    val currentOrNextTitle: String?,
) {
    init {
        require(generatedAtEpochMillis >= 0L) { "generatedAtEpochMillis must not be negative" }
        require(allocations.size <= MAX_LANES) { "Org schedule snapshot supports at most $MAX_LANES lanes" }
    }

    companion object {
        const val MAX_LANES = 6
        val EMPTY = OrgScheduleSnapshot(0L, emptyList(), null)
    }
}

/**
 * Stable, dependency-free wire format shared by Wear cache tests and the future
 * Zara transport/Data-Layer adapter. Values are URL encoded so Org titles/tags
 * cannot corrupt delimiters.
 */
object OrgScheduleSnapshotCodec {
    const val SCHEMA = 1

    fun encode(snapshot: OrgScheduleSnapshot): String = buildString {
        append("schema=").append(SCHEMA).append('\n')
        append("generated=").append(snapshot.generatedAtEpochMillis).append('\n')
        snapshot.currentOrNextTitle?.let { append("next=").append(escape(it)).append('\n') }
        snapshot.allocations.forEach { allocation ->
            append("allocation=")
            append(escape(allocation.id)).append('|')
            append(escape(allocation.title)).append('|')
            append(escape(allocation.status)).append('|')
            append(allocation.startMinute).append('|')
            append(allocation.endMinute).append('|')
            append(escape(allocation.priority.orEmpty())).append('|')
            append(escape(allocation.tags.joinToString(","))).append('|')
            append(escape(allocation.source.orEmpty())).append('\n')
        }
    }

    fun decode(raw: String): OrgScheduleSnapshot? {
        if (raw.isBlank()) return null
        val lines = raw.lineSequence().filter { it.isNotBlank() }.toList()
        if (lines.firstOrNull() != "schema=$SCHEMA") return null

        var generatedAt = 0L
        var nextTitle: String? = null
        val allocations = mutableListOf<OrgScheduleAllocation>()

        for (line in lines.drop(1)) {
            when {
                line.startsWith("generated=") -> {
                    generatedAt = line.removePrefix("generated=").toLongOrNull() ?: return null
                }
                line.startsWith("next=") -> {
                    nextTitle = unescape(line.removePrefix("next=")).ifBlank { null }
                }
                line.startsWith("allocation=") -> {
                    val fields = line.removePrefix("allocation=").split('|')
                    if (fields.size != 8) return null
                    val start = fields[3].toIntOrNull() ?: return null
                    val end = fields[4].toIntOrNull() ?: return null
                    val allocation = runCatching {
                        OrgScheduleAllocation(
                            id = unescape(fields[0]),
                            title = unescape(fields[1]),
                            status = unescape(fields[2]),
                            startMinute = start,
                            endMinute = end,
                            priority = unescape(fields[5]).ifBlank { null },
                            tags = unescape(fields[6]).split(',').filter { it.isNotBlank() },
                            source = unescape(fields[7]).ifBlank { null },
                        )
                    }.getOrNull() ?: return null
                    allocations += allocation
                }
            }
        }

        if (allocations.size > OrgScheduleSnapshot.MAX_LANES) return null
        return OrgScheduleSnapshot(generatedAt, allocations, nextTitle)
    }

    private fun escape(value: String): String = URLEncoder.encode(value, StandardCharsets.UTF_8.name())

    private fun unescape(value: String): String = URLDecoder.decode(value, StandardCharsets.UTF_8.name())
}
