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
        require(status.isNotBlank()) { "Org allocation status must not be blank" }
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
        require(allocations.map(OrgScheduleAllocation::id).distinct().size == allocations.size) {
            "Org schedule allocation ids must be unique"
        }
        require(currentOrNextTitle == null || currentOrNextTitle.isNotBlank()) {
            "currentOrNextTitle must be null or non-blank"
        }
    }

    companion object {
        const val MAX_LANES = 6
        val EMPTY = OrgScheduleSnapshot(0L, emptyList(), null)
    }
}

internal fun shouldAcceptOrgScheduleSnapshot(
    current: OrgScheduleSnapshot,
    incoming: OrgScheduleSnapshot,
): Boolean {
    if (incoming.generatedAtEpochMillis > current.generatedAtEpochMillis) return true
    if (incoming.generatedAtEpochMillis < current.generatedAtEpochMillis) return false
    return incoming == current
}

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

        var generatedAt: Long? = null
        var nextTitle: String? = null
        var nextSeen = false
        val allocations = mutableListOf<OrgScheduleAllocation>()
        val ids = mutableSetOf<String>()

        for (line in lines.drop(1)) {
            when {
                line.startsWith("generated=") -> {
                    if (generatedAt != null) return null
                    generatedAt = line.removePrefix("generated=").toLongOrNull() ?: return null
                }
                line.startsWith("next=") -> {
                    if (nextSeen) return null
                    nextSeen = true
                    val decoded = decodeEscaped(line.removePrefix("next=")) ?: return null
                    if (decoded.isBlank()) return null
                    nextTitle = decoded
                }
                line.startsWith("allocation=") -> {
                    if (allocations.size >= OrgScheduleSnapshot.MAX_LANES) return null
                    val fields = line.removePrefix("allocation=").split('|')
                    if (fields.size != 8) return null
                    val id = decodeEscaped(fields[0]) ?: return null
                    if (!ids.add(id)) return null
                    val title = decodeEscaped(fields[1]) ?: return null
                    val status = decodeEscaped(fields[2]) ?: return null
                    val start = fields[3].toIntOrNull() ?: return null
                    val end = fields[4].toIntOrNull() ?: return null
                    val priorityRaw = decodeEscaped(fields[5]) ?: return null
                    val tagsRaw = decodeEscaped(fields[6]) ?: return null
                    val sourceRaw = decodeEscaped(fields[7]) ?: return null
                    val allocation = runCatching {
                        OrgScheduleAllocation(
                            id = id,
                            title = title,
                            status = status,
                            startMinute = start,
                            endMinute = end,
                            priority = priorityRaw.ifBlank { null },
                            tags = tagsRaw.split(',').filter { it.isNotBlank() },
                            source = sourceRaw.ifBlank { null },
                        )
                    }.getOrNull() ?: return null
                    allocations += allocation
                }
                else -> return null
            }
        }

        val generated = generatedAt ?: return null
        return runCatching {
            OrgScheduleSnapshot(generated, allocations, nextTitle)
        }.getOrNull()
    }

    private fun escape(value: String): String = URLEncoder.encode(value, StandardCharsets.UTF_8.name())

    private fun decodeEscaped(value: String): String? = runCatching {
        URLDecoder.decode(value, StandardCharsets.UTF_8.name())
    }.getOrNull()
}
