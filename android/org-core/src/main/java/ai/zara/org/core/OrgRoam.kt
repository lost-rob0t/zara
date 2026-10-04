package ai.zara.org.core

data class OrgRoamLink(
    val sourceId: String,
    val targetId: String,
    val label: String?,
    val path: String,
    val line: Int,
)

data class OrgRoamNode(
    val id: String,
    val customId: String?,
    val path: String,
    val line: Int,
    val level: Int,
    val title: String,
    val aliases: Set<String>,
    val tags: Set<String>,
    val links: List<OrgRoamLink>,
)

data class OrgRoamGraph(
    val nodes: Map<String, OrgRoamNode>,
    val duplicateIds: Set<String>,
) {
    fun backlinks(id: String): List<OrgRoamLink> =
        nodes.values.asSequence()
            .flatMap { it.links.asSequence() }
            .filter { it.targetId == id }
            .sortedWith(compareBy<OrgRoamLink> { it.path }.thenBy { it.line })
            .toList()

    fun search(query: String): List<OrgRoamNode> {
        val needle = query.trim().lowercase()
        if (needle.isBlank()) return nodes.values.sortedBy { it.title.lowercase() }
        return nodes.values
            .filter { node ->
                node.id.lowercase().contains(needle) ||
                    node.title.lowercase().contains(needle) ||
                    node.aliases.any { it.lowercase().contains(needle) } ||
                    node.tags.any { it.lowercase().contains(needle) }
            }
            .sortedBy { it.title.lowercase() }
    }
}

object OrgRoam {
    private val heading = Regex("^(\\*+)\\s+(.*)$")
    private val property = Regex("^:([^:]+):\\s*(.*)$")
    private val trailingTags = Regex("\\s+:([A-Za-z0-9_@#%:.-]+):\\s*$")
    private val priority = Regex("^\\[#([A-Z])](?:\\s+|$)")
    private val idLink = Regex("\\[\\[id:([^]\\s]+)](?:\\[([^]]*)])?]")
    private val aliasToken = Regex("\"([^\"]+)\"|(\\S+)")

    fun build(documents: Map<String, String>): OrgRoamGraph {
        val candidates = mutableListOf<NodeCandidate>()

        documents.toSortedMap().forEach { (path, source) ->
            val lines = source.lines()
            val todoStates = OrgParser.todoStates(source)
            var index = 0
            while (index < lines.size) {
                val match = heading.matchEntire(lines[index])
                if (match == null) {
                    index += 1
                    continue
                }

                val level = match.groupValues[1].length
                val rawHeading = match.groupValues[2].trim()
                val next = nextSiblingBoundary(lines, index + 1, level)
                val metadata = headingMetadata(lines, index + 1, next)
                val canonicalId = metadata.id ?: metadata.customId
                if (canonicalId != null) {
                    val titleAndTags = normalizeHeading(rawHeading, todoStates)
                    candidates += NodeCandidate(
                        id = canonicalId,
                        customId = metadata.customId,
                        path = path,
                        line = index + 1,
                        level = level,
                        title = titleAndTags.first,
                        aliases = metadata.aliases,
                        tags = titleAndTags.second,
                        bodyStart = index + 1,
                        bodyEndExclusive = next,
                        lines = lines,
                    )
                }
                index += 1
            }
        }

        val duplicateIds = candidates.groupBy { it.id }
            .filterValues { it.size > 1 }
            .keys
            .toSet()

        val nodes = linkedMapOf<String, OrgRoamNode>()
        candidates.forEach { candidate ->
            if (candidate.id in duplicateIds || candidate.id in nodes) return@forEach
            val links = mutableListOf<OrgRoamLink>()
            for (lineIndex in candidate.bodyStart until candidate.bodyEndExclusive) {
                idLink.findAll(candidate.lines[lineIndex]).forEach { match ->
                    links += OrgRoamLink(
                        sourceId = candidate.id,
                        targetId = match.groupValues[1],
                        label = match.groupValues.getOrNull(2)?.takeIf { it.isNotBlank() },
                        path = candidate.path,
                        line = lineIndex + 1,
                    )
                }
            }
            nodes[candidate.id] = OrgRoamNode(
                id = candidate.id,
                customId = candidate.customId,
                path = candidate.path,
                line = candidate.line,
                level = candidate.level,
                title = candidate.title,
                aliases = candidate.aliases,
                tags = candidate.tags,
                links = links,
            )
        }

        return OrgRoamGraph(nodes = nodes, duplicateIds = duplicateIds)
    }

    private fun nextSiblingBoundary(lines: List<String>, start: Int, level: Int): Int {
        for (index in start until lines.size) {
            val match = heading.matchEntire(lines[index]) ?: continue
            if (match.groupValues[1].length <= level) return index
        }
        return lines.size
    }

    private fun headingMetadata(lines: List<String>, start: Int, endExclusive: Int): HeadingMetadata {
        var id: String? = null
        var customId: String? = null
        var aliases = emptySet<String>()

        for (index in start until endExclusive) {
            val trimmed = lines[index].trim()
            if (index > start && heading.matches(lines[index])) break
            val match = property.matchEntire(trimmed) ?: continue
            when (match.groupValues[1].uppercase()) {
                "ID" -> id = match.groupValues[2].trim().takeIf { it.isNotBlank() }
                "CUSTOM_ID" -> customId = match.groupValues[2].trim().takeIf { it.isNotBlank() }
                "ROAM_ALIASES" -> aliases = parseAliases(match.groupValues[2])
            }
        }

        return HeadingMetadata(id, customId, aliases)
    }

    private fun normalizeHeading(raw: String, todoStates: List<String>): Pair<String, Set<String>> {
        var text = raw
        val state = todoStates.firstOrNull { text == it || text.startsWith("$it ") }
        if (state != null) text = text.removePrefix(state).trimStart()
        priority.find(text)?.let { text = text.removeRange(it.range).trimStart() }

        var tags = emptySet<String>()
        trailingTags.find(text)?.let { match ->
            tags = match.groupValues[1].split(':').filter { it.isNotBlank() }.toSet()
            text = text.removeRange(match.range).trimEnd()
        }
        return text to tags
    }

    private fun parseAliases(raw: String): Set<String> =
        aliasToken.findAll(raw)
            .mapNotNull { match ->
                match.groupValues[1].ifBlank { match.groupValues[2] }.takeIf { it.isNotBlank() }
            }
            .toCollection(linkedSetOf())

    private data class HeadingMetadata(
        val id: String?,
        val customId: String?,
        val aliases: Set<String>,
    )

    private data class NodeCandidate(
        val id: String,
        val customId: String?,
        val path: String,
        val line: Int,
        val level: Int,
        val title: String,
        val aliases: Set<String>,
        val tags: Set<String>,
        val bodyStart: Int,
        val bodyEndExclusive: Int,
        val lines: List<String>,
    )
}
