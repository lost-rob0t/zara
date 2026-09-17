package ai.zara.ui.org

import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp


data class OrgRenderNode(
    val id: String? = null,
    val level: Int,
    val title: String,
    val todo: String? = null,
    val tags: List<String> = emptyList(),
    val project: String? = null,
    val body: String = "",
    val backlinks: List<String> = emptyList(),
    val memoryKinds: List<String> = emptyList(),
)

data class OrgRenderDocument(
    val title: String,
    val nodes: List<OrgRenderNode>,
)

data class OrgRenderedText(val annotated: AnnotatedString) {
    val text: String
        get() = annotated.text
}

object OrgTextRenderer {
    private val heading = Regex("^(\\*+)\\s+.*$")

    fun headingFontSp(level: Int, baseFontSp: Float = 16f): Float {
        val ratio = when (level.coerceAtLeast(0)) {
            0 -> 1.55f
            1 -> 1.45f
            2 -> 1.30f
            3 -> 1.18f
            4 -> 1.10f
            5 -> 1.04f
            else -> 1f
        }
        return baseFontSp * ratio
    }

    fun render(document: OrgRenderDocument, baseFontSp: Float = 16f): OrgRenderedText {
        val source = buildString {
            append("#+title: ")
            append(document.title)
            append('\n')
            document.nodes.forEachIndexed { index, node ->
                if (index > 0) append('\n')
                append("*".repeat(node.level.coerceAtLeast(1)))
                append(' ')
                node.todo?.takeIf { it.isNotBlank() }?.let {
                    append(it)
                    append(' ')
                }
                append(node.title)
                if (node.tags.isNotEmpty()) {
                    append(" :")
                    append(node.tags.joinToString(":"))
                    append(':')
                }
                append('\n')
                node.id?.takeIf { it.isNotBlank() }?.let {
                    append(":PROPERTIES:\n:ID: ")
                    append(it)
                    append('\n')
                    node.project?.takeIf { project -> project.isNotBlank() }?.let { project ->
                        append(":PROJECT: ")
                        append(project)
                        append('\n')
                    }
                    append(":END:\n")
                } ?: node.project?.takeIf { it.isNotBlank() }?.let {
                    append("Project: ")
                    append(it)
                    append('\n')
                }
                if (node.project != null && node.id != null) {
                    append("Project: ")
                    append(node.project)
                    append('\n')
                }
                if (node.backlinks.isNotEmpty()) {
                    append("Backlinks: ")
                    append(node.backlinks.joinToString(", "))
                    append('\n')
                }
                if (node.memoryKinds.isNotEmpty()) {
                    append("Memory: ")
                    append(node.memoryKinds.joinToString(", "))
                    append('\n')
                }
                if (node.body.isNotBlank()) {
                    append(node.body.trimEnd())
                    append('\n')
                }
            }
        }
        return renderSource(source, baseFontSp)
    }

    fun renderSource(source: String, baseFontSp: Float = 16f): OrgRenderedText {
        val annotated = buildAnnotatedString {
            val lines = source.split('\n')
            lines.forEachIndexed { index, line ->
                val start = length
                append(line)
                val match = heading.matchEntire(line)
                if (match != null) {
                    val level = match.groupValues[1].length
                    addStyle(
                        SpanStyle(
                            fontSize = headingFontSp(level, baseFontSp).sp,
                            fontWeight = FontWeight.SemiBold,
                        ),
                        start,
                        length,
                    )
                }
                if (index != lines.lastIndex) append('\n')
            }
        }
        return OrgRenderedText(annotated)
    }
}
