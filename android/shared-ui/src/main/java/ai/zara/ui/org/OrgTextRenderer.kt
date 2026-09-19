package ai.zara.ui.org

import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp


data class OrgRenderConfig(
    val baseFontSp: Float = 16f,
    val headingScales: List<Float> = listOf(1.45f, 1.30f, 1.18f, 1.10f, 1.04f),
    val showBacklinks: Boolean = true,
    val showProperties: Boolean = true,
) {
    init {
        require(baseFontSp > 0f) { "baseFontSp must be positive" }
        require(headingScales.all { it > 0f }) { "headingScales must be positive" }
    }

    fun headingScale(level: Int): Float {
        val index = level.coerceAtLeast(1) - 1
        return headingScales.getOrNull(index) ?: 1f
    }
}

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

    fun headingFontSp(
        level: Int,
        baseFontSp: Float = 16f,
        headingScales: List<Float> = OrgRenderConfig().headingScales,
    ): Float {
        val config = OrgRenderConfig(baseFontSp = baseFontSp, headingScales = headingScales)
        return config.baseFontSp * config.headingScale(level)
    }

    fun render(
        document: OrgRenderDocument,
        baseFontSp: Float = 16f,
    ): OrgRenderedText = render(document, OrgRenderConfig(baseFontSp = baseFontSp))

    fun render(document: OrgRenderDocument, config: OrgRenderConfig): OrgRenderedText {
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
                if (config.showProperties) {
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
                }
                if (node.project != null && node.id != null && !config.showProperties) {
                    append("Project: ")
                    append(node.project)
                    append('\n')
                }
                if (config.showBacklinks && node.backlinks.isNotEmpty()) {
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
        return renderSource(source, config)
    }

    fun renderSource(source: String, baseFontSp: Float = 16f): OrgRenderedText =
        renderSource(source, OrgRenderConfig(baseFontSp = baseFontSp))

    fun renderSource(source: String, config: OrgRenderConfig): OrgRenderedText {
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
                            fontSize = (config.baseFontSp * config.headingScale(level)).sp,
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
