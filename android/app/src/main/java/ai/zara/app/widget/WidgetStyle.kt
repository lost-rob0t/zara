package ai.zara.app.widget

import ai.zara.app.ui.AppRoute
import ai.zara.ui.theme.ZaraTheme
import java.io.File
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

enum class WidgetKind(val atom: String) {
    ASSISTANT("assistant"),
    RUNTIME("runtime"),
    ACTIONS("actions"),
}

enum class WidgetRoute(val atom: String, val appRoute: AppRoute) {
    CHAT("chat", AppRoute.Chat),
    VOICE("voice", AppRoute.Voice),
    LOGIC("logic", AppRoute.Logic),
    PROJECTS("projects", AppRoute.Projects),
    SCHEDULED("scheduled", AppRoute.Scheduled),
    RUNTIME("runtime", AppRoute.Runtime),
    CONNECTION("connection", AppRoute.Connection),
    PERMISSIONS("permissions", AppRoute.Permissions),
    APPEARANCE("appearance", AppRoute.Appearance),
    PLUGINS("plugins", AppRoute.Plugins),
    UPDATES("updates", AppRoute.Updates),
    DIAGNOSTICS("diagnostics", AppRoute.Diagnostics),
    ABOUT("about", AppRoute.About),
    ;

    companion object {
        fun fromAtom(raw: String): WidgetRoute? = when (raw.trim().lowercase()) {
            "remote" -> RUNTIME
            "themes" -> APPEARANCE
            else -> entries.firstOrNull { it.atom == raw.trim().lowercase() }
        }
    }
}

enum class WidgetAlignment { START, CENTER, END }

enum class WidgetSemanticColorRole(val atom: String) {
    BACKGROUND("background"),
    SURFACE("surface"),
    SURFACE_ELEVATED("surface_elevated"),
    BORDER("border"),
    BORDER_ACTIVE("border_active"),
    PRIMARY("primary"),
    SECONDARY("secondary"),
    ACCENT_MAGENTA("accent_magenta"),
    ACCENT_CYAN("accent_cyan"),
    TEXT("text"),
    TEXT_MUTED("text_muted"),
    SUCCESS("success"),
    WARNING("warning"),
    ERROR("error"),
    FOCUS("focus"),
    AMBIENT_GLOW("ambient_glow"),
}

fun interface WidgetSemanticPaletteResolver {
    fun resolve(theme: ZaraTheme, role: WidgetSemanticColorRole): Int
}

data class WidgetColors(
    val background: Int,
    val surface: Int,
    val border: Int,
    val title: Int,
    val body: Int,
    val label: Int,
    val status: Int,
    val actionBackground: Int,
    val actionText: Int,
    val sigil: Int,
)

data class WidgetMetrics(
    val outerPaddingDp: Int = 16,
    val contentGapDp: Int = 6,
    val cornerRadiusDp: Int = 20,
    val borderWidthDp: Int = 1,
    val titleSp: Int = 16,
    val bodySp: Int = 12,
    val labelSp: Int = 10,
    val actionSp: Int = 11,
    val sigilSizeDp: Int = 34,
    val actionCornerRadiusDp: Int = 12,
)

data class WidgetText(
    val eyebrow: String,
    val title: String,
    val subtitle: String,
    val primaryLabel: String,
    val secondaryLabel: String,
    val tertiaryLabel: String,
)

data class WidgetVisibility(
    val showSigil: Boolean = true,
    val showSubtitle: Boolean = true,
    val showStatus: Boolean = true,
    val showSecondary: Boolean = true,
    val showTertiary: Boolean = true,
)

data class WidgetActions(
    val primary: WidgetRoute,
    val secondary: WidgetRoute,
    val tertiary: WidgetRoute,
)

data class WidgetResolvedStyle(
    val colors: WidgetColors,
    val metrics: WidgetMetrics,
    val text: WidgetText,
    val visibility: WidgetVisibility,
    val alignment: WidgetAlignment,
    val actions: WidgetActions,
)

data class WidgetStyleSheet(
    val name: String,
    val theme: ZaraTheme,
    private val widgets: Map<WidgetKind, WidgetResolvedStyle>,
) {
    fun forWidget(kind: WidgetKind): WidgetResolvedStyle = checkNotNull(widgets[kind])
}

class WidgetStyleCompiler(
    private val semanticColors: WidgetSemanticPaletteResolver,
) {
    fun compile(
        source: String,
        selectedTheme: ZaraTheme,
        systemDark: Boolean,
    ): WidgetStyleSheet {
        require(source.encodeToByteArray().size <= MAX_SOURCE_BYTES) { "Widget stylesheet is too large" }
        val facts = WidgetPrologFacts.parse(source)
        val versions = facts.filter { it.name == "zara_widget_stylesheet" }
        require(versions.size == 1 && versions.single().arguments == listOf("1")) {
            "Expected zara_widget_stylesheet(1)"
        }
        val styleFacts = facts.filter { it.name == "widget_style" }
        require(styleFacts.size == 1 && styleFacts.single().arguments.size == 1) {
            "Expected exactly one widget_style/1 fact"
        }
        val styleName = requireAtom(styleFacts.single().arguments.single(), "style name")
        val themeFacts = facts.filter { it.name == "widget_theme" }
        require(themeFacts.size <= 1) { "widget_theme/2 may be declared once" }
        val declaredTheme = themeFacts.singleOrNull()?.let { fact ->
            require(fact.arguments.size == 2) { "widget_theme/2 has invalid arity" }
            requireStyle(fact.arguments[0], styleName)
            theme(fact.arguments[1], selectedTheme)
        } ?: selectedTheme
        val resolvedTheme = if (declaredTheme == ZaraTheme.System) {
            if (systemDark) ZaraTheme.Outrun else ZaraTheme.Light
        } else {
            declaredTheme
        }
        val supported = setOf(
            "zara_widget_stylesheet",
            "widget_style",
            "widget_theme",
            "widget_color",
            "widget_metric",
            "widget_text",
            "widget_flag",
            "widget_alignment",
            "widget_action",
        )
        facts.forEach { require(it.name in supported) { "Unsupported widget stylesheet predicate ${it.name}/${it.arguments.size}" } }
        rejectDuplicateProperties(facts)
        val widgets = WidgetKind.entries.associateWith { kind ->
            var style = defaults(kind, resolvedTheme)
            facts.filter { it.name !in setOf("zara_widget_stylesheet", "widget_style", "widget_theme") }
                .filter { applies(it, styleName, kind) }
                .forEach { fact -> style = apply(style, fact, resolvedTheme) }
            style
        }
        return WidgetStyleSheet(styleName, declaredTheme, widgets)
    }

    private fun defaults(kind: WidgetKind, theme: ZaraTheme): WidgetResolvedStyle {
        fun semantic(role: WidgetSemanticColorRole) = semanticColors.resolve(theme, role)
        val text = when (kind) {
            WidgetKind.ASSISTANT -> WidgetText(
                "LOCAL · PRIVATE · EXTENSIBLE",
                "Symbolic intelligence",
                "On your terms",
                "CHAT",
                "VOICE",
                "LOGIC",
            )
            WidgetKind.RUNTIME -> WidgetText(
                "ZARA RUNTIME",
                "Runtime",
                "Last known device state",
                "RUNTIME",
                "DIAGNOSTICS",
                "LOGIC",
            )
            WidgetKind.ACTIONS -> WidgetText(
                "QUICK ACTIONS",
                "Open Zara",
                "Bounded routes into the symbolic workspace",
                "CHAT",
                "VOICE",
                "LOGIC",
            )
        }
        val actions = when (kind) {
            WidgetKind.RUNTIME -> WidgetActions(WidgetRoute.RUNTIME, WidgetRoute.DIAGNOSTICS, WidgetRoute.LOGIC)
            else -> WidgetActions(WidgetRoute.CHAT, WidgetRoute.VOICE, WidgetRoute.LOGIC)
        }
        return WidgetResolvedStyle(
            colors = WidgetColors(
                background = semantic(WidgetSemanticColorRole.BACKGROUND),
                surface = semantic(WidgetSemanticColorRole.SURFACE),
                border = semantic(WidgetSemanticColorRole.BORDER),
                title = semantic(WidgetSemanticColorRole.TEXT),
                body = semantic(WidgetSemanticColorRole.TEXT_MUTED),
                label = semantic(WidgetSemanticColorRole.ACCENT_CYAN),
                status = semantic(WidgetSemanticColorRole.SUCCESS),
                actionBackground = semantic(WidgetSemanticColorRole.SURFACE_ELEVATED),
                actionText = semantic(WidgetSemanticColorRole.TEXT),
                sigil = semantic(WidgetSemanticColorRole.ACCENT_MAGENTA),
            ),
            metrics = WidgetMetrics(),
            text = text,
            visibility = WidgetVisibility(),
            alignment = WidgetAlignment.START,
            actions = actions,
        )
    }

    private fun applies(fact: WidgetFact, styleName: String, kind: WidgetKind): Boolean {
        require(fact.arguments.size >= 2) { "${fact.name} has invalid arity" }
        requireStyle(fact.arguments[0], styleName)
        val selector = fact.arguments[1]
        require(selector == "all" || WidgetKind.entries.any { it.atom == selector }) {
            "Unknown widget selector $selector"
        }
        return selector == "all" || selector == kind.atom
    }

    private fun apply(
        style: WidgetResolvedStyle,
        fact: WidgetFact,
        theme: ZaraTheme,
    ): WidgetResolvedStyle = when (fact.name) {
        "widget_color" -> {
            require(fact.arguments.size == 4) { "widget_color/4 has invalid arity" }
            style.copy(colors = applyColor(style.colors, fact.arguments[2], fact.arguments[3], theme))
        }
        "widget_metric" -> {
            require(fact.arguments.size == 4) { "widget_metric/4 has invalid arity" }
            style.copy(metrics = applyMetric(style.metrics, fact.arguments[2], integer(fact.arguments[3])))
        }
        "widget_text" -> {
            require(fact.arguments.size == 4) { "widget_text/4 has invalid arity" }
            style.copy(text = applyText(style.text, fact.arguments[2], WidgetPrologFacts.decodeQuoted(fact.arguments[3])))
        }
        "widget_flag" -> {
            require(fact.arguments.size == 4) { "widget_flag/4 has invalid arity" }
            style.copy(visibility = applyFlag(style.visibility, fact.arguments[2], boolean(fact.arguments[3])))
        }
        "widget_alignment" -> {
            require(fact.arguments.size == 3) { "widget_alignment/3 has invalid arity" }
            style.copy(alignment = enumValue<WidgetAlignment>(fact.arguments[2], "alignment"))
        }
        "widget_action" -> {
            require(fact.arguments.size == 4) { "widget_action/4 has invalid arity" }
            style.copy(actions = applyAction(style.actions, fact.arguments[2], route(fact.arguments[3])))
        }
        else -> error("Unsupported fact ${fact.name}")
    }

    private fun applyColor(colors: WidgetColors, property: String, raw: String, theme: ZaraTheme): WidgetColors {
        val value = color(raw, theme)
        return when (property) {
            "background" -> colors.copy(background = value)
            "surface" -> colors.copy(surface = value)
            "border" -> colors.copy(border = value)
            "title" -> colors.copy(title = value)
            "body" -> colors.copy(body = value)
            "label" -> colors.copy(label = value)
            "status" -> colors.copy(status = value)
            "action_background" -> colors.copy(actionBackground = value)
            "action_text" -> colors.copy(actionText = value)
            "sigil" -> colors.copy(sigil = value)
            else -> throw IllegalArgumentException("Unknown widget color $property")
        }
    }

    private fun applyMetric(metrics: WidgetMetrics, property: String, value: Int): WidgetMetrics = when (property) {
        "outer_padding_dp" -> metrics.copy(outerPaddingDp = bounded(property, value, 0, 40))
        "content_gap_dp" -> metrics.copy(contentGapDp = bounded(property, value, 0, 32))
        "corner_radius_dp" -> metrics.copy(cornerRadiusDp = bounded(property, value, 0, 48))
        "border_width_dp" -> metrics.copy(borderWidthDp = bounded(property, value, 0, 8))
        "title_sp" -> metrics.copy(titleSp = bounded(property, value, 8, 32))
        "body_sp" -> metrics.copy(bodySp = bounded(property, value, 8, 28))
        "label_sp" -> metrics.copy(labelSp = bounded(property, value, 8, 24))
        "action_sp" -> metrics.copy(actionSp = bounded(property, value, 8, 24))
        "sigil_size_dp" -> metrics.copy(sigilSizeDp = bounded(property, value, 12, 64))
        "action_corner_radius_dp" -> metrics.copy(actionCornerRadiusDp = bounded(property, value, 0, 32))
        else -> throw IllegalArgumentException("Unknown widget metric $property")
    }

    private fun applyText(text: WidgetText, property: String, value: String): WidgetText {
        require(value.length <= 160 && value.none { it.code < 0x20 && it != '\n' }) { "Widget text is invalid" }
        return when (property) {
            "eyebrow" -> text.copy(eyebrow = value)
            "title" -> text.copy(title = value)
            "subtitle" -> text.copy(subtitle = value)
            "primary_label" -> text.copy(primaryLabel = value)
            "secondary_label" -> text.copy(secondaryLabel = value)
            "tertiary_label" -> text.copy(tertiaryLabel = value)
            else -> throw IllegalArgumentException("Unknown widget text $property")
        }
    }

    private fun applyFlag(visibility: WidgetVisibility, property: String, value: Boolean): WidgetVisibility = when (property) {
        "show_sigil" -> visibility.copy(showSigil = value)
        "show_subtitle" -> visibility.copy(showSubtitle = value)
        "show_status" -> visibility.copy(showStatus = value)
        "show_secondary" -> visibility.copy(showSecondary = value)
        "show_tertiary" -> visibility.copy(showTertiary = value)
        else -> throw IllegalArgumentException("Unknown widget flag $property")
    }

    private fun applyAction(actions: WidgetActions, property: String, value: WidgetRoute): WidgetActions = when (property) {
        "primary" -> actions.copy(primary = value)
        "secondary" -> actions.copy(secondary = value)
        "tertiary" -> actions.copy(tertiary = value)
        else -> throw IllegalArgumentException("Unknown widget action slot $property")
    }

    private fun color(raw: String, theme: ZaraTheme): Int {
        val semantic = Regex("semantic\\(([a-z_]+)\\)").matchEntire(raw)
        if (semantic != null) {
            val role = WidgetSemanticColorRole.entries.firstOrNull { it.atom == semantic.groupValues[1] }
                ?: throw IllegalArgumentException("Unknown semantic color ${semantic.groupValues[1]}")
            return semanticColors.resolve(theme, role)
        }
        val decoded = WidgetPrologFacts.decodeQuoted(raw)
        require(decoded.matches(Regex("#[0-9A-Fa-f]{6}|#[0-9A-Fa-f]{8}"))) {
            "Widget colors must be semantic(role), #RRGGBB, or #AARRGGBB"
        }
        val digits = decoded.drop(1)
        return if (digits.length == 6) {
            (0xFF000000L or digits.toLong(16)).toInt()
        } else {
            digits.toLong(16).toInt()
        }
    }

    private fun rejectDuplicateProperties(facts: List<WidgetFact>) {
        val seen = mutableSetOf<String>()
        facts.filter { it.name.startsWith("widget_") && it.name !in setOf("widget_style", "widget_theme") }
            .forEach { fact ->
                require(fact.arguments.size >= 3) { "${fact.name} has invalid arity" }
                val key = "${fact.name}:${fact.arguments[0]}:${fact.arguments[1]}:${fact.arguments[2]}"
                require(seen.add(key)) { "Duplicate widget style declaration" }
            }
    }

    private fun requireStyle(raw: String, expected: String) {
        require(requireAtom(raw, "style reference") == expected) { "Widget declaration references another style" }
    }

    private fun theme(raw: String, current: ZaraTheme): ZaraTheme = when (raw) {
        "current" -> current
        else -> ZaraTheme.entries.firstOrNull { it.name.lowercase() == raw }
            ?: throw IllegalArgumentException("Unknown widget theme $raw")
    }

    private fun route(raw: String): WidgetRoute = WidgetRoute.fromAtom(raw)
        ?: throw IllegalArgumentException("Unknown widget route $raw")

    private inline fun <reified T : Enum<T>> enumValue(raw: String, label: String): T =
        enumValues<T>().firstOrNull { it.name.lowercase() == raw }
            ?: throw IllegalArgumentException("Unknown widget $label $raw")

    private fun requireAtom(raw: String, label: String): String {
        require(raw.matches(Regex("[a-z][a-z0-9_]{0,63}"))) { "Invalid $label" }
        return raw
    }

    private fun integer(raw: String): Int = raw.toIntOrNull()
        ?: throw IllegalArgumentException("Widget metric must be an integer")

    private fun boolean(raw: String): Boolean = when (raw) {
        "true" -> true
        "false" -> false
        else -> throw IllegalArgumentException("Widget flag must be true or false")
    }

    private fun bounded(name: String, value: Int, minimum: Int, maximum: Int): Int {
        require(value in minimum..maximum) { "$name must be between $minimum and $maximum" }
        return value
    }

    companion object {
        const val MAX_SOURCE_BYTES = 128 * 1024
    }
}

internal data class WidgetFact(val name: String, val arguments: List<String>)

internal object WidgetPrologFacts {
    fun parse(source: String): List<WidgetFact> = splitClauses(source).map { clause ->
        require(!clause.startsWith(":-") && !clause.contains(":-")) {
            "Widget stylesheets may contain facts only"
        }
        val match = Regex("^([a-z][a-z0-9_]*)\\((.*)\\)$", RegexOption.DOT_MATCHES_ALL).matchEntire(clause)
            ?: throw IllegalArgumentException("Malformed widget stylesheet fact")
        WidgetFact(match.groupValues[1], splitArguments(match.groupValues[2]))
    }.also { require(it.isNotEmpty()) { "Widget stylesheet is empty" } }

    fun decodeQuoted(raw: String): String {
        require(raw.length >= 2 && raw.first() == '\'' && raw.last() == '\'') {
            "Widget text and literal colors must be quoted"
        }
        val result = StringBuilder()
        var escaped = false
        raw.substring(1, raw.length - 1).forEach { character ->
            if (escaped) {
                result.append(
                    when (character) {
                        'n' -> '\n'
                        't' -> '\t'
                        else -> character
                    },
                )
                escaped = false
            } else if (character == '\\') {
                escaped = true
            } else {
                result.append(character)
            }
        }
        require(!escaped) { "Widget quoted value has an incomplete escape" }
        return result.toString()
    }

    private fun splitClauses(source: String): List<String> {
        val clauses = mutableListOf<String>()
        val current = StringBuilder()
        var round = 0
        var quoted = false
        var escaped = false
        var comment = false
        source.forEach { character ->
            if (comment) {
                if (character == '\n') comment = false
                return@forEach
            }
            if (!quoted && character == '%') {
                comment = true
                return@forEach
            }
            if (quoted) {
                current.append(character)
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == '\'') quoted = false
                return@forEach
            }
            when (character) {
                '\'' -> {
                    quoted = true
                    current.append(character)
                }
                '(' -> {
                    round += 1
                    current.append(character)
                }
                ')' -> {
                    round -= 1
                    require(round >= 0) { "Unbalanced widget stylesheet" }
                    current.append(character)
                }
                '.' -> if (round == 0) {
                    val clause = current.toString().trim()
                    if (clause.isNotEmpty()) clauses += clause
                    current.clear()
                } else {
                    current.append(character)
                }
                else -> current.append(character)
            }
        }
        require(!quoted && round == 0 && current.isBlank()) { "Unterminated widget stylesheet fact" }
        return clauses
    }

    private fun splitArguments(source: String): List<String> {
        if (source.isBlank()) return emptyList()
        val arguments = mutableListOf<String>()
        val current = StringBuilder()
        var round = 0
        var quoted = false
        var escaped = false
        source.forEach { character ->
            if (quoted) {
                current.append(character)
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == '\'') quoted = false
                return@forEach
            }
            when (character) {
                '\'' -> {
                    quoted = true
                    current.append(character)
                }
                '(' -> {
                    round += 1
                    current.append(character)
                }
                ')' -> {
                    round -= 1
                    require(round >= 0) { "Unbalanced widget stylesheet term" }
                    current.append(character)
                }
                ',' -> if (round == 0) {
                    arguments += current.toString().trim()
                    current.clear()
                } else {
                    current.append(character)
                }
                else -> current.append(character)
            }
        }
        require(!quoted && round == 0) { "Unbalanced widget stylesheet arguments" }
        arguments += current.toString().trim()
        require(arguments.none(String::isEmpty)) { "Widget stylesheet has an empty argument" }
        return arguments
    }
}

class WidgetStyleStore(
    private val file: File,
    private val compiler: WidgetStyleCompiler,
) {
    fun load(defaultSource: String, selectedTheme: ZaraTheme, systemDark: Boolean): WidgetStyleSheet {
        val imported = if (file.isFile && file.length() in 1..WidgetStyleCompiler.MAX_SOURCE_BYTES.toLong()) {
            runCatching { file.readText() }.getOrNull()
        } else {
            null
        }
        return imported?.let { runCatching { compiler.compile(it, selectedTheme, systemDark) }.getOrNull() }
            ?: compiler.compile(defaultSource, selectedTheme, systemDark)
    }

    fun import(source: String, selectedTheme: ZaraTheme, systemDark: Boolean): WidgetStyleSheet {
        val compiled = compiler.compile(source, selectedTheme, systemDark)
        val directory = file.absoluteFile.parentFile
            ?: throw IllegalStateException("Widget style path has no parent")
        check(directory.exists() || directory.mkdirs()) { "Widget style directory could not be created" }
        val temporary = Files.createTempFile(directory.toPath(), ".${file.name}.", ".tmp").toFile()
        try {
            temporary.writeText(source)
            replace(temporary, file)
        } finally {
            temporary.delete()
        }
        return compiled
    }

    fun exportSource(): String? = if (hasImport()) file.readText() else null

    fun hasImport(): Boolean = file.isFile && file.length() in 1..WidgetStyleCompiler.MAX_SOURCE_BYTES.toLong()

    fun reset() {
        check(!file.exists() || file.delete()) { "Imported widget style could not be removed" }
    }

    private fun replace(source: File, destination: File) {
        try {
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING,
            )
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING)
        }
    }
}
