package ai.zara.app.ui

import ai.zara.ui.theme.ZaraSemanticTokens
import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import androidx.compose.ui.graphics.Color

private const val MAX_PROLOG_THEME_BYTES = 128 * 1024
private const val MAX_THEME_FACE_OVERRIDES = 64

/**
 * A user-authored Prolog theme resolved onto one of Zara's native semantic
 * palettes. The source remains Prolog; Android only consumes the documented
 * theme export predicates from it.
 */
data class ImportedPrologTheme(
    val id: String,
    val displayName: String,
    val parent: ZaraTheme,
    val tokens: ZaraSemanticTokens,
)

/**
 * Emacs-like theme file support for Zara Android.
 *
 *   zara_theme/2        ~= deftheme
 *   zara_theme_parent/2 ~= derived theme inheritance
 *   zara_theme_face/3   ~= custom-theme-set-faces
 *
 * Other Prolog code is allowed to coexist in the file. This importer only
 * reads the exported theme facts; it does not pretend the file is a second
 * configuration language.
 */
object PrologThemeFile {
    private val idPattern = Regex("[a-z][a-z0-9_]*")
    private val themePattern = Regex(
        """^\s*zara_theme\(([a-z][a-z0-9_]*),\s*'((?:''|[^'])*)'\)\.\s*$"""
    )
    private val parentPattern = Regex(
        """^\s*zara_theme_parent\(([a-z][a-z0-9_]*),\s*([a-z][a-z0-9_]*)\)\.\s*$"""
    )
    private val facePattern = Regex(
        """^\s*zara_theme_face\(([a-z][a-z0-9_]*),\s*([a-z][a-z0-9_]*),\s*'(#[0-9A-Fa-f]{6}|#[0-9A-Fa-f]{8})'\)\.\s*$"""
    )

    private val parents = mapOf(
        "outrun" to ZaraTheme.Outrun,
        "outrun_oled" to ZaraTheme.OutrunOled,
        "starintel" to ZaraTheme.StarIntel,
        "starintel_oled" to ZaraTheme.StarIntelOled,
        "midnight" to ZaraTheme.Midnight,
        "midnight_oled" to ZaraTheme.MidnightOled,
        "terminal" to ZaraTheme.Terminal,
        "terminal_oled" to ZaraTheme.TerminalOled,
        "light" to ZaraTheme.Light,
    )

    fun make(
        id: String,
        displayName: String,
        parent: ZaraTheme = ZaraTheme.OutrunOled,
    ): String {
        require(idPattern.matches(id)) {
            "Theme id must be a lowercase Prolog atom: [a-z][a-z0-9_]*"
        }
        val parentAtom = parents.entries.firstOrNull { it.value == parent }?.key
            ?: throw IllegalArgumentException("System cannot be a deterministic custom-theme parent")
        val label = displayName.replace("'", "''")
        return """% Zara custom theme — ordinary Prolog, Emacs-style inheritance.
% Keep only the faces you want to override; every other face comes from Parent.

zara_theme($id, '$label').
zara_theme_parent($id, $parentAtom).

% zara_theme_face($id, background, '#000000').
% zara_theme_face($id, primary, '#e21cf2').
% zara_theme_face($id, accent_cyan, '#00d7ff').
% zara_theme_face($id, text, '#eaf2ff').
"""
    }

    fun import(source: String, systemDark: Boolean = true): ImportedPrologTheme {
        require(source.toByteArray(Charsets.UTF_8).size <= MAX_PROLOG_THEME_BYTES) {
            "Theme file exceeds $MAX_PROLOG_THEME_BYTES bytes"
        }

        var declaredId: String? = null
        var displayName: String? = null
        var parentId: String? = null
        val faces = linkedMapOf<String, Color>()

        source.lineSequence().forEachIndexed { index, rawLine ->
            val line = stripComment(rawLine).trim()
            if (line.isEmpty()) return@forEachIndexed

            themePattern.matchEntire(line)?.let { match ->
                check(declaredId == null) { "Multiple zara_theme/2 declarations" }
                declaredId = match.groupValues[1]
                displayName = match.groupValues[2].replace("''", "'")
                return@forEachIndexed
            }

            parentPattern.matchEntire(line)?.let { match ->
                val id = match.groupValues[1]
                check(parentId == null) { "Multiple zara_theme_parent/2 declarations" }
                ensureSameTheme(declaredId, id, index + 1)
                parentId = match.groupValues[2]
                return@forEachIndexed
            }

            facePattern.matchEntire(line)?.let { match ->
                val id = match.groupValues[1]
                val face = match.groupValues[2]
                ensureSameTheme(declaredId, id, index + 1)
                require(face in FACE_NAMES) { "Unknown theme face '$face' on line ${index + 1}" }
                require(faces.size < MAX_THEME_FACE_OVERRIDES || face in faces) {
                    "Theme has too many face overrides"
                }
                check(face !in faces) { "Duplicate theme face '$face'" }
                faces[face] = parseColor(match.groupValues[3])
                return@forEachIndexed
            }

            if (line.startsWith("zara_theme(") ||
                line.startsWith("zara_theme_parent(") ||
                line.startsWith("zara_theme_face(")) {
                throw IllegalArgumentException("Malformed Zara theme fact on line ${index + 1}")
            }
            // Deliberately ignore unrelated Prolog. A theme file is still Prolog,
            // not a bespoke JSON-ish format with a .pl extension.
        }

        val id = requireNotNull(declaredId) { "Missing zara_theme/2" }
        val label = requireNotNull(displayName) { "Missing theme display name" }
        val parentAtom = requireNotNull(parentId) { "Missing zara_theme_parent/2" }
        val parent = parents[parentAtom]
            ?: throw IllegalArgumentException("Unknown deterministic theme parent '$parentAtom'")

        val base = themeTokens(parent, systemDark = systemDark, reducedGlow = false)
        return ImportedPrologTheme(
            id = id,
            displayName = label,
            parent = parent,
            tokens = applyFaces(base, faces),
        )
    }

    private fun ensureSameTheme(declaredId: String?, factId: String, line: Int) {
        if (declaredId != null && declaredId != factId) {
            throw IllegalArgumentException(
                "Theme fact on line $line targets '$factId', expected '$declaredId'"
            )
        }
    }

    private fun stripComment(line: String): String {
        var quoted = false
        var index = 0
        while (index < line.length) {
            when (line[index]) {
                '\'' -> {
                    if (quoted && index + 1 < line.length && line[index + 1] == '\'') {
                        index += 2
                        continue
                    }
                    quoted = !quoted
                }
                '%' -> if (!quoted) return line.substring(0, index)
            }
            index += 1
        }
        return line
    }

    private fun parseColor(value: String): Color {
        val hex = value.drop(1)
        val (alpha, red, green, blue) = when (hex.length) {
            6 -> listOf(255, hexByte(hex, 0), hexByte(hex, 2), hexByte(hex, 4))
            8 -> listOf(
                hexByte(hex, 0),
                hexByte(hex, 2),
                hexByte(hex, 4),
                hexByte(hex, 6),
            )
            else -> error("Color regex accepted an invalid color")
        }
        return Color(red = red, green = green, blue = blue, alpha = alpha)
    }

    private fun hexByte(hex: String, offset: Int): Int =
        hex.substring(offset, offset + 2).toInt(16)

    private fun applyFaces(
        base: ZaraSemanticTokens,
        faces: Map<String, Color>,
    ): ZaraSemanticTokens = base.copy(
        background = faces["background"] ?: base.background,
        surface = faces["surface"] ?: base.surface,
        surfaceElevated = faces["surface_elevated"] ?: base.surfaceElevated,
        surfaceInput = faces["surface_input"] ?: base.surfaceInput,
        border = faces["border"] ?: base.border,
        borderActive = faces["border_active"] ?: base.borderActive,
        primary = faces["primary"] ?: base.primary,
        secondary = faces["secondary"] ?: base.secondary,
        accentMagenta = faces["accent_magenta"] ?: base.accentMagenta,
        accentCyan = faces["accent_cyan"] ?: base.accentCyan,
        text = faces["text"] ?: base.text,
        textMuted = faces["text_muted"] ?: base.textMuted,
        success = faces["success"] ?: base.success,
        warning = faces["warning"] ?: base.warning,
        error = faces["error"] ?: base.error,
        focus = faces["focus"] ?: base.focus,
        ambientGlow = faces["ambient_glow"] ?: base.ambientGlow,
    )

    private val FACE_NAMES = setOf(
        "background",
        "surface",
        "surface_elevated",
        "surface_input",
        "border",
        "border_active",
        "primary",
        "secondary",
        "accent_magenta",
        "accent_cyan",
        "text",
        "text_muted",
        "success",
        "warning",
        "error",
        "focus",
        "ambient_glow",
    )
}
