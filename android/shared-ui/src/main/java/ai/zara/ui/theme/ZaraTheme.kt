package ai.zara.ui.theme

import androidx.compose.ui.graphics.Color

data class ZaraSemanticTokens(
    val background: Color,
    val surface: Color,
    val surfaceElevated: Color,
    val surfaceInput: Color,
    val border: Color,
    val borderActive: Color,
    val primary: Color,
    val secondary: Color,
    val accentMagenta: Color,
    val accentCyan: Color,
    val text: Color,
    val textMuted: Color,
    val success: Color,
    val warning: Color,
    val error: Color,
    val focus: Color,
    val ambientGlow: Color,
)

private val OutrunTokens = ZaraSemanticTokens(
    background = Color(0xFF02040B),
    surface = Color(0xFF07101B),
    surfaceElevated = Color(0xFF0A1324),
    surfaceInput = Color(0xFF080F1E),
    border = Color(0xFF1A2A49),
    borderActive = Color(0xFF775CFF),
    primary = Color(0xFFE21CF2),
    secondary = Color(0xFF16D9FF),
    accentMagenta = Color(0xFFF000FF),
    accentCyan = Color(0xFF00D7FF),
    text = Color(0xFFEAF2FF),
    textMuted = Color(0xFF8D9DBA),
    success = Color(0xFF6CE7A6),
    warning = Color(0xFFFFD166),
    error = Color(0xFFFF6B8B),
    focus = Color(0xFFB56DFF),
    ambientGlow = Color(0xFF3A0D5E),
)

enum class ZaraTheme { Outrun, StarIntel, Midnight, Terminal, Light, System }

fun themeTokens(
    theme: ZaraTheme,
    systemDark: Boolean,
    reducedGlow: Boolean,
): ZaraSemanticTokens {
    val resolved = if (theme == ZaraTheme.System) {
        if (systemDark) ZaraTheme.Outrun else ZaraTheme.Light
    } else {
        theme
    }
    val tokens = when (resolved) {
        ZaraTheme.StarIntel -> OutrunTokens.copy(
            background = Color(0xFF080807),
            surface = Color(0xFF14130F),
            surfaceElevated = Color(0xFF201D15),
            surfaceInput = Color(0xFF10100D),
            border = Color(0xFF4C4329),
            borderActive = Color(0xFFE8C56A),
            primary = Color(0xFFE8C56A),
            secondary = Color(0xFFF1DA9A),
            accentMagenta = Color(0xFFD4AF37),
            accentCyan = Color(0xFFF1DA9A),
            text = Color(0xFFF8F3E6),
            textMuted = Color(0xFFBEB5A1),
            focus = Color(0xFFFFD971),
            ambientGlow = Color(0xFF342A10),
        )
        ZaraTheme.Midnight -> OutrunTokens.copy(
            background = Color(0xFF080919),
            surface = Color(0xFF11132A),
            surfaceElevated = Color(0xFF1B1D3C),
            surfaceInput = Color(0xFF0D1024),
            border = Color(0xFF343B68),
            borderActive = Color(0xFF9C92FF),
            primary = Color(0xFFB3A4FF),
            secondary = Color(0xFF8ABFFF),
            accentMagenta = Color(0xFFB3A4FF),
            accentCyan = Color(0xFF8ABFFF),
            focus = Color(0xFFCEC4FF),
            ambientGlow = Color(0xFF24204E),
        )
        ZaraTheme.Terminal -> OutrunTokens.copy(
            background = Color(0xFF030805),
            surface = Color(0xFF08120C),
            surfaceElevated = Color(0xFF102117),
            surfaceInput = Color(0xFF050D08),
            border = Color(0xFF294E36),
            borderActive = Color(0xFF8EF0A8),
            primary = Color(0xFF8EF0A8),
            secondary = Color(0xFFADEBC0),
            accentMagenta = Color(0xFF8EF0A8),
            accentCyan = Color(0xFFADEBC0),
            text = Color(0xFFE3F8E9),
            textMuted = Color(0xFF9CBBA6),
            focus = Color(0xFFBFFFCC),
            ambientGlow = Color(0xFF12321D),
        )
        ZaraTheme.Light -> OutrunTokens.copy(
            background = Color(0xFFF7F7FA),
            surface = Color(0xFFFFFFFF),
            surfaceElevated = Color(0xFFECECF3),
            surfaceInput = Color(0xFFF2F2F7),
            border = Color(0xFFB8BAC8),
            borderActive = Color(0xFF6450A8),
            primary = Color(0xFF7A247D),
            secondary = Color(0xFF006478),
            accentMagenta = Color(0xFF88258C),
            accentCyan = Color(0xFF006478),
            text = Color(0xFF1C2030),
            textMuted = Color(0xFF555C70),
            success = Color(0xFF17623B),
            warning = Color(0xFF765100),
            error = Color(0xFFAC2044),
            focus = Color(0xFF6034A0),
            ambientGlow = Color(0xFFEAE1F3),
        )
        else -> OutrunTokens
    }
    return if (reducedGlow) tokens.copy(ambientGlow = Color.Transparent) else tokens
}
