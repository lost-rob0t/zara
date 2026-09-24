package ai.zara.app.ui.extensions

private val PORTABLE_ID = Regex("[a-zA-Z0-9][a-zA-Z0-9._-]{0,63}")
private val PORTABLE_OWNER = Regex("[a-zA-Z0-9][a-zA-Z0-9._:-]{0,127}")
private val ACTION_PREFIXES = listOf("route:", "prompt:", "submit:", "plugin:")
private const val MAX_LABEL = 160
private const val MAX_ACTION = 1_024
private const val MAX_CONTRIBUTIONS = 256

enum class UiPlatform(val wire: String) {
    DESKTOP("desktop"),
    ANDROID("android");

    companion object {
        fun fromWire(value: String): UiPlatform =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("Unknown UI platform: $value")
    }
}

enum class UiSlot(val wire: String) {
    DRAWER("drawer"),
    CHAT_TOP("chat.top"),
    CHAT_BOTTOM("chat.bottom"),
    SETTINGS("settings"),
    PLUGINS("plugins");

    companion object {
        fun fromWire(value: String): UiSlot =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("Unknown UI slot: $value")
    }
}

enum class UiContributionKind(val wire: String) {
    SURFACE("surface"),
    SECTION("section"),
    TEXT("text"),
    BUTTON("button"),
    TOGGLE("toggle"),
    STATUS("status");

    companion object {
        fun fromWire(value: String): UiContributionKind =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("Unknown UI contribution kind: $value")
    }
}

data class UiContribution(
    val id: String,
    val slot: UiSlot,
    val kind: UiContributionKind,
    val label: String,
    val action: String = "",
    val priority: Int = 100,
    val platforms: Set<UiPlatform> = setOf(UiPlatform.DESKTOP, UiPlatform.ANDROID),
    val owner: String = "unbound",
) {
    init {
        require(PORTABLE_ID.matches(id)) {
            "UI contribution id must be a bounded portable identifier"
        }
        require(label.isNotBlank() && label.length <= MAX_LABEL) {
            "UI contribution label must contain 1 to $MAX_LABEL characters"
        }
        require(priority in -10_000..10_000) {
            "UI contribution priority must be between -10000 and 10000"
        }
        require(platforms.isNotEmpty()) {
            "UI contribution must target at least one platform"
        }
        require(PORTABLE_OWNER.matches(owner)) {
            "UI contribution owner must be a bounded portable identifier"
        }
        require(action.length <= MAX_ACTION) {
            "UI action must not exceed $MAX_ACTION characters"
        }
        require(action.isEmpty() || ACTION_PREFIXES.any(action::startsWith)) {
            "UI action must use route:, prompt:, submit:, or plugin:"
        }
        if (kind in setOf(UiContributionKind.SURFACE, UiContributionKind.BUTTON, UiContributionKind.TOGGLE)) {
            require(action.isNotEmpty()) {
                "UI contribution kind ${kind.wire} requires an action"
            }
        }
    }
}

class UiExtensionRegistry {
    private val byOwner = linkedMapOf<String, List<UiContribution>>()

    @Synchronized
    fun replaceOwner(owner: String, contributions: List<UiContribution>) {
        require(PORTABLE_OWNER.matches(owner)) {
            "UI owner must be a bounded portable identifier"
        }
        require(contributions.size <= MAX_CONTRIBUTIONS) {
            "UI owner may register at most $MAX_CONTRIBUTIONS contributions"
        }
        val staged = contributions.map { it.copy(owner = owner) }
        val keys = mutableSetOf<Pair<UiSlot, String>>()
        staged.forEach { item ->
            require(keys.add(item.slot to item.id)) {
                "duplicate UI contribution ${item.slot.wire}:${item.id}"
            }
        }
        byOwner[owner] = staged
    }

    @Synchronized
    fun clearOwner(owner: String) {
        byOwner.remove(owner)
    }

    @Synchronized
    fun snapshot(): List<UiContribution> =
        byOwner.values
            .flatten()
            .sortedWith(
                compareBy<UiContribution> { it.priority }
                    .thenBy { it.slot.wire }
                    .thenBy { it.owner }
                    .thenBy { it.id },
            )

    fun forPlatform(platform: UiPlatform, slot: UiSlot? = null): List<UiContribution> =
        snapshot().filter { item ->
            platform in item.platforms && (slot == null || item.slot == slot)
        }
}

object PortablePythonUiInitParser {
    fun parse(source: String): List<UiContribution> {
        val contributions = mutableListOf<UiContribution>()
        var sawRegister = false
        source.lineSequence().forEachIndexed { index, raw ->
            val line = raw.trim()
            if (line.isEmpty() || line.startsWith("#")) return@forEachIndexed
            if (line == "def register(ui):") {
                require(!sawRegister) { "init.py may define register(ui) only once" }
                sawRegister = true
                return@forEachIndexed
            }
            require(sawRegister && line.startsWith("ui.add(") && line.endsWith(")")) {
                "Android portable init.py only permits ui.add(...) calls; line ${index + 1} is executable Python"
            }
            contributions += parseAdd(line, index + 1)
            require(contributions.size <= MAX_CONTRIBUTIONS) {
                "init.py contains too many UI contributions"
            }
        }
        require(sawRegister) { "init.py must define register(ui)" }
        val registry = UiExtensionRegistry()
        registry.replaceOwner("user:init.py", contributions)
        return registry.snapshot()
    }

    private fun parseAdd(line: String, lineNumber: Int): UiContribution {
        val body = line.removePrefix("ui.add(").dropLast(1)
        val arguments = splitTopLevel(body)
        require(arguments.size == 7) {
            "ui.add on line $lineNumber must have 7 positional arguments"
        }
        return UiContribution(
            id = parseString(arguments[0]),
            slot = UiSlot.fromWire(parseString(arguments[1])),
            kind = UiContributionKind.fromWire(parseString(arguments[2])),
            label = parseString(arguments[3]),
            action = parseString(arguments[4]),
            priority = arguments[5].trim().toIntOrNull()
                ?: throw IllegalArgumentException("ui.add priority on line $lineNumber must be an integer"),
            platforms = parseStringList(arguments[6]).map(UiPlatform::fromWire).toSet(),
        )
    }

    private fun splitTopLevel(text: String): List<String> {
        val output = mutableListOf<String>()
        val current = StringBuilder()
        var quoted = false
        var escaped = false
        var square = 0
        text.forEach { character ->
            if (quoted) {
                current.append(character)
                when {
                    escaped -> escaped = false
                    character == '\\' -> escaped = true
                    character == '"' -> quoted = false
                }
                return@forEach
            }
            when (character) {
                '"' -> {
                    quoted = true
                    current.append(character)
                }
                '[' -> {
                    square += 1
                    current.append(character)
                }
                ']' -> {
                    square -= 1
                    require(square >= 0) { "unbalanced ui.add platform list" }
                    current.append(character)
                }
                ',' -> if (square == 0) {
                    output += current.toString().trim()
                    current.clear()
                } else {
                    current.append(character)
                }
                else -> current.append(character)
            }
        }
        require(!quoted && square == 0) { "unterminated ui.add value" }
        output += current.toString().trim()
        return output
    }

    private fun parseString(raw: String): String {
        val value = raw.trim()
        require(value.length >= 2 && value.first() == '"' && value.last() == '"') {
            "portable init.py string arguments must use double quotes"
        }
        val body = value.substring(1, value.length - 1)
        val result = StringBuilder()
        var escaped = false
        body.forEach { character ->
            if (escaped) {
                result.append(
                    when (character) {
                        'n' -> '\n'
                        'r' -> '\r'
                        't' -> '\t'
                        '\\' -> '\\'
                        '"' -> '"'
                        else -> throw IllegalArgumentException("unsupported string escape: \\$character")
                    },
                )
                escaped = false
            } else if (character == '\\') {
                escaped = true
            } else {
                result.append(character)
            }
        }
        require(!escaped) { "unterminated string escape" }
        return result.toString()
    }

    private fun parseStringList(raw: String): List<String> {
        val value = raw.trim()
        require(value.startsWith("[") && value.endsWith("]")) {
            "portable init.py platforms must be a list"
        }
        val body = value.substring(1, value.length - 1).trim()
        if (body.isEmpty()) return emptyList()
        return splitTopLevel(body).map(::parseString)
    }
}
