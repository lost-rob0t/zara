package ai.zara.app.integration

import java.io.File

enum class AndroidAuthorityLevel(val rank: Int) {
    LOCKED(0),
    STANDARD(1),
    ELEVATED(2),
    UNRESTRICTED(3);

    companion object {
        fun fromAtom(atom: String): AndroidAuthorityLevel = when (atom) {
            "locked" -> LOCKED
            "standard" -> STANDARD
            "elevated" -> ELEVATED
            "unrestricted" -> UNRESTRICTED
            else -> throw IllegalArgumentException("unknown Android authority level")
        }
    }
}

enum class AndroidBackend(val atom: String) {
    ASSIST("assist"),
    INTENT("intent"),
    ACCESSIBILITY("accessibility"),
    NOTIFICATION("notification"),
    IME("ime"),
    SHELL("shell"),
    SHIZUKU("shizuku"),
    ROOT("root"),
    DEVICE_POLICY("device_policy"),
    HIDDEN_API("hidden_api"),
    APP_FUNCTIONS("app_functions");

    companion object {
        fun fromAtom(atom: String): AndroidBackend =
            entries.firstOrNull { it.atom == atom }
                ?: throw IllegalArgumentException("unknown Android backend")
    }
}

enum class AndroidConfirmationMode(val atom: String) {
    NONE("none"),
    SENSITIVE("sensitive"),
    ALWAYS("always");

    companion object {
        fun fromAtom(atom: String): AndroidConfirmationMode =
            entries.firstOrNull { it.atom == atom }
                ?: throw IllegalArgumentException("unknown Android confirmation mode")
    }
}

data class AndroidAuthoritySnapshot(
    val global: AndroidAuthorityLevel,
    val backendLevels: Map<AndroidBackend, AndroidAuthorityLevel>,
    val confirmation: AndroidConfirmationMode,
) {
    fun levelFor(backend: AndroidBackend): AndroidAuthorityLevel = backendLevels[backend] ?: global

    fun allows(
        backend: AndroidBackend,
        required: AndroidAuthorityLevel,
    ): Boolean = levelFor(backend).rank >= required.rank
}

class AndroidAuthorityPolicy private constructor(
    private val sources: () -> List<String>,
) {
    fun snapshot(): AndroidAuthoritySnapshot = AndroidAuthorityParser.parse(sources())

    companion object {
        fun fromWorkspace(root: File): AndroidAuthorityPolicy = AndroidAuthorityPolicy {
            root.listFiles()
                .orEmpty()
                .asSequence()
                .filter { file -> file.isFile && file.name.matches(SOURCE_NAME) }
                .sortedBy(File::getName)
                .map(File::readText)
                .toList()
        }

        fun fromSources(sources: () -> List<String>): AndroidAuthorityPolicy =
            AndroidAuthorityPolicy(sources)

        private val SOURCE_NAME = Regex("[a-zA-Z][a-zA-Z0-9_-]{0,63}\\.pl")
    }
}

object AndroidAuthorityParser {
    private val globalFact = Regex("^\\s*android_authority\\(\\s*([a-z][a-z0-9_]*)\\s*\\)\\s*\\.\\s*$")
    private val backendFact = Regex(
        "^\\s*android_backend\\(\\s*([a-z][a-z0-9_]*)\\s*,\\s*([a-z][a-z0-9_]*)\\s*\\)\\s*\\.\\s*$",
    )
    private val confirmationFact = Regex(
        "^\\s*android_confirmation\\(\\s*([a-z][a-z0-9_]*)\\s*,\\s*([a-z][a-z0-9_]*)\\s*\\)\\s*\\.\\s*$",
    )
    private val authorityPredicate = Regex("^\\s*android_(authority|backend|confirmation)\\s*\\(")

    fun parse(sources: List<String>): AndroidAuthoritySnapshot {
        val globalFacts = mutableListOf<AndroidAuthorityLevel>()
        val backendFacts = linkedMapOf<AndroidBackend, AndroidAuthorityLevel>()
        val confirmationFacts = linkedMapOf<AndroidAuthorityLevel, AndroidConfirmationMode>()

        logicalLines(sources).forEach { line ->
            globalFact.matchEntire(line)?.let { match ->
                globalFacts += AndroidAuthorityLevel.fromAtom(match.groupValues[1])
                return@forEach
            }
            backendFact.matchEntire(line)?.let { match ->
                val backend = AndroidBackend.fromAtom(match.groupValues[1])
                val level = AndroidAuthorityLevel.fromAtom(match.groupValues[2])
                require(backend !in backendFacts) { "duplicate Android backend authority fact" }
                backendFacts[backend] = level
                return@forEach
            }
            confirmationFact.matchEntire(line)?.let { match ->
                val level = AndroidAuthorityLevel.fromAtom(match.groupValues[1])
                val mode = AndroidConfirmationMode.fromAtom(match.groupValues[2])
                require(level !in confirmationFacts) { "duplicate Android confirmation fact" }
                confirmationFacts[level] = mode
                return@forEach
            }
            if (authorityPredicate.containsMatchIn(line)) {
                throw IllegalArgumentException("malformed Android authority fact")
            }
        }

        require(globalFacts.distinct().size <= 1) { "conflicting Android authority facts" }
        val global = globalFacts.firstOrNull() ?: AndroidAuthorityLevel.STANDARD
        val confirmation = confirmationFacts[global] ?: when (global) {
            AndroidAuthorityLevel.LOCKED -> AndroidConfirmationMode.ALWAYS
            AndroidAuthorityLevel.STANDARD -> AndroidConfirmationMode.SENSITIVE
            AndroidAuthorityLevel.ELEVATED -> AndroidConfirmationMode.SENSITIVE
            AndroidAuthorityLevel.UNRESTRICTED -> AndroidConfirmationMode.NONE
        }
        return AndroidAuthoritySnapshot(global, backendFacts.toMap(), confirmation)
    }

    private fun logicalLines(sources: List<String>): Sequence<String> = sequence {
        sources.forEach { source ->
            source.lineSequence().forEach { rawLine ->
                val line = rawLine.substringBefore('%').trim()
                if (line.isNotEmpty()) yield(line)
            }
        }
    }
}
