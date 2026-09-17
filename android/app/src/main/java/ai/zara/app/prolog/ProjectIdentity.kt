package ai.zara.app.prolog

data class PrologProjectIdentity(
    val projectName: String? = null,
    val llmAppName: String? = null,
) {
    fun effectiveProjectName(): String = projectName?.takeIf(String::isNotBlank) ?: DEFAULT_PROJECT_NAME

    fun effectiveLlmAppName(configuredName: String? = null): String =
        llmAppName?.takeIf(String::isNotBlank)
            ?: projectName?.takeIf(String::isNotBlank)
            ?: configuredName?.takeIf(String::isNotBlank)
            ?: DEFAULT_PROJECT_NAME

    fun defaultWakeWords(): List<String> {
        val name = effectiveProjectName().trim()
        return if (name.equals(DEFAULT_PROJECT_NAME, ignoreCase = true)) {
            LEGACY_ZARA_WAKE_WORDS
        } else {
            listOf("hey ${name.lowercase()}", name.lowercase()).distinct()
        }
    }

    companion object {
        const val DEFAULT_PROJECT_NAME = "Zara"
        val LEGACY_ZARA_WAKE_WORDS = listOf(
            "zarathushtra",
            "zarathustra",
            "hey zara",
            "zara",
            "sarah",
            "sara",
        )
    }
}

object PrologProjectIdentityResolver {
    private val identityFact = Regex(
        """(?m)^\s*(project_name|llm_app_name)\s*\(\s*(?:"((?:\\.|[^"\\])*)"|'((?:\\.|[^'\\])*)'|([a-z][a-zA-Z0-9_-]*))\s*\)\s*\.\s*(?:%.*)?$""",
    )

    fun resolve(sources: List<PrologSource>): PrologProjectIdentity {
        var projectName: String? = null
        var llmAppName: String? = null
        sources.forEach { source ->
            identityFact.findAll(source.text).forEach { match ->
                val value = decodeValue(match)
                if (value.isNotBlank()) {
                    when (match.groupValues[1]) {
                        "project_name" -> projectName = value
                        "llm_app_name" -> llmAppName = value
                    }
                }
            }
        }
        return PrologProjectIdentity(projectName = projectName, llmAppName = llmAppName)
    }

    private fun decodeValue(match: MatchResult): String {
        val raw = sequenceOf(
            match.groupValues[2],
            match.groupValues[3],
            match.groupValues[4],
        ).firstOrNull(String::isNotEmpty).orEmpty()
        return raw
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\\"", "\"")
            .replace("\\'", "'")
            .replace("\\\\", "\\")
            .trim()
    }
}
