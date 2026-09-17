package ai.zara.app.localai

import java.util.concurrent.ConcurrentHashMap

object LocalPromptContexts {
    private const val MAX_CONTEXTS = 32
    private const val MAX_CONTEXT_CHARS = 8_192
    private const val MAX_TOTAL_CHARS = 24_576

    private val contexts = ConcurrentHashMap<String, String>()

    @Synchronized
    fun register(name: String, context: String) {
        require(name.matches(Regex("[A-Za-z0-9._-]{1,64}"))) { "Prompt context name is invalid" }
        val bounded = context.trim()
        require(bounded.isNotEmpty()) { "Prompt context is empty" }
        require(bounded.length <= MAX_CONTEXT_CHARS) { "Prompt context is too large" }
        val previous = contexts[name]
        if (previous == null) {
            require(contexts.size < MAX_CONTEXTS) { "Prompt context limit reached" }
        }
        contexts[name] = bounded
        if (totalCharacters() > MAX_TOTAL_CHARS) {
            if (previous == null) {
                contexts.remove(name, bounded)
            } else {
                contexts[name] = previous
            }
            throw IllegalArgumentException("Aggregate prompt context limit reached")
        }
    }

    @Synchronized
    fun unregister(name: String): Boolean = contexts.remove(name) != null

    fun snapshot(): Map<String, String> = contexts.toSortedMap()

    fun apply(request: LocalGenerationRequest): LocalGenerationRequest {
        val prefix = snapshot().entries.joinToString("\n\n") { (name, context) ->
            "[Zara capability context: $name]\n$context"
        }
        if (prefix.isBlank()) return request
        val prompt = "$prefix\n\n${request.prompt}"
        require(prompt.length <= 32_768) { "Prompt plus capability context exceeds local model limit" }
        return request.copy(prompt = prompt)
    }

    @Synchronized
    internal fun clearForTests() {
        contexts.clear()
    }

    private fun totalCharacters(): Int = contexts.values.sumOf(String::length)
}
