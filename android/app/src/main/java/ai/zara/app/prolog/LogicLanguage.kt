package ai.zara.app.prolog

import java.net.URI
import java.util.Locale
import kotlin.math.sqrt

enum class PrologTokenKind { COMMENT, DIRECTIVE, VARIABLE, ATOM, NUMBER, STRING, OPERATOR, PUNCTUATION }

data class PrologToken(val kind: PrologTokenKind, val start: Int, val endExclusive: Int) {
    fun text(source: String): String = source.substring(start, endExclusive)
}

object PrologLexer {
    fun lex(source: String): List<PrologToken> {
        val tokens = mutableListOf<PrologToken>()
        var index = 0
        while (index < source.length) {
            val start = index
            val character = source[index]
            when {
                character.isWhitespace() -> index += 1
                character == '%' -> {
                    index = source.indexOf('\n', index).let { if (it < 0) source.length else it }
                    tokens += PrologToken(PrologTokenKind.COMMENT, start, index)
                }
                character == '\'' || character == '"' -> {
                    val quote = character
                    index += 1
                    var escaped = false
                    while (index < source.length) {
                        val next = source[index++]
                        if (escaped) escaped = false
                        else if (next == '\\') escaped = true
                        else if (next == quote) break
                    }
                    tokens += PrologToken(PrologTokenKind.STRING, start, index)
                }
                character.isDigit() -> {
                    index += 1
                    while (index < source.length && (source[index].isDigit() || source[index] == '.')) index += 1
                    tokens += PrologToken(PrologTokenKind.NUMBER, start, index)
                }
                character.isLetter() || character == '_' -> {
                    index += 1
                    while (index < source.length && (source[index].isLetterOrDigit() || source[index] == '_')) index += 1
                    val kind = if (character.isUpperCase() || character == '_') PrologTokenKind.VARIABLE else PrologTokenKind.ATOM
                    tokens += PrologToken(kind, start, index)
                }
                index + 1 < source.length && source.substring(index, index + 2) in setOf(":-", "?-", "->", "\\+") -> {
                    index += 2
                    tokens += PrologToken(PrologTokenKind.DIRECTIVE, start, index)
                }
                character in "=<>\\/+*-" -> {
                    index += 1
                    while (index < source.length && source[index] in "=<>\\/+*-") index += 1
                    tokens += PrologToken(PrologTokenKind.OPERATOR, start, index)
                }
                else -> {
                    index += 1
                    tokens += PrologToken(PrologTokenKind.PUNCTUATION, start, index)
                }
            }
        }
        return tokens
    }
}

data class PrologEditorSnapshot(val text: String, val cursor: Int)

data class PrologEditorHistory private constructor(
    private val undoStack: List<PrologEditorSnapshot>,
    val current: PrologEditorSnapshot,
    private val redoStack: List<PrologEditorSnapshot>,
) {
    val canUndo: Boolean get() = undoStack.isNotEmpty()
    val canRedo: Boolean get() = redoStack.isNotEmpty()

    fun edit(text: String, cursor: Int): PrologEditorHistory {
        if (text == current.text && cursor == current.cursor) return this
        return PrologEditorHistory((undoStack + current).takeLast(MAX_HISTORY), PrologEditorSnapshot(text, cursor.coerceIn(0, text.length)), emptyList())
    }

    fun undo(): PrologEditorHistory {
        if (!canUndo) return this
        return PrologEditorHistory(undoStack.dropLast(1), undoStack.last(), listOf(current) + redoStack)
    }

    fun redo(): PrologEditorHistory {
        if (!canRedo) return this
        return PrologEditorHistory(undoStack + current, redoStack.first(), redoStack.drop(1))
    }

    companion object {
        private const val MAX_HISTORY = 100
        fun initial(text: String, cursor: Int = text.length) = PrologEditorHistory(emptyList(), PrologEditorSnapshot(text, cursor.coerceIn(0, text.length)), emptyList())
    }
}

data class PrologSearchMatch(val source: String, val line: Int, val column: Int, val start: Int, val endExclusive: Int)

object PrologSearch {
    fun find(sources: List<PrologSource>, query: String, limit: Int = 100): List<PrologSearchMatch> {
        require(query.isNotEmpty()) { "Search query is required" }
        require(limit in 1..500) { "Search result limit is invalid" }
        val matches = mutableListOf<PrologSearchMatch>()
        sources.forEach { source ->
            var offset = 0
            while (matches.size < limit) {
                val found = source.text.indexOf(query, offset, ignoreCase = true)
                if (found < 0) break
                val prefix = source.text.substring(0, found)
                val lineStart = prefix.lastIndexOf('\n') + 1
                matches += PrologSearchMatch(source.name, prefix.count { it == '\n' } + 1, found - lineStart + 1, found, found + query.length)
                offset = found + query.length
            }
        }
        return matches
    }
}

data class PrologWorkspaceCatalog(
    val facts: List<PredicateRef>,
    val rules: List<PredicateRef>,
    val schemas: List<PredicateRef>,
    val experts: List<PredicateRef>,
    val activations: Map<String, String>,
) {
    companion object {
        private val schemaPattern = Regex("(?m)^\\s*:-\\s*zara_schema\\(\\s*([a-z][A-Za-z0-9_]*)\\s*,\\s*(\\d+)")
        private val activationPattern = Regex("(?m)^\\s*expert_activation\\(\\s*([a-z][A-Za-z0-9_]*)\\s*,\\s*([a-z][A-Za-z0-9_]*)\\s*\\)\\s*\\.")

        fun from(sources: List<PrologSource>): PrologWorkspaceCatalog {
            val documents = sources.map { PrologSourceAnalyzer.analyze(it.name, it.text) }
            val facts = documents.flatMap { it.clauses }.filter { it.kind == PrologClauseKind.FACT }.map { it.predicate }.distinctBy { it.indicator }.sortedBy { it.indicator }
            val rules = documents.flatMap { it.clauses }.filter { it.kind == PrologClauseKind.RULE }.map { it.predicate }.distinctBy { it.indicator }.sortedBy { it.indicator }
            val schemas = sources.flatMap { source ->
                schemaPattern.findAll(source.text).map { PredicateRef(it.groupValues[1], it.groupValues[2].toInt()) }.toList()
            }.distinctBy { it.indicator }.sortedBy { it.indicator }
            val experts = rules.filter { it.arity == 2 && it.name.endsWith("_explain") }
            val activations = sources.flatMap { source ->
                activationPattern.findAll(source.text).map { it.groupValues[2] to it.groupValues[1] }.toList()
            }.groupBy({ it.first }, { it.second }).mapValues { (_, expertsForWord) ->
                require(expertsForWord.distinct().size == 1) { "Expert activation collision" }
                expertsForWord.first()
            }
            return PrologWorkspaceCatalog(facts, rules, schemas, experts, activations)
        }
    }
}

data class NaturalLanguageExpertSelection(
    val expertId: String,
    val query: String,
)

object LocalNaturalLanguageExpertRouter {
    private val utterance = Regex("^([a-z][A-Za-z0-9_]*)\\s+([a-z][A-Za-z0-9_]*)$")

    fun select(text: String, catalog: PrologWorkspaceCatalog): NaturalLanguageExpertSelection? {
        val match = utterance.matchEntire(text.trim().lowercase(Locale.ROOT)) ?: return null
        val expertId = catalog.activations[match.groupValues[1]] ?: return null
        val predicate = PredicateRef("${expertId}_explain", 2)
        if (predicate !in catalog.experts) return null
        return NaturalLanguageExpertSelection(
            expertId = expertId,
            query = "${predicate.name}(${match.groupValues[2]}, Result)",
        )
    }

    fun query(text: String, catalog: PrologWorkspaceCatalog): String? =
        select(text, catalog)?.query
}

data class LocalPrologCommand(val query: String) {
    companion object {
        private val expertCommand = Regex("^/expert\\s+([a-z][A-Za-z0-9_]*)\\s+([a-z][A-Za-z0-9_]*)$")

        fun parse(text: String, catalog: PrologWorkspaceCatalog): LocalPrologCommand {
            val trimmed = text.trim()
            if (trimmed.startsWith("/prolog ")) {
                return LocalPrologCommand(PrologQueryPolicy.requireSafe(trimmed.removePrefix("/prolog ")))
            }
            val match = expertCommand.matchEntire(trimmed)
                ?: throw IllegalArgumentException("Use /expert PREDICATE ENTITY")
            val predicate = PredicateRef(match.groupValues[1], 2)
            require(catalog.experts.any { it == predicate }) { "Expert entry is not declared in the private workspace" }
            return LocalPrologCommand("${predicate.name}(${match.groupValues[2]}, Result)")
        }
    }
}

enum class IntentDraftProviderKind { DETERMINISTIC, ON_DEVICE_LLM, REMOTE }

data class IntentArgument(val name: String, val type: String) {
    init {
        require(name.matches(Regex("[a-z][a-z0-9_]{0,31}"))) { "Invalid intent argument name" }
        require(type in setOf("atom", "integer", "number", "string", "list", "term")) { "Invalid intent argument type" }
    }
}

data class IntentHelperRequest(
    val intent: String,
    val actionWords: List<String>,
    val arguments: List<IntentArgument>,
) {
    init {
        require(intent.matches(Regex("[a-z][a-z0-9_]{0,31}"))) { "Invalid intent name" }
        require(actionWords.isNotEmpty() && actionWords.size <= 32) { "Action-word count must be 1 through 32" }
        require(actionWords.all { it.matches(Regex("[a-z][a-z0-9_]{0,31}")) }) { "Invalid action word" }
        require(arguments.size <= 8) { "Intent argument count exceeds 8" }
    }
}

data class IntentHelperDraft(
    val provider: IntentDraftProviderKind,
    val model: String,
    val source: String,
    val requiresApproval: Boolean = true,
)

data class IntentGeneratorConfiguration private constructor(
    val provider: IntentDraftProviderKind,
    val endpoint: String?,
    val model: String,
) {
    companion object {
        fun deterministic() = IntentGeneratorConfiguration(IntentDraftProviderKind.DETERMINISTIC, null, "zara-intent-compiler-1")

        fun onDevice(model: String): IntentGeneratorConfiguration {
            require(model.matches(Regex("[A-Za-z0-9._/-]{1,128}"))) { "Invalid on-device model id" }
            return IntentGeneratorConfiguration(IntentDraftProviderKind.ON_DEVICE_LLM, null, model)
        }

        fun remote(endpoint: String, model: String): IntentGeneratorConfiguration {
            val uri = runCatching { URI(endpoint) }.getOrNull()
            require(uri != null && uri.scheme == "https" && uri.host != null && uri.userInfo == null) { "Remote intent endpoint must be credential-free HTTPS" }
            require(endpoint.length <= 2_048) { "Remote intent endpoint is too large" }
            require(model.matches(Regex("[A-Za-z0-9._/-]{1,128}"))) { "Invalid remote model id" }
            return IntentGeneratorConfiguration(IntentDraftProviderKind.REMOTE, endpoint, model)
        }
    }
}

object DeterministicIntentHelperGenerator {
    fun generate(request: IntentHelperRequest): IntentHelperDraft {
        val arity = request.arguments.size
        val types = request.arguments.joinToString(", ") { it.type }
        val variables = request.arguments.map { it.name.replaceFirstChar(Char::uppercaseChar) }
        val subject = variables.firstOrNull() ?: "Entity"
        val actionFacts = request.actionWords.distinct().joinToString("\n") { word ->
            "verb_intent($word, ${request.intent}, $arity)."
        }
        val schemaTypes = (request.arguments.map { it.type } + "term").joinToString(", ")
        val decisionArguments = (variables + "Decision").joinToString(", ")
        val source = buildString {
            append("% generated intent helper; review before activation\n")
            append(actionFacts).append("\n\n")
            if (arity > 0) append(":- zara_schema(${request.intent}, $arity, [$types]).\n")
            append(":- zara_schema(${request.intent}_explain, ${arity + 1}, [$schemaTypes]).\n\n")
            append("${request.intent}_explain($decisionArguments) :-\n")
            append("    Decision = intent(${request.intent}, $subject).\n")
        }
        return IntentHelperDraft(IntentDraftProviderKind.DETERMINISTIC, "zara-intent-compiler-1", source)
    }
}

data class PrologCompletion(
    val label: String,
    val insertion: String,
    val detail: String,
)

data class PrologPredicateSchema(
    val predicate: PredicateRef,
    val argumentTypes: List<String>,
    val line: Int,
)

object PrologSchemaValidator {
    private val schema = Regex(
        """^\s*:-\s*zara_schema\(\s*([a-z][A-Za-z0-9_]*)\s*,\s*(\d+)\s*,\s*\[([^]]*)]\s*\)\s*\.$""",
    )
    private val supportedTypes = setOf("atom", "integer", "number", "string", "list", "term")

    fun validate(document: PrologDocument): List<PrologDiagnostic> {
        val declared = document.text.lineSequence().mapIndexedNotNull { index, line ->
            val match = schema.matchEntire(line) ?: return@mapIndexedNotNull null
            val arity = match.groupValues[2].toInt()
            val types = match.groupValues[3].split(',').map(String::trim).filter(String::isNotEmpty)
            PrologPredicateSchema(PredicateRef(match.groupValues[1], arity), types, index + 1)
        }.toList()
        val diagnostics = mutableListOf<PrologDiagnostic>()
        declared.forEach { declaration ->
            if (declaration.argumentTypes.size != declaration.predicate.arity) {
                diagnostics += PrologDiagnostic(
                    declaration.line,
                    "${declaration.predicate.indicator} declares ${declaration.argumentTypes.size} argument types",
                )
            }
            declaration.argumentTypes.filterNot(supportedTypes::contains).forEach { type ->
                diagnostics += PrologDiagnostic(declaration.line, "Unknown schema type: $type")
            }
            document.clauses.filter {
                it.kind != PrologClauseKind.DIRECTIVE && it.predicate.name == declaration.predicate.name
            }.forEach { clause ->
                if (clause.predicate.arity != declaration.predicate.arity) {
                    diagnostics += PrologDiagnostic(
                        clause.line,
                        "${clause.predicate.indicator} violates declared ${declaration.predicate.indicator}",
                    )
                } else if (clause.kind == PrologClauseKind.FACT) {
                    validateFactArguments(clause, declaration).forEach(diagnostics::add)
                }
            }
        }
        return diagnostics.distinct()
    }

    private fun validateFactArguments(
        clause: PrologClause,
        declaration: PrologPredicateSchema,
    ): List<PrologDiagnostic> {
        val open = clause.text.indexOf('(')
        val close = clause.text.lastIndexOf(')')
        if (open < 0 || close <= open) return emptyList()
        val arguments = splitArguments(clause.text.substring(open + 1, close))
        return arguments.zip(declaration.argumentTypes).mapIndexedNotNull { index, (argument, type) ->
            if (matchesType(argument.trim(), type)) null else PrologDiagnostic(
                clause.line,
                "${declaration.predicate.indicator} argument ${index + 1} must be $type",
            )
        }
    }

    private fun splitArguments(text: String): List<String> {
        val result = mutableListOf<String>()
        var depth = 0
        var quoted = false
        var start = 0
        text.forEachIndexed { index, character ->
            if (character == '\'' && (index == 0 || text[index - 1] != '\\')) quoted = !quoted
            if (!quoted) when (character) {
                '(', '[' -> depth += 1
                ')', ']' -> depth -= 1
                ',' -> if (depth == 0) {
                    result += text.substring(start, index)
                    start = index + 1
                }
            }
        }
        result += text.substring(start)
        return result
    }

    private fun matchesType(value: String, type: String): Boolean = when (type) {
        "integer" -> value.toLongOrNull() != null
        "number" -> value.toDoubleOrNull() != null
        "string" -> value.startsWith('"') && value.endsWith('"')
        "list" -> value.startsWith('[') && value.endsWith(']')
        "atom" -> value.matches(Regex("[a-z][A-Za-z0-9_]*|'(?:\\\\.|[^'\\\\])*'"))
        "term" -> true
        else -> false
    }
}

object PrologCompletionEngine {
    private val builtIns = listOf(
        PrologCompletion("zara_schema/3", "zara_schema(\${1:name}, \${2:arity}, [\${3:types}])", "Zara predicate schema"),
        PrologCompletion("member/2", "member(\${1:Element}, \${2:List})", "ISO list membership"),
        PrologCompletion("length/2", "length(\${1:List}, \${2:Length})", "List length"),
        PrologCompletion("append/3", "append(\${1:Left}, \${2:Right}, \${3:Result})", "List append"),
        PrologCompletion("findall/3", "findall(\${1:Template}, \${2:Goal}, \${3:Results})", "Collect solutions"),
    )

    fun complete(text: String, cursor: Int, documents: List<PrologDocument>): List<PrologCompletion> {
        val prefix = text.take(cursor.coerceIn(0, text.length)).takeLastWhile { it.isLetterOrDigit() || it == '_' }
        val workspace = documents.flatMap { it.clauses }.map { it.predicate }.distinctBy { it.indicator }.map { ref ->
            val variables = (1..ref.arity).joinToString(", ") { index -> "\${" + index + ":Arg" + index + "}" }
            PrologCompletion(ref.indicator, "${ref.name}($variables)", "Workspace predicate")
        }
        return (workspace + builtIns)
            .distinctBy { it.label }
            .filter { prefix.isBlank() || it.label.startsWith(prefix, ignoreCase = true) }
            .sortedWith(compareBy<PrologCompletion> { !it.label.startsWith(prefix, true) }.thenBy { it.label })
            .take(12)
    }
}

data class LocalEmbeddingConfiguration(
    val enabled: Boolean = false,
    val modelVersion: String = "zara-token-hash-1",
    val dimensions: Int = 96,
)

object LocalEmbeddingModel {
    fun embed(text: String, configuration: LocalEmbeddingConfiguration): List<Float> {
        if (!configuration.enabled) return emptyList()
        require(configuration.modelVersion == "zara-token-hash-1") { "Unsupported local embedding model" }
        require(configuration.dimensions in 32..512) { "Invalid local embedding dimensions" }
        val vector = FloatArray(configuration.dimensions)
        text.lowercase(Locale.ROOT).split(Regex("[^a-z0-9_]+"))
            .filter(String::isNotBlank)
            .forEach { token ->
                val hash = token.fold(0x811c9dc5.toInt()) { value, character ->
                    (value xor character.code) * 0x01000193
                }
                val index = (hash and Int.MAX_VALUE) % vector.size
                vector[index] += if (hash < 0) -1f else 1f
            }
        val norm = sqrt(vector.sumOf { (it * it).toDouble() }).toFloat()
        if (norm > 0f) vector.indices.forEach { vector[it] /= norm }
        return vector.toList()
    }
}
