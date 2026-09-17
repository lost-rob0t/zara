package ai.zara.app.prolog

import kotlin.math.sqrt

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
        text.lowercase().split(Regex("[^a-z0-9_]+"))
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
