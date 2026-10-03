package ai.zara.app.prolog

data class PrologFormArgument(
    val name: String,
    val type: String,
) {
    init {
        require(name.matches(Regex("[a-z][a-z0-9_]{0,31}"))) { "Invalid Prolog argument name" }
        require(type in SUPPORTED_TYPES) { "Invalid Prolog argument type" }
    }

    val variable: String
        get() = name.split('_').joinToString("") { part -> part.replaceFirstChar(Char::uppercaseChar) }

    companion object {
        val SUPPORTED_TYPES = setOf("atom", "integer", "number", "string", "list", "term")
    }
}

data class PrologPredicateSignature(
    val predicate: PredicateRef,
    val arguments: List<PrologFormArgument>,
    val source: String,
    val line: Int,
) {
    val detail: String
        get() {
            val args = arguments.joinToString(", ") { "${it.name}:${it.type}" }
            return if (args.isBlank()) "$source:$line" else "$args · $source:$line"
        }
}

object PrologFormBuilder {
    private val predicateName = Regex("[a-z][A-Za-z0-9_]{0,63}")
    private val atom = Regex("[a-z][A-Za-z0-9_]*")
    private val number = Regex("-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?")
    private val quotedAtom = Regex("'(?:\\\\.|[^'\\\\])*'")
    private val string = Regex("\"(?:\\\\.|[^\"\\\\])*\"")

    fun schema(predicate: String, arguments: List<PrologFormArgument>): String {
        val name = requirePredicate(predicate)
        require(arguments.size <= 16) { "Schema arity exceeds 16" }
        val types = arguments.joinToString(", ") { it.type }
        val signature = arguments.joinToString(", ") { "${it.name}:${it.type}" }
        return buildString {
            append("% zara_signature(").append(name).append('/').append(arguments.size)
            append(", [").append(signature).append("]).\n")
            append(":- zara_schema(").append(name).append(", ").append(arguments.size)
            append(", [").append(types).append("]).\n")
        }
    }

    fun fact(predicate: String, values: List<String>): String {
        val name = requirePredicate(predicate)
        require(values.size <= 16) { "Fact arity exceeds 16" }
        values.forEach(::requireDataTerm)
        return if (values.isEmpty()) "$name.\n" else "$name(${values.joinToString(", ")}).\n"
    }

    fun rule(predicate: String, arguments: List<PrologFormArgument>, body: String): String {
        val name = requirePredicate(predicate)
        require(arguments.size <= 16) { "Rule arity exceeds 16" }
        val cleanBody = body.trim()
        require(cleanBody.isNotBlank()) { "Rule body is required" }
        require(cleanBody.length <= 4_096) { "Rule body is too large" }
        require('%' !in cleanBody && ":-" !in cleanBody && "?-" !in cleanBody) {
            "Rule body may contain goals only"
        }
        require(!cleanBody.trimEnd().endsWith('.')) { "Rule body must not include the final period" }
        PrologAuthorityPolicy.requireSafeQuery(cleanBody)
        val head = if (arguments.isEmpty()) name else {
            "$name(${arguments.joinToString(", ") { it.variable }})"
        }
        val source = "$head :-\n    ${cleanBody.replace("\n", "\n    ")}.\n"
        val diagnostics = PrologSourceAnalyzer.analyze("builder.pl", source).diagnostics
        require(diagnostics.isEmpty()) { diagnostics.joinToString("; ") { it.message } }
        return source
    }

    fun parseArguments(specification: String): List<PrologFormArgument> {
        if (specification.isBlank()) return emptyList()
        return specification.split(',').map { item ->
            val parts = item.trim().split(':', limit = 2)
            require(parts.size == 2) { "Arguments use name:type syntax" }
            PrologFormArgument(parts[0].trim(), parts[1].trim())
        }
    }

    fun parseFactValues(values: String): List<String> = splitTopLevel(values)

    private fun requirePredicate(raw: String): String {
        val name = raw.trim()
        require(predicateName.matches(name)) { "Invalid predicate name" }
        PrologAuthorityPolicy.requireSafeQuery("$name(Result)")
        return name
    }

    private fun requireDataTerm(raw: String) {
        val value = raw.trim()
        require(value.isNotBlank() && value.length <= 512) { "Invalid fact value" }
        require('\n' !in value && '\r' !in value && '%' !in value && ":-" !in value && "?-" !in value) {
            "Fact values must be data terms"
        }
        val safe = when {
            atom.matches(value) -> true
            number.matches(value) -> true
            quotedAtom.matches(value) -> true
            string.matches(value) -> true
            value.startsWith('[') && value.endsWith(']') -> {
                val inner = value.substring(1, value.length - 1)
                splitTopLevel(inner).all { element ->
                    runCatching { requireDataTerm(element) }.isSuccess
                }
            }
            else -> false
        }
        require(safe) { "Fact values support atoms, numbers, quoted text, and lists" }
    }

    private fun splitTopLevel(text: String): List<String> {
        if (text.isBlank()) return emptyList()
        val results = mutableListOf<String>()
        var round = 0
        var square = 0
        var quote: Char? = null
        var escaped = false
        var start = 0
        text.forEachIndexed { index, character ->
            if (quote != null) {
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == quote) quote = null
                return@forEachIndexed
            }
            when (character) {
                '\'', '"' -> quote = character
                '(' -> round += 1
                ')' -> round -= 1
                '[' -> square += 1
                ']' -> square -= 1
                ',' -> if (round == 0 && square == 0) {
                    results += text.substring(start, index).trim()
                    start = index + 1
                }
            }
            require(round >= 0 && square >= 0) { "Unbalanced fact values" }
        }
        require(quote == null && round == 0 && square == 0) { "Unbalanced fact values" }
        results += text.substring(start).trim()
        require(results.none(String::isBlank)) { "Empty fact value" }
        return results
    }
}

object PrologSignatureCatalog {
    private val schemaPattern = Regex(
        "^\\s*:-\\s*zara_schema\\(\\s*([a-z][A-Za-z0-9_]*)\\s*,\\s*(\\d+)\\s*,\\s*\\[([^]]*)]\\s*\\)\\s*\\.$",
    )
    private val signaturePattern = Regex(
        "^\\s*%\\s*zara_signature\\(\\s*([a-z][A-Za-z0-9_]*)/(\\d+)\\s*,\\s*\\[([^]]*)]\\s*\\)\\.\\s*$",
    )

    fun from(documents: List<PrologDocument>): Map<PredicateRef, PrologPredicateSignature> {
        val signatures = linkedMapOf<PredicateRef, PrologPredicateSignature>()
        documents.forEach { document ->
            val explicitNames = document.text.lineSequence().mapIndexedNotNull { index, line ->
                val match = signaturePattern.matchEntire(line) ?: return@mapIndexedNotNull null
                val predicate = PredicateRef(match.groupValues[1], match.groupValues[2].toInt())
                val names = match.groupValues[3].split(',').map(String::trim).filter(String::isNotBlank)
                predicate to (index + 1 to names)
            }.toMap()

            document.text.lineSequence().forEachIndexed { index, line ->
                val match = schemaPattern.matchEntire(line) ?: return@forEachIndexed
                val predicate = PredicateRef(match.groupValues[1], match.groupValues[2].toInt())
                val types = match.groupValues[3].split(',').map(String::trim).filter(String::isNotBlank)
                if (types.size != predicate.arity || types.any { it !in PrologFormArgument.SUPPORTED_TYPES }) {
                    return@forEachIndexed
                }
                val explicit = explicitNames[predicate]?.second.orEmpty()
                val arguments = types.mapIndexed { argumentIndex, type ->
                    val named = explicit.getOrNull(argumentIndex)?.substringBefore(':')?.trim().orEmpty()
                    val name = named.takeIf { it.matches(Regex("[a-z][a-z0-9_]{0,31}")) }
                        ?: "arg${argumentIndex + 1}"
                    PrologFormArgument(name, type)
                }
                signatures[predicate] = PrologPredicateSignature(
                    predicate = predicate,
                    arguments = arguments,
                    source = document.source,
                    line = index + 1,
                )
            }

            document.clauses.filter { it.kind != PrologClauseKind.DIRECTIVE }.forEach { clause ->
                if (clause.predicate !in signatures) {
                    signatures[clause.predicate] = PrologPredicateSignature(
                        predicate = clause.predicate,
                        arguments = (1..clause.predicate.arity).map { PrologFormArgument("arg$it", "term") },
                        source = clause.source,
                        line = clause.line,
                    )
                }
            }
        }
        return signatures
    }
}
