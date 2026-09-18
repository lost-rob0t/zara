package ai.zara.app.prolog

/**
 * Authority boundary for Android Prolog inputs.
 *
 * Operator-authored workspace files are executable configuration. They are structurally validated
 * before consult, but they are not globally reduced to a facts-only sandbox. Full effectful/meta
 * validation is reserved for untrusted inputs (for example imported/model-provided source) and the
 * bounded ad-hoc query surface.
 */
object PrologAuthorityPolicy {
    private val forbiddenNames = setOf(
        "abolish",
        "access_file",
        "absolute_file_name",
        "apply",
        "assert",
        "asserta",
        "assertz",
        "bagof",
        "call",
        "call_cleanup",
        "catch",
        "clause",
        "close",
        "consult",
        "current_predicate",
        "delete_directory",
        "delete_file",
        "directory_files",
        "engine_create",
        "ensure_loaded",
        "exclude",
        "findall",
        "foldl",
        "forall",
        "foreign_struct",
        "future",
        "geturl",
        "get_url",
        "goal_expansion",
        "halt",
        "ignore",
        "include",
        "initialization",
        "load_files",
        "load_foreign_library",
        "make_directory",
        "maplist",
        "not",
        "once",
        "open",
        "partition",
        "phrase",
        "phrase_from_file",
        "process_create",
        "read",
        "rename_file",
        "retract",
        "retractall",
        "scanl",
        "set_prolog_flag",
        "setof",
        "setup_call_cleanup",
        "shell",
        "sleep",
        "system",
        "task_create",
        "term_expansion",
        "thread_create",
        "thread_sleep",
        "throw",
        "use_foreign_module",
        "use_module",
        "working_directory",
        "write",
    )

    private val forbiddenPrefixes = listOf(
        "curl_",
        "ffi_",
        "foreign_",
        "http_",
        "https_",
        "pl_thread",
        "process_",
        "socket",
        "sqlite3_",
        "tcp_",
        "udp_",
    )

    private val callPattern = Regex(
        "(?:^|[^A-Za-z0-9_])(?:[a-z][A-Za-z0-9_]*:)?([a-z][A-Za-z0-9_]*)\\s*(?=\\()",
    )
    private val quotedCallPattern = Regex(
        "'([A-Za-z][A-Za-z0-9_]*)'\\s*(?=\\()",
    )
    private val bareHaltPattern = Regex(
        "(?:^|[,;!])\\s*(?:[a-z][A-Za-z0-9_]*:)?halt\\s*(?=$|[,;!])",
        RegexOption.IGNORE_CASE,
    )
    private val dynamicTermConstruction = Regex("=\\s*\\.\\.")
    private val dynamicQualifiedGoal = Regex(
        "(?:[A-Z_][A-Za-z0-9_]*\\s*:\\s*[A-Z_][A-Za-z0-9_]*|[a-z][A-Za-z0-9_]*\\s*:\\s*[A-Z_][A-Za-z0-9_]*)",
    )
    private val dynamicBareGoal = Regex(
        "(?:^|[,;]|->)\\s*([A-Z_][A-Za-z0-9_]*)\\s*(?=(?:[,;]|->|$))",
    )
    private val parenthesizedDynamicGoal = Regex(
        "(?<![A-Za-z0-9_])\\(\\s*[A-Z_][A-Za-z0-9_]*\\s*\\)",
    )
    private val negationMetaGoal = Regex("""\\\+""")
    private val safeSchemaDirective = Regex(
        "^zara_schema\\(\\s*[a-z][A-Za-z0-9_]*\\s*,\\s*[0-9]{1,3}\\s*,\\s*\\[[^]]*]\\s*\\)$",
    )
    private val safeOperatorDirective = Regex(
        "^op\\(\\s*(?:[1-9][0-9]{0,2}|1[01][0-9]{2}|1200)\\s*,\\s*(?:xfx|xfy|yfx|fx|fy|xf|yf)\\s*,\\s*([a-z][A-Za-z0-9_]*)\\s*\\)$",
    )

    fun validate(source: PrologSource): List<PrologDiagnostic> =
        validate(PrologSourceAnalyzer.analyze(source.name, source.text))

    /** Full fail-closed validation for source that has not crossed an operator trust boundary. */
    fun validate(document: PrologDocument): List<PrologDiagnostic> {
        val diagnostics = validateTrusted(document).toMutableList()

        document.clauses.forEach { clause ->
            when (clause.kind) {
                PrologClauseKind.DIRECTIVE -> validateDirective(clause)?.let(diagnostics::add)
                PrologClauseKind.FACT -> {
                    if (isForbidden(clause.predicate.name)) {
                        diagnostics += PrologDiagnostic(
                            clause.line,
                            "Reserved effectful predicate cannot be defined: ${clause.predicate.indicator}",
                        )
                    }
                    unsafeCalls(clause.text).forEach { name ->
                        diagnostics += PrologDiagnostic(
                            clause.line,
                            "Effectful or executable meta term is not available in untrusted facts: $name",
                        )
                    }
                }
                PrologClauseKind.RULE -> {
                    if (isForbidden(clause.predicate.name)) {
                        diagnostics += PrologDiagnostic(
                            clause.line,
                            "Reserved effectful predicate cannot be defined: ${clause.predicate.indicator}",
                        )
                    }
                    val body = clause.text.substringAfter(":-", "")
                    unsafeCalls(body).forEach { name ->
                        diagnostics += PrologDiagnostic(
                            clause.line,
                            "Effectful or meta predicate is not available in untrusted source: $name",
                        )
                    }
                }
            }
        }
        return diagnostics.distinct()
    }

    fun validateTrusted(source: PrologSource): List<PrologDiagnostic> =
        validateTrusted(PrologSourceAnalyzer.analyze(source.name, source.text))

    /** Syntax/schema validation that deliberately preserves trusted operator execution authority. */
    fun validateTrusted(document: PrologDocument): List<PrologDiagnostic> =
        (document.diagnostics + PrologSchemaValidator.validate(document)).distinct()

    fun requireValidWorkspace(sources: List<PrologSource>) {
        val violations = sources.flatMap { source ->
            validateTrusted(source).map { diagnostic ->
                "${source.name}:${diagnostic.line}: ${diagnostic.message}"
            }
        }
        require(violations.isEmpty()) {
            violations.take(12).joinToString("; ")
        }
    }

    fun requireSafeUntrustedWorkspace(sources: List<PrologSource>) {
        val violations = sources.flatMap { source ->
            validate(source).map { diagnostic ->
                "${source.name}:${diagnostic.line}: ${diagnostic.message}"
            }
        }
        require(violations.isEmpty()) {
            violations.take(12).joinToString("; ")
        }
    }

    /** Compatibility name for callers that explicitly mean an untrusted workspace boundary. */
    fun requireSafeWorkspace(sources: List<PrologSource>) = requireSafeUntrustedWorkspace(sources)

    fun requireSafeQuery(query: String): String {
        val calls = unsafeCalls(query)
        require(calls.isEmpty()) {
            "This predicate is not available in the bounded mobile workspace: ${calls.first()}"
        }
        return query
    }

    private fun validateDirective(clause: PrologClause): PrologDiagnostic? {
        val directive = clause.text
            .removeSuffix(".")
            .trim()
            .removePrefix(":-")
            .trim()

        if (safeSchemaDirective.matches(directive)) return null
        val operator = safeOperatorDirective.matchEntire(directive)
        if (operator != null) {
            val name = operator.groupValues[1]
            return if (isForbidden(name)) {
                PrologDiagnostic(clause.line, "Effectful predicate cannot be exposed as an operator: $name")
            } else {
                null
            }
        }
        return PrologDiagnostic(
            clause.line,
            "Only zara_schema/3 and validated op/3 directives are allowed in untrusted source",
        )
    }

    private fun unsafeCalls(text: String): List<String> {
        val names = mutableListOf<String>()
        quotedCallPattern.findAll(text).forEach { match ->
            val name = match.groupValues[1].lowercase()
            if (isForbidden(name)) names += name
        }

        val code = maskQuotedAndComments(text)
        names += callPattern.findAll(code)
            .map { it.groupValues[1].lowercase() }
            .filter(::isForbidden)
            .toList()
        if (bareHaltPattern.containsMatchIn(code)) names += "halt"
        if (dynamicTermConstruction.containsMatchIn(code)) names += "=../2"
        if (dynamicQualifiedGoal.containsMatchIn(code)) names += "dynamic module goal"
        if (dynamicBareGoal.containsMatchIn(code)) names += "dynamic variable goal"
        if (parenthesizedDynamicGoal.containsMatchIn(code)) names += "parenthesized dynamic variable goal"
        if (negationMetaGoal.containsMatchIn(code)) names += "\\+/1 meta goal"
        return names.distinct()
    }

    private fun isForbidden(rawName: String): Boolean {
        val name = rawName.substringAfterLast(':').lowercase()
        if (name in forbiddenNames || forbiddenPrefixes.any(name::startsWith)) return true
        if (name.startsWith("file_") || name.endsWith("_file") || "_file_" in name) return true
        if (name.startsWith("directory_") || name.endsWith("_directory") || "_directory_" in name) return true
        return false
    }

    private fun maskQuotedAndComments(text: String): String {
        val output = StringBuilder(text.length)
        var quote: Char? = null
        var escaped = false
        var comment = false
        text.forEach { character ->
            when {
                comment -> {
                    if (character == '\n') {
                        comment = false
                        output.append('\n')
                    } else {
                        output.append(' ')
                    }
                }
                quote != null -> {
                    output.append(if (character == '\n') '\n' else ' ')
                    if (escaped) {
                        escaped = false
                    } else if (character == '\\') {
                        escaped = true
                    } else if (character == quote) {
                        quote = null
                    }
                }
                character == '%' -> {
                    comment = true
                    output.append(' ')
                }
                character == '\'' || character == '"' -> {
                    quote = character
                    output.append(' ')
                }
                else -> output.append(character)
            }
        }
        return output.toString()
    }
}
