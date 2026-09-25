package ai.zara.app.prolog

import java.io.File
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

enum class PrologClauseKind { FACT, RULE, DIRECTIVE }

data class PrologDiagnostic(
    val line: Int,
    val message: String,
)

data class PredicateRef(
    val name: String,
    val arity: Int,
) {
    val indicator: String = "$name/$arity"
}

data class PrologClause(
    val kind: PrologClauseKind,
    val predicate: PredicateRef,
    val bodyPredicates: List<PredicateRef>,
    val source: String,
    val line: Int,
    val text: String,
)

enum class LogicNodeKind { PREDICATE, CLAUSE }

data class LogicGraphNode(
    val id: String,
    val label: String,
    val kind: LogicNodeKind,
    val source: String,
    val line: Int,
)

data class LogicGraphEdge(
    val from: String,
    val to: String,
    val label: String,
)

data class LogicGraph(
    val nodes: List<LogicGraphNode>,
    val edges: List<LogicGraphEdge>,
)

data class PrologDocument(
    val source: String,
    val text: String,
    val clauses: List<PrologClause>,
    val diagnostics: List<PrologDiagnostic>,
    val graph: LogicGraph,
)

object PrologQueryPolicy {
    private const val MAX_QUERY_LENGTH = 4_096
    private val resultVariable = Regex("\\bResult\\b")
    private val forbidden = Regex(
        "(?i)(^|[^a-zA-Z0-9_])(" +
            "consult|ensure_loaded|load_files|call|once|catch|throw|halt|shell|" +
            "open|close|read|write|process_create|assert|asserta|assertz|retract|" +
            "retractall|abolish|clause|current_predicate|set_prolog_flag|" +
            "working_directory|directory_files|delete_file|rename_file" +
            ")\\s*(\\(|$)",
    )

    fun requireSafe(raw: String): String {
        var query = raw.trim()
        require(query.isNotEmpty()) { "Prolog query is required" }
        require(query.length <= MAX_QUERY_LENGTH) { "Prolog query is too large" }
        require(query.none { it.code < 0x20 && it != '\n' && it != '\t' }) {
            "Prolog query contains control characters"
        }
        if (query.startsWith("?-")) query = query.removePrefix("?-").trim()
        if (query.endsWith('.')) query = query.dropLast(1).trimEnd()
        require(resultVariable.containsMatchIn(query)) {
            "A bounded query must bind the Result variable"
        }
        require(!forbidden.containsMatchIn(query)) {
            "This predicate is not available in the bounded mobile workspace"
        }
        require(balanced(query)) { "Prolog query has unbalanced delimiters" }
        return query
    }

    private fun balanced(text: String): Boolean {
        var round = 0
        var square = 0
        var quoted = false
        var escaped = false
        text.forEach { character ->
            if (quoted) {
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == '\'') quoted = false
                return@forEach
            }
            when (character) {
                '\'' -> quoted = true
                '(' -> round += 1
                ')' -> round -= 1
                '[' -> square += 1
                ']' -> square -= 1
            }
            if (round < 0 || square < 0) return false
        }
        return !quoted && round == 0 && square == 0
    }
}

object PrologSourceAnalyzer {
    fun analyze(source: String, text: String): PrologDocument {
        val diagnostics = mutableListOf<PrologDiagnostic>()
        scanBalance(text, diagnostics)
        val rawClauses = splitClauses(text, diagnostics)
        val clauses = rawClauses.mapNotNull { raw -> parseClause(source, raw) }
        val predicateNodes = linkedMapOf<String, LogicGraphNode>()
        val clauseNodes = mutableListOf<LogicGraphNode>()
        val edges = mutableListOf<LogicGraphEdge>()
        clauses.forEachIndexed { index, clause ->
            val predicateId = "predicate:${clause.predicate.indicator}"
            predicateNodes.putIfAbsent(
                predicateId,
                LogicGraphNode(
                    id = predicateId,
                    label = clause.predicate.indicator,
                    kind = LogicNodeKind.PREDICATE,
                    source = source,
                    line = clause.line,
                ),
            )
            val clauseId = "clause:$source:${clause.line}:$index"
            clauseNodes += LogicGraphNode(
                id = clauseId,
                label = clause.text.lineSequence().first().take(72),
                kind = LogicNodeKind.CLAUSE,
                source = source,
                line = clause.line,
            )
            edges += LogicGraphEdge(predicateId, clauseId, "defines")
            clause.bodyPredicates.forEach { target ->
                val targetId = "predicate:${target.indicator}"
                predicateNodes.putIfAbsent(
                    targetId,
                    LogicGraphNode(
                        id = targetId,
                        label = target.indicator,
                        kind = LogicNodeKind.PREDICATE,
                        source = source,
                        line = clause.line,
                    ),
                )
                edges += LogicGraphEdge(predicateId, targetId, "calls")
            }
        }
        return PrologDocument(
            source = source,
            text = text,
            clauses = clauses,
            diagnostics = diagnostics.distinct(),
            graph = LogicGraph(predicateNodes.values.toList() + clauseNodes, edges.distinct()),
        )
    }

    private data class RawClause(val text: String, val line: Int)

    private fun splitClauses(
        source: String,
        diagnostics: MutableList<PrologDiagnostic>,
    ): List<RawClause> {
        val clauses = mutableListOf<RawClause>()
        val current = StringBuilder()
        var line = 1
        var clauseLine = 1
        var round = 0
        var square = 0
        var quoted = false
        var escaped = false
        var comment = false
        source.forEach { character ->
            if (comment) {
                if (character == '\n') {
                    comment = false
                    line += 1
                    if (current.isNotBlank()) current.append(character)
                    else clauseLine = line
                }
                return@forEach
            }
            if (!quoted && character == '%') {
                comment = true
                return@forEach
            }
            current.append(character)
            if (quoted) {
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == '\'') quoted = false
            } else {
                when (character) {
                    '\'' -> quoted = true
                    '(' -> round += 1
                    ')' -> round -= 1
                    '[' -> square += 1
                    ']' -> square -= 1
                    '.' -> if (round == 0 && square == 0) {
                        val value = current.toString().trim()
                        if (value.isNotEmpty()) clauses += RawClause(value, clauseLine)
                        current.clear()
                        clauseLine = line
                    }
                }
            }
            if (character == '\n') {
                line += 1
                if (current.isBlank()) clauseLine = line
            }
        }
        if (current.isNotBlank()) {
            diagnostics += PrologDiagnostic(clauseLine, "Clause must end with a period")
        }
        return clauses
    }

    private fun scanBalance(text: String, diagnostics: MutableList<PrologDiagnostic>) {
        var round = 0
        var square = 0
        var quoted = false
        var escaped = false
        var line = 1
        text.forEach { character ->
            if (quoted) {
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == '\'') quoted = false
            } else {
                when (character) {
                    '\'' -> quoted = true
                    '(' -> round += 1
                    ')' -> round -= 1
                    '[' -> square += 1
                    ']' -> square -= 1
                }
            }
            if (character == '\n') line += 1
        }
        if (round != 0 || square != 0 || quoted) {
            diagnostics += PrologDiagnostic(line, "Unbalanced Prolog delimiters or quote")
        }
    }

    private fun parseClause(source: String, raw: RawClause): PrologClause? {
        val text = raw.text.removeSuffix(".").trim()
        if (text.isEmpty()) return null
        if (text.startsWith(":-")) {
            val predicate = PredicateRef("directive", 1)
            return PrologClause(
                PrologClauseKind.DIRECTIVE,
                predicate,
                bodyPredicateRefs(text.removePrefix(":-").trim()),
                source,
                raw.line,
                raw.text,
            )
        }
        val separator = topLevelRuleSeparator(text)
        val head = if (separator < 0) text else text.substring(0, separator).trim()
        val predicate = predicateRef(head) ?: return null
        val body = if (separator < 0) emptyList() else {
            bodyPredicateRefs(text.substring(separator + 2))
        }
        return PrologClause(
            kind = if (separator < 0) PrologClauseKind.FACT else PrologClauseKind.RULE,
            predicate = predicate,
            bodyPredicates = body,
            source = source,
            line = raw.line,
            text = raw.text,
        )
    }

    private fun topLevelRuleSeparator(text: String): Int {
        var round = 0
        var square = 0
        var quoted = false
        var index = 0
        while (index < text.length - 1) {
            val character = text[index]
            if (character == '\'' && (index == 0 || text[index - 1] != '\\')) quoted = !quoted
            if (!quoted) {
                when (character) {
                    '(' -> round += 1
                    ')' -> round -= 1
                    '[' -> square += 1
                    ']' -> square -= 1
                }
                if (round == 0 && square == 0 && character == ':' && text[index + 1] == '-') {
                    return index
                }
            }
            index += 1
        }
        return -1
    }

    private fun predicateRef(term: String): PredicateRef? {
        val nameMatch = Regex("^([a-z][a-zA-Z0-9_]*(?::[a-z][a-zA-Z0-9_]*)?)").find(term)
            ?: return null
        val name = nameMatch.groupValues[1]
        val open = term.indexOf('(', nameMatch.range.last + 1)
        if (open < 0) return PredicateRef(name, 0)
        val close = matchingClose(term, open) ?: return null
        val arguments = term.substring(open + 1, close)
        return PredicateRef(name, topLevelArity(arguments))
    }

    private fun bodyPredicateRefs(body: String): List<PredicateRef> {
        val results = mutableListOf<PredicateRef>()
        val matcher = Regex("([a-z][a-zA-Z0-9_]*(?::[a-z][a-zA-Z0-9_]*)?)\\s*\\(")
        matcher.findAll(body).forEach { match ->
            val open = body.indexOf('(', match.range.first)
            val close = matchingClose(body, open) ?: return@forEach
            val ref = PredicateRef(
                match.groupValues[1],
                topLevelArity(body.substring(open + 1, close)),
            )
            if (ref.name !in setOf("is", "true", "fail")) results += ref
        }
        return results.distinct()
    }

    private fun matchingClose(text: String, open: Int): Int? {
        var depth = 0
        var quoted = false
        for (index in open until text.length) {
            val character = text[index]
            if (character == '\'' && (index == 0 || text[index - 1] != '\\')) quoted = !quoted
            if (quoted) continue
            if (character == '(') depth += 1
            if (character == ')') {
                depth -= 1
                if (depth == 0) return index
            }
        }
        return null
    }

    private fun topLevelArity(arguments: String): Int {
        if (arguments.isBlank()) return 0
        var round = 0
        var square = 0
        var quoted = false
        var arity = 1
        arguments.forEachIndexed { index, character ->
            if (character == '\'' && (index == 0 || arguments[index - 1] != '\\')) quoted = !quoted
            if (quoted) return@forEachIndexed
            when (character) {
                '(' -> round += 1
                ')' -> round -= 1
                '[' -> square += 1
                ']' -> square -= 1
                ',' -> if (round == 0 && square == 0) arity += 1
            }
        }
        return arity
    }
}

data class PrologSource(
    val name: String,
    val text: String,
)

class PrologWorkspace(private val root: File) {
    init {
        check(root.mkdirs() || root.isDirectory) { "Prolog workspace is unavailable" }
    }

    fun listSources(): List<PrologSource> =
        root.listFiles()
            .orEmpty()
            .filter { it.isFile && it.name.matches(SOURCE_NAME) }
            .sortedBy { it.name }
            .map { PrologSource(it.name, it.readText()) }

    fun readSource(name: String): PrologSource {
        val file = sourceFile(name)
        check(file.isFile) { "Prolog source does not exist" }
        return PrologSource(file.name, file.readText())
    }

    fun saveSource(name: String, text: String): PrologSource {
        require(text.encodeToByteArray().size <= MAX_SOURCE_BYTES) { "Prolog source is too large" }
        val destination = sourceFile(name)
        val temporary = File.createTempFile(".${destination.name}.", ".tmp", root)
        try {
            temporary.writeText(text)
            try {
                Files.move(
                    temporary.toPath(),
                    destination.toPath(),
                    StandardCopyOption.ATOMIC_MOVE,
                    StandardCopyOption.REPLACE_EXISTING,
                )
            } catch (_: AtomicMoveNotSupportedException) {
                Files.move(
                    temporary.toPath(),
                    destination.toPath(),
                    StandardCopyOption.REPLACE_EXISTING,
                )
            }
        } finally {
            temporary.delete()
        }
        return PrologSource(destination.name, text)
    }

    fun seedExamples(examples: List<PrologExample>) {
        examples.forEach { example ->
            val destination = sourceFile(example.fileName)
            if (!destination.exists()) saveSource(example.fileName, example.source)
        }
    }

    fun deleteSource(name: String): Boolean {
        val file = sourceFile(name)
        return !file.exists() || file.delete()
    }

    fun renameSource(from: String, to: String): PrologSource {
        val source = sourceFile(from)
        check(source.isFile) { "Prolog source does not exist" }
        val destination = sourceFile(to)
        require(!destination.exists()) { "Prolog source already exists" }
        try {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.ATOMIC_MOVE)
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath())
        }
        return PrologSource(destination.name, destination.readText())
    }

    fun exportBundle(): String = buildString {
        append("ZARA-PROLOG-WORKSPACE/1\n")
        listSources().forEach { source ->
            val normalized = if (source.text.endsWith('\n')) source.text else source.text + "\n"
            val bytes = normalized.encodeToByteArray()
            append("SOURCE ").append(source.name).append(' ').append(bytes.size).append('\n')
            append(normalized)
            append("END-SOURCE\n")
        }
    }

    fun importBundle(bundle: String): List<PrologSource> {
        require(bundle.encodeToByteArray().size <= MAX_BUNDLE_BYTES) { "Workspace bundle is too large" }
        val lines = bundle.lines()
        require(lines.firstOrNull() == "ZARA-PROLOG-WORKSPACE/1") { "Unsupported workspace bundle" }
        val imported = mutableListOf<PrologSource>()
        val names = mutableSetOf<String>()
        var index = 1
        while (index < lines.size && lines[index].isNotEmpty()) {
            val header = BUNDLE_HEADER.matchEntire(lines[index])
                ?: throw IllegalArgumentException("Malformed workspace bundle header")
            val name = header.groupValues[1]
            val expectedBytes = header.groupValues[2].toInt()
            require(names.add(name)) { "Duplicate workspace source" }
            sourceFile(name)
            index += 1
            val body = mutableListOf<String>()
            while (index < lines.size && lines[index] != "END-SOURCE") {
                body += lines[index]
                index += 1
            }
            require(index < lines.size) { "Unterminated workspace source" }
            val text = body.joinToString("\n") + "\n"
            require(text.encodeToByteArray().size == expectedBytes) { "Workspace source length mismatch" }
            imported += PrologSource(name, text)
            index += 1
        }

        // Bundles cross an untrusted import boundary. Validate the complete bundle before the first
        // file is persisted so effectful/model-provided source cannot become executable merely by
        // being imported into the operator-owned workspace.
        PrologAuthorityPolicy.requireSafeUntrustedWorkspace(imported)
        imported.forEach { source -> saveSource(source.name, source.text) }
        return imported
    }

    fun sourceFiles(): List<File> = listSources().map { sourceFile(it.name) }

    private fun sourceFile(name: String): File {
        require(name.matches(SOURCE_NAME)) {
            "Prolog source names must be visible .pl basenames"
        }
        val canonicalRoot = root.canonicalFile
        val file = File(canonicalRoot, name).canonicalFile
        require(file.parentFile == canonicalRoot) { "Prolog source escaped the private workspace" }
        return file
    }

    companion object {
        private val SOURCE_NAME = Regex("[a-zA-Z][a-zA-Z0-9_-]{0,63}\\.pl")
        private const val MAX_SOURCE_BYTES = 512 * 1024
        private const val MAX_BUNDLE_BYTES = 4 * 1024 * 1024
        private val BUNDLE_HEADER = Regex("SOURCE ([a-zA-Z][a-zA-Z0-9_-]{0,63}\\.pl) ([0-9]{1,7})")
    }
}

data class PrologExample(
    val title: String,
    val summary: String,
    val fileName: String,
    val source: String,
    val query: String,
)

object PrologExampleCatalog {
    val examples = listOf(
        PrologExample(
            title = "Family recursion",
            summary = "Facts, two rule clauses, recursion, and variable bindings.",
            fileName = "family.pl",
            source = """
                parent(alice, bob).
                parent(bob, charlie).
                parent(charlie, dana).

                ancestor(X, Y) :- parent(X, Y).
                ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
            """.trimIndent() + "\n",
            query = "ancestor(alice, Result)",
        ),
        PrologExample(
            title = "Expert diagnosis",
            summary = "An explainable expert system built from symptoms and rules.",
            fileName = "expert_system.pl",
            source = """
                symptom(alex, fever).
                symptom(alex, cough).
                symptom(alex, fatigue).

                diagnosis(Person, flu) :-
                    symptom(Person, fever),
                    symptom(Person, cough),
                    symptom(Person, fatigue).

                explanation(Person, Result) :-
                    diagnosis(Person, Condition),
                    Result = diagnosis(Person, Condition).
            """.trimIndent() + "\n",
            query = "explanation(alex, Result)",
        ),
        PrologExample(
            title = "Configuration as logic",
            summary = "Derive effective settings from facts instead of branching UI code.",
            fileName = "config.pl",
            source = """
                config(theme, outrun).
                config(runtime, local).
                config(privacy, device_only).

                effective(Key, Result) :- config(Key, Result).
                local_private(Result) :-
                    config(runtime, local),
                    config(privacy, device_only),
                    Result = enabled.
            """.trimIndent() + "\n",
            query = "effective(theme, Result)",
        ),
    )
}

data class PrologTutorialStep(
    val title: String,
    val lesson: String,
    val exampleFile: String,
    val query: String,
)

object PrologTutorialCatalog {
    val steps = listOf(
        PrologTutorialStep(
            "Facts are data",
            "A fact states a relationship. parent(alice, bob). means the relationship is true.",
            "family.pl",
            "parent(alice, Result)",
        ),
        PrologTutorialStep(
            "Variables ask questions",
            "Capitalized names are variables. Result is filled once for every matching solution.",
            "family.pl",
            "ancestor(alice, Result)",
        ),
        PrologTutorialStep(
            "Rules derive knowledge",
            "Head :- Body means the head is true when every goal in the body succeeds.",
            "family.pl",
            "ancestor(bob, Result)",
        ),
        PrologTutorialStep(
            "Recursion walks graphs",
            "The second ancestor rule follows one parent edge and asks ancestor/2 again.",
            "family.pl",
            "ancestor(alice, Result)",
        ),
        PrologTutorialStep(
            "Expert systems explain",
            "Keep evidence as facts and decisions as rules. Query the explanation predicate to return proof-shaped data.",
            "expert_system.pl",
            "explanation(alex, Result)",
        ),
        PrologTutorialStep(
            "Configuration can be executable",
            "config/2 stores user choices; effective/2 derives the setting consumed by Zara.",
            "config.pl",
            "effective(runtime, Result)",
        ),
    )
}
