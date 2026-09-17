package ai.zara.app.ui

import ai.zara.app.prolog.LogicGraph
import ai.zara.app.prolog.LogicNodeKind
import ai.zara.app.prolog.PrologDocument
import ai.zara.app.prolog.PrologCompletionEngine
import ai.zara.app.prolog.PrologExampleCatalog
import ai.zara.app.prolog.PrologSource
import ai.zara.app.prolog.PrologSourceAnalyzer
import ai.zara.app.prolog.PrologSchemaValidator
import ai.zara.app.prolog.PrologTutorialCatalog
import ai.zara.app.runtime.LocalQueryResult
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalServerState
import ai.zara.ui.theme.ZaraSemanticTokens
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.OutlinedTextFieldDefaults
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.OffsetMapping
import androidx.compose.ui.text.input.TransformedText
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.sin

private enum class StudioPane(val label: String) {
    Editor("IDE"),
    Expert("Expert"),
    Syntax("Syntax"),
    Graph("Graph"),
    Learn("Learn"),
}

@Composable
internal fun PrologStudioSurface(
    localState: LocalServerState,
    sources: List<PrologSource>,
    queryResult: LocalQueryResult?,
    operationError: String?,
    operationBusy: Boolean,
    onSaveSource: (String, String) -> Unit,
    onReload: () -> Unit,
    onRunQuery: (String) -> Unit,
    padding: PaddingValues,
) {
    val first = sources.firstOrNull()
    var paneName by rememberSaveable { mutableStateOf(StudioPane.Editor.name) }
    var selectedName by rememberSaveable { mutableStateOf(first?.name.orEmpty()) }
    var draft by rememberSaveable { mutableStateOf(first?.text.orEmpty()) }
    var query by rememberSaveable { mutableStateOf(firstExampleQuery(selectedName)) }
    val selected = sources.firstOrNull { it.name == selectedName } ?: first
    val document = remember(selectedName, draft) {
        val analyzed = PrologSourceAnalyzer.analyze(selectedName.ifBlank { "scratch.pl" }, draft)
        analyzed.copy(diagnostics = analyzed.diagnostics + PrologSchemaValidator.validate(analyzed))
    }
    val pane = StudioPane.entries.firstOrNull { it.name == paneName } ?: StudioPane.Editor

    ScreenBody(padding) {
        ScreenTitle("Logic", "Local Prolog studio and symbolic runtime")
        SectionCard("LOCAL SERVER") {
            KeyValueRow("state", localState.phase.name.lowercase())
            KeyValueRow("generation", localState.generation.toString())
            KeyValueRow("sources", localState.loadedSources.size.toString())
            localState.failure?.let { ErrorBanner(it) }
            if (localState.phase == LocalServerPhase.READY) {
                MutedNotice("Trealla owns one serialized runtime actor. Workspace reloads replace the runtime so old clauses cannot accumulate.")
            }
        }

        Row(
            modifier = Modifier.fillMaxWidth().horizontalScroll(rememberScrollState()),
            horizontalArrangement = Arrangement.spacedBy(6.dp),
        ) {
            StudioPane.entries.forEach { destination ->
                StudioTab(destination.label, destination == pane) { paneName = destination.name }
            }
        }

        when (pane) {
            StudioPane.Editor -> EditorPane(
                sources = sources,
                selected = selected,
                selectedName = selectedName,
                draft = draft,
                document = document,
                query = query,
                queryResult = queryResult,
                operationBusy = operationBusy,
                onSelect = { source ->
                    selectedName = source.name
                    draft = source.text
                    query = firstExampleQuery(source.name)
                },
                onCreateSource = { name ->
                    val normalized = if (name.endsWith(".pl")) name else "$name.pl"
                    onSaveSource(normalized, "% $normalized\n")
                },
                onDraft = { draft = it },
                onQuery = { query = it },
                onSave = { onSaveSource(selectedName.ifBlank { "scratch.pl" }, draft) },
                onReload = onReload,
                onRunQuery = { onRunQuery(query) },
            )
            StudioPane.Expert -> ExpertEditorPane(
                sources = sources,
                operationBusy = operationBusy,
                onSaveSource = onSaveSource,
            )
            StudioPane.Syntax -> SyntaxEditorPane(
                sources = sources,
                operationBusy = operationBusy,
                onSaveSource = onSaveSource,
            )
            StudioPane.Graph -> GraphPane(document)
            StudioPane.Learn -> TutorialPane(
                onOpen = { fileName, exampleQuery ->
                    sources.firstOrNull { it.name == fileName }?.let { source ->
                        selectedName = source.name
                        draft = source.text
                        query = exampleQuery
                        paneName = StudioPane.Editor.name
                    }
                },
            )
        }
        operationError?.let { ErrorBanner(it) }
    }
}

@Composable
private fun StudioTab(label: String, selected: Boolean, onClick: () -> Unit) {
    val tokens = LocalZaraTokens.current
    Surface(
        onClick = onClick,
        color = if (selected) tokens.ambientGlow else tokens.surface,
        border = BorderStroke(1.dp, if (selected) tokens.borderActive else tokens.border),
        shape = MaterialTheme.shapes.large,
    ) {
        Text(
            label,
            modifier = Modifier.padding(horizontal = 15.dp, vertical = 9.dp),
            color = if (selected) tokens.text else tokens.textMuted,
            fontFamily = FontFamily.Monospace,
        )
    }
}

@Composable
private fun EditorPane(
    sources: List<PrologSource>,
    selected: PrologSource?,
    selectedName: String,
    draft: String,
    document: PrologDocument,
    query: String,
    queryResult: LocalQueryResult?,
    operationBusy: Boolean,
    onSelect: (PrologSource) -> Unit,
    onCreateSource: (String) -> Unit,
    onDraft: (String) -> Unit,
    onQuery: (String) -> Unit,
    onSave: () -> Unit,
    onReload: () -> Unit,
    onRunQuery: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val completions = remember(draft, document) {
        PrologCompletionEngine.complete(draft, draft.length, listOf(document))
    }
    var creatingSource by rememberSaveable { mutableStateOf(false) }
    var newSourceName by rememberSaveable { mutableStateOf("") }
    SectionCard("SOURCES") {
        Row(
            modifier = Modifier.fillMaxWidth().horizontalScroll(rememberScrollState()),
            horizontalArrangement = Arrangement.spacedBy(4.dp),
        ) {
            sources.forEach { source ->
                TextButton(onClick = { onSelect(source) }) {
                    Text(
                        source.name,
                        color = if (source.name == selectedName) tokens.accentCyan else tokens.textMuted,
                        fontFamily = FontFamily.Monospace,
                    )
                }
            }
            TextButton(onClick = { creatingSource = !creatingSource }) {
                Text("＋ source", color = tokens.accentMagenta)
            }
        }
        if (creatingSource) {
            OutlinedTextField(
                value = newSourceName,
                onValueChange = { newSourceName = it },
                modifier = Modifier.fillMaxWidth(),
                label = { Text("new_source.pl") },
                singleLine = true,
                colors = studioFieldColors(),
            )
            PrimaryAction(
                "Create source",
                !operationBusy && newSourceName.isNotBlank(),
            ) {
                onCreateSource(newSourceName.trim())
                newSourceName = ""
                creatingSource = false
            }
        }
    }
    SectionCard("EDITOR · ${selected?.name ?: "scratch.pl"}") {
        OutlinedTextField(
            value = draft,
            onValueChange = onDraft,
            modifier = Modifier.fillMaxWidth().heightIn(min = 300.dp),
            textStyle = MaterialTheme.typography.bodySmall.copy(
                color = tokens.text,
                fontFamily = FontFamily.Monospace,
            ),
            label = { Text("Prolog source") },
            visualTransformation = remember(tokens) { PrologVisualTransformation(tokens) },
            colors = studioFieldColors(),
        )
        if (completions.isNotEmpty()) {
            Text("AUTOCOMPLETE", color = tokens.accentCyan, style = MaterialTheme.typography.labelSmall)
            Row(
                modifier = Modifier.fillMaxWidth().horizontalScroll(rememberScrollState()),
                horizontalArrangement = Arrangement.spacedBy(4.dp),
            ) {
                completions.take(6).forEach { completion ->
                    TextButton(onClick = {
                        val prefix = draft.takeLastWhile { it.isLetterOrDigit() || it == '_' }
                        val predicate = completion.label.substringBefore('/')
                        val arity = completion.label.substringAfter('/', "0").toIntOrNull() ?: 0
                        val arguments = (1..arity).joinToString(", ") { "Arg$it" }
                        onDraft(draft.dropLast(prefix.length) + predicate + if (arity == 0) "" else "($arguments)")
                    }) {
                        Text(completion.label, fontFamily = FontFamily.Monospace)
                    }
                }
            }
        }
        if (document.diagnostics.isEmpty()) {
            KeyValueRow("analysis", "${document.clauses.size} clauses · clean")
        } else {
            document.diagnostics.forEach { diagnostic ->
                ErrorBanner("line ${diagnostic.line}: ${diagnostic.message}")
            }
        }
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            PrimaryAction("Save & reload", !operationBusy && document.diagnostics.isEmpty(), onSave)
            SecondaryAction("Reload runtime", !operationBusy, onReload)
        }
    }
    SectionCard("QUERY") {
        OutlinedTextField(
            value = query,
            onValueChange = onQuery,
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Goal binding Result") },
            textStyle = MaterialTheme.typography.bodyMedium.copy(fontFamily = FontFamily.Monospace),
            colors = studioFieldColors(),
        )
        MutedNotice("Queries are bounded and must bind Result. File, process, meta-call, database mutation, and arbitrary consult predicates are blocked.")
        PrimaryAction("Run query", !operationBusy && query.isNotBlank(), onRunQuery)
        queryResult?.let { result ->
            SelectionContainer {
                Text(
                    if (result.terms.isEmpty()) "false." else result.terms.joinToString("\n"),
                    color = tokens.text,
                    fontFamily = FontFamily.Monospace,
                )
            }
            KeyValueRow("runtime generation", result.generation.toString())
        }
    }
}

@Composable
private fun ExpertEditorPane(
    sources: List<PrologSource>,
    operationBusy: Boolean,
    onSaveSource: (String, String) -> Unit,
) {
    var expertName by rememberSaveable { mutableStateOf("triage") }
    var evidence by rememberSaveable { mutableStateOf("signal, source, confidence") }
    var conclusion by rememberSaveable { mutableStateOf("review") }
    val tokens = LocalZaraTokens.current
    val generated = remember(expertName, evidence, conclusion) {
        expertSystemSource(expertName, evidence, conclusion)
    }
    SectionCard("EXPERT SYSTEM BUILDER") {
        MutedNotice("Build a typed, explainable system from schema declarations, evidence facts, a decision rule, and an explanation term. Generated predicates remain ordinary editable Prolog.")
        OutlinedTextField(
            value = expertName,
            onValueChange = { expertName = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Expert name") },
            singleLine = true,
            colors = studioFieldColors(),
        )
        OutlinedTextField(
            value = evidence,
            onValueChange = { evidence = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Evidence keys, comma separated") },
            singleLine = true,
            colors = studioFieldColors(),
        )
        OutlinedTextField(
            value = conclusion,
            onValueChange = { conclusion = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Conclusion atom") },
            singleLine = true,
            colors = studioFieldColors(),
        )
    }
    SectionCard("GENERATED SOURCE") {
        Text(generated, color = tokens.text, fontFamily = FontFamily.Monospace)
        PrimaryAction(
            "Save expert_system.pl",
            !operationBusy && generated.isNotBlank(),
        ) {
            val existing = sources.firstOrNull { it.name == "expert_system.pl" }?.text.orEmpty()
            val merged = if (existing.contains("% generated:$expertName")) existing else {
                existing.trimEnd() + "\n\n" + generated
            }
            onSaveSource("expert_system.pl", merged.trimStart())
        }
    }
}

@Composable
private fun SyntaxEditorPane(
    sources: List<PrologSource>,
    operationBusy: Boolean,
    onSaveSource: (String, String) -> Unit,
) {
    var operator by rememberSaveable { mutableStateOf("because") }
    var precedence by rememberSaveable { mutableStateOf("600") }
    var associativity by rememberSaveable { mutableStateOf("xfx") }
    val safeOperator = operator.trim().lowercase().replace(Regex("[^a-z0-9_]+"), "_").trim('_')
    val safePrecedence = precedence.toIntOrNull()?.coerceIn(1, 1200)
    val safeAssociativity = associativity.takeIf { it in setOf("xfx", "xfy", "yfx", "fx", "fy", "xf", "yf") }
    val generated = if (safeOperator.isNotBlank() && safePrecedence != null && safeAssociativity != null) {
        ":- op($safePrecedence, $safeAssociativity, $safeOperator).\n"
    } else ""
    SectionCard("LANGUAGE SYNTAX") {
        MutedNotice("Define a standard Prolog operator. It is stored in syntax.pl and loaded by the same bounded Trealla runtime as the rest of the workspace.")
        OutlinedTextField(operator, { operator = it }, Modifier.fillMaxWidth(), label = { Text("Operator atom") }, singleLine = true, colors = studioFieldColors())
        OutlinedTextField(precedence, { precedence = it }, Modifier.fillMaxWidth(), label = { Text("Precedence 1–1200") }, singleLine = true, colors = studioFieldColors())
        OutlinedTextField(associativity, { associativity = it }, Modifier.fillMaxWidth(), label = { Text("xfx · xfy · yfx · fx · fy · xf · yf") }, singleLine = true, colors = studioFieldColors())
    }
    SectionCard("GENERATED DIRECTIVE") {
        Text(generated.ifBlank { "Invalid operator definition" }, fontFamily = FontFamily.Monospace)
        PrimaryAction("Add to syntax.pl", !operationBusy && generated.isNotBlank()) {
            val existing = sources.firstOrNull { it.name == "syntax.pl" }?.text.orEmpty()
            if (!existing.contains(generated.trim())) {
                onSaveSource("syntax.pl", (existing.trimEnd() + "\n" + generated).trimStart())
            }
        }
    }
}

@Composable
private fun GraphPane(document: PrologDocument) {
    val tokens = LocalZaraTokens.current
    SectionCard("FACT / RULE GRAPH") {
        if (document.graph.nodes.isEmpty()) {
            MutedNotice("Add facts or rules in the IDE to build the graph.")
            return@SectionCard
        }
        LogicGraphCanvas(document.graph)
        document.graph.nodes.filter { it.kind == LogicNodeKind.PREDICATE }.forEach { node ->
            KeyValueRow(node.label, "${node.source}:${node.line}")
        }
        Text("EDGES", color = tokens.accentCyan, style = MaterialTheme.typography.labelSmall)
        document.graph.edges.filter { it.label == "calls" }.forEach { edge ->
            Text(
                "${edge.from.removePrefix("predicate:")} → ${edge.to.removePrefix("predicate:")}",
                color = tokens.textMuted,
                fontFamily = FontFamily.Monospace,
            )
        }
    }
}

@Composable
private fun LogicGraphCanvas(graph: LogicGraph) {
    val tokens = LocalZaraTokens.current
    val nodes = graph.nodes.filter { it.kind == LogicNodeKind.PREDICATE }.take(14)
    Box(modifier = Modifier.fillMaxWidth().height(280.dp)) {
        Canvas(modifier = Modifier.fillMaxWidth().height(280.dp)) {
            if (nodes.isEmpty()) return@Canvas
            val center = Offset(size.width / 2f, size.height / 2f)
            val radius = minOf(size.width, size.height) * 0.34f
            val positions = nodes.mapIndexed { index, _ ->
                val angle = (2.0 * PI * index / nodes.size) - PI / 2.0
                Offset(
                    center.x + (cos(angle) * radius).toFloat(),
                    center.y + (sin(angle) * radius).toFloat(),
                )
            }
            val positionsById = nodes.mapIndexed { index, node -> node.id to positions[index] }.toMap()
            graph.edges.filter { it.label == "calls" }.forEach { edge ->
                val from = positionsById[edge.from] ?: return@forEach
                val to = positionsById[edge.to] ?: return@forEach
                drawLine(tokens.borderActive, from, to, strokeWidth = 2.dp.toPx())
            }
            positions.forEachIndexed { index, position ->
                drawCircle(
                    color = if (index == 0) tokens.accentMagenta else tokens.accentCyan,
                    radius = 12.dp.toPx(),
                    center = position,
                )
                drawCircle(tokens.background, 6.dp.toPx(), position)
            }
        }
    }
}

@Composable
private fun TutorialPane(onOpen: (String, String) -> Unit) {
    val tokens = LocalZaraTokens.current
    SectionCard("PROLOG FROM ZERO TO ZARA") {
        PrologTutorialCatalog.steps.forEachIndexed { index, step ->
            Surface(
                modifier = Modifier.fillMaxWidth(),
                color = tokens.surfaceInput,
                border = BorderStroke(1.dp, tokens.border),
                shape = MaterialTheme.shapes.medium,
            ) {
                Column(Modifier.padding(14.dp)) {
                    Text(
                        "${index + 1}. ${step.title}",
                        color = tokens.accentCyan,
                        fontFamily = FontFamily.Monospace,
                    )
                    Text(
                        step.lesson,
                        modifier = Modifier.padding(top = 6.dp),
                        color = tokens.text,
                    )
                    Text(
                        "?- ${step.query}.",
                        modifier = Modifier.padding(top = 8.dp),
                        color = tokens.textMuted,
                        fontFamily = FontFamily.Monospace,
                    )
                    TextButton(onClick = { onOpen(step.exampleFile, step.query) }) {
                        Text("Open lesson in IDE")
                    }
                }
            }
            Spacer(Modifier.size(8.dp))
        }
    }
}

private fun firstExampleQuery(fileName: String): String =
    PrologExampleCatalog.examples.firstOrNull { it.fileName == fileName }?.query
        ?: "member(Result, [hello, prolog])"

private fun expertSystemSource(name: String, evidence: String, conclusion: String): String {
    val safeName = name.trim().lowercase().replace(Regex("[^a-z0-9_]+"), "_")
        .trim('_').take(32)
    val safeConclusion = conclusion.trim().lowercase().replace(Regex("[^a-z0-9_]+"), "_")
        .trim('_').take(32)
    val keys = evidence.split(',').map {
        it.trim().lowercase().replace(Regex("[^a-z0-9_]+"), "_").trim('_').take(32)
    }.filter { it.isNotBlank() }.distinct().take(8)
    if (safeName.isBlank() || safeConclusion.isBlank() || keys.isEmpty()) return ""
    val goals = keys.joinToString(",\n    ") { "evidence(Entity, $it)" }
    return """
        % generated:$safeName
        :- zara_schema(evidence, 2, [atom, atom]).
        :- zara_schema(${safeName}_decision, 2, [atom, atom]).
        :- zara_schema(${safeName}_explain, 2, [atom, term]).

        ${safeName}_decision(Entity, $safeConclusion) :-
            $goals.

        ${safeName}_explain(Entity, Result) :-
            ${safeName}_decision(Entity, Decision),
            Result = decision(Entity, Decision).
    """.trimIndent() + "\n"
}

private class PrologVisualTransformation(
    private val tokens: ZaraSemanticTokens,
) : VisualTransformation {
    private data class Rule(val regex: Regex, val style: SpanStyle)

    private val rules = listOf(
        Rule(Regex("\\b[A-Z_][A-Za-z0-9_]*\\b"), SpanStyle(color = tokens.secondary)),
        Rule(Regex("\\b\\d+(?:\\.\\d+)?\\b"), SpanStyle(color = tokens.primary)),
        Rule(
            Regex("\\b[a-z][A-Za-z0-9_]*(?=\\s*\\()"),
            SpanStyle(color = tokens.accentCyan, fontWeight = FontWeight.SemiBold),
        ),
        Rule(Regex("'(?:\\\\.|[^'\\\\])*'"), SpanStyle(color = tokens.warning)),
        Rule(Regex("\"(?:\\\\.|[^\"\\\\])*\""), SpanStyle(color = tokens.warning)),
        Rule(Regex("%[^\\n]*"), SpanStyle(color = tokens.textMuted)),
    )

    override fun filter(text: AnnotatedString): TransformedText {
        val highlighted = AnnotatedString.Builder(text)
        rules.forEach { rule ->
            rule.regex.findAll(text.text).forEach { match ->
                highlighted.addStyle(rule.style, match.range.first, match.range.last + 1)
            }
        }
        return TransformedText(highlighted.toAnnotatedString(), OffsetMapping.Identity)
    }
}

@Composable
private fun studioFieldColors() = run {
    val tokens = LocalZaraTokens.current
    OutlinedTextFieldDefaults.colors(
        focusedTextColor = tokens.text,
        unfocusedTextColor = tokens.text,
        focusedBorderColor = tokens.borderActive,
        unfocusedBorderColor = tokens.border,
        focusedLabelColor = tokens.accentCyan,
        unfocusedLabelColor = tokens.textMuted,
        cursorColor = tokens.focus,
        focusedContainerColor = Color.Transparent,
        unfocusedContainerColor = Color.Transparent,
    )
}
