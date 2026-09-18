package ai.zara.app.ui

import ai.zara.app.prolog.ExplorerGraph
import ai.zara.app.prolog.ExplorerLocation
import ai.zara.app.prolog.ExplorerNode
import ai.zara.app.prolog.ExplorerNodeKind
import ai.zara.app.prolog.PrologDocument
import ai.zara.app.prolog.PrologGraphExplorerModel
import ai.zara.app.prolog.PrologSource
import ai.zara.ui.theme.ZaraSemanticTokens
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.gestures.detectTransformGestures
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.geometry.CornerRadius
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Path
import androidx.compose.ui.graphics.PathEffect
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.drawscope.withTransform
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.drawText
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.rememberTextMeasurer
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Constraints
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlin.math.atan2
import kotlin.math.cos
import kotlin.math.roundToInt
import kotlin.math.sin

@OptIn(ExperimentalLayoutApi::class)
@Composable
internal fun PrologGraphExplorer(
    documents: List<PrologDocument>,
    savedSources: List<PrologSource>,
    startWithFiles: Boolean = false,
    canNavigate: (String) -> Boolean,
    onNavigate: (String, Int) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    var mode by rememberSaveable(startWithFiles) { mutableStateOf(if (startWithFiles) "Files" else "Predicates") }
    var sourceFilter by rememberSaveable { mutableStateOf("") }
    var sourceMenu by remember { mutableStateOf(false) }
    var search by rememberSaveable { mutableStateOf("") }
    var focus by rememberSaveable { mutableStateOf<String?>(null) }
    var selectedId by rememberSaveable { mutableStateOf<String?>(null) }
    var viewedSource by rememberSaveable { mutableStateOf<String?>(null) }
    var viewedLine by rememberSaveable { mutableStateOf(1) }
    val graph by produceState<ExplorerGraph?>(null, documents, mode, sourceFilter) {
        value = null
        value = withContext(Dispatchers.Default) {
            PrologGraphExplorerModel.build(documents, mode == "Clauses", sourceFilter.ifBlank { null })
        }
    }
    val visible = remember(graph, search, focus) {
        graph?.let { PrologGraphExplorerModel.filter(it, search, focus) }
    }
    val selected = graph?.nodes?.firstOrNull { it.id == selectedId }
    val viewed = documents.firstOrNull { it.source == viewedSource }

    fun openSource(location: ExplorerLocation) {
        viewedSource = location.source
        viewedLine = location.line
    }

    SectionCard("LOGIC EXPLORER") {
        FlowRow(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
            listOf("Predicates", "Clauses", "Files").forEach { label ->
                FilterChip(selected = mode == label, onClick = {
                    mode = label
                    focus = null
                    selectedId = null
                }, label = { Text(label) })
            }
        }
        FlowRow(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Box {
                TextButton(onClick = { sourceMenu = true }) { Text(sourceFilter.ifBlank { "All files (${documents.size})" }) }
                DropdownMenu(expanded = sourceMenu, onDismissRequest = { sourceMenu = false }, modifier = Modifier.heightIn(max = 320.dp)) {
                    DropdownMenuItem(text = { Text("All files") }, onClick = { sourceFilter = ""; sourceMenu = false; focus = null })
                    documents.sortedBy { it.source }.forEach { document ->
                        DropdownMenuItem(text = { Text(document.source) }, onClick = {
                            sourceFilter = document.source; sourceMenu = false; focus = null
                        })
                    }
                }
            }
            TextButton(onClick = { sourceFilter = ""; search = ""; focus = null; selectedId = null }) { Text("Show all") }
        }
        OutlinedTextField(
            value = search, onValueChange = { if (it.length <= 256) search = it },
            modifier = Modifier.fillMaxWidth(), singleLine = true,
            label = { Text(if (mode == "Files") "Find file or source text" else "Find predicate, clause or file") },
            colors = studioFieldColors(),
        )
        MutedNotice("Static workspace source map—not a proof or live execution trace. References may include built-ins or data terms; meta-calls and custom syntax may be incomplete.")
        if (mode == "Files") {
            val files = documents.filter {
                (sourceFilter.isBlank() || it.source == sourceFilter) &&
                    (search.isBlank() || it.source.contains(search, true) || it.text.contains(search, true))
            }.sortedBy { it.source }
            Text("${files.size} / ${documents.size} files", color = tokens.textMuted)
            LazyColumn(Modifier.fillMaxWidth().height(320.dp), verticalArrangement = Arrangement.spacedBy(6.dp)) {
                items(files, key = { it.source }) { document ->
                    Surface(onClick = { viewedSource = document.source; viewedLine = 1 }, color = tokens.surfaceInput,
                        border = BorderStroke(1.dp, tokens.border), modifier = Modifier.fillMaxWidth()) {
                        Column(Modifier.padding(12.dp)) {
                            Text(document.source, color = tokens.accentCyan, fontFamily = FontFamily.Monospace)
                            Text("${document.clauses.size} clauses · ${document.diagnostics.size} diagnostics", color = tokens.textMuted)
                            if (savedSources.none { it.name == document.source && it.text == document.text }) {
                                Text("UNSAVED DRAFT", color = tokens.warning)
                            }
                        }
                    }
                }
                if (files.isEmpty()) item { MutedNotice("No matching files. Show all clears the filters.") }
            }
        } else if (visible == null) {
            MutedNotice("Indexing workspace…")
        } else {
            val current = visible
            Text("${current.nodes.size} / ${graph?.nodes?.size ?: 0} nodes · ${current.edges.size} relationships", color = tokens.textMuted)
            if (search.isNotBlank() || focus != null) MutedNotice("Filtered view includes immediate incoming and outgoing neighbors.")
            if (current.nodes.isEmpty()) {
                MutedNotice("No matching nodes. Show all resets the scope; Files can inspect empty files and unsupported syntax.")
            } else {
                ExplorerCanvas(current, selectedId ?: focus) { selectedId = it }
                Text("FACT · RULE · FACT + RULE · REFERENCE", color = tokens.textMuted, style = MaterialTheme.typography.labelSmall)
                MutedNotice("Arrows point toward references. Dashed arrows define clauses. Loop arrows show recursive references. Tap a node or use the complete accessible index below.")
                LazyColumn(Modifier.fillMaxWidth().height(240.dp), verticalArrangement = Arrangement.spacedBy(6.dp)) {
                    items(current.nodes, key = { it.id }) { node ->
                        Surface(onClick = { selectedId = node.id }, color = tokens.surfaceInput,
                            border = BorderStroke(1.dp, nodeColor(node.kind, tokens)), modifier = Modifier.fillMaxWidth()) {
                            Column(Modifier.padding(12.dp)) {
                                Text(node.kind.label, color = nodeColor(node.kind, tokens), style = MaterialTheme.typography.labelSmall)
                                Text(node.label, color = tokens.text, fontFamily = FontFamily.Monospace, maxLines = 3, overflow = TextOverflow.Ellipsis)
                                Text(node.provenance, color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
                            }
                        }
                    }
                }
            }
        }
    }
    if (viewed != null) {
        PrologSourceDialog(viewed, viewedLine,
            isDraft = savedSources.none { it.name == viewed.source && it.text == viewed.text },
            canNavigate = canNavigate(viewed.source),
            onDismiss = { viewedSource = null },
            onNavigate = { onNavigate(viewed.source, viewedLine); viewedSource = null; selectedId = null })
    } else if (viewedSource != null) {
        AlertDialog(onDismissRequest = { viewedSource = null }, title = { Text("Source no longer available") },
            text = { Text("The workspace changed. Reopen the file from Files.") },
            confirmButton = { TextButton(onClick = { viewedSource = null }) { Text("Close") } })
    } else if (selected != null && graph != null) {
        ExplorerInspector(selected, graph!!, onDismiss = { selectedId = null },
            onFocus = { focus = selected.id; search = ""; selectedId = null },
            onSelect = { selectedId = it }, onOpenSource = ::openSource)
    }
}

@Composable
private fun ExplorerInspector(
    node: ExplorerNode, graph: ExplorerGraph, onDismiss: () -> Unit, onFocus: () -> Unit,
    onSelect: (String) -> Unit, onOpenSource: (ExplorerLocation) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val byId = remember(graph) { graph.nodes.associateBy { it.id } }
    val edges = remember(graph, node.id) { graph.edges.filter { it.from == node.id || it.to == node.id } }
    AlertDialog(onDismissRequest = onDismiss,
        title = { Text(node.kind.label, color = nodeColor(node.kind, tokens)) },
        text = {
            LazyColumn(Modifier.heightIn(max = 420.dp), verticalArrangement = Arrangement.spacedBy(8.dp)) {
                item { SelectionContainer { Text(node.label, fontFamily = FontFamily.Monospace) } }
                item { Text(node.provenance) }
                if (node.locations.isEmpty()) item {
                    Text("No definition in these workspace files. This may be a built-in, external predicate or data term—not evidence of failure.")
                }
                itemsIndexed(node.locations) { index, location ->
                    TextButton(onClick = { onOpenSource(location) }) {
                        Column {
                            Text("${index + 1}. ${location.source}:${location.line} · View Prolog")
                            Text(location.text, fontFamily = FontFamily.Monospace, maxLines = 4, overflow = TextOverflow.Ellipsis)
                        }
                    }
                }
                item { Text("RELATIONSHIPS (${edges.size})", color = tokens.accentCyan) }
                items(edges) { edge ->
                    val target = if (edge.from == node.id) edge.to else edge.from
                    TextButton(onClick = { onSelect(target) }) {
                        Text("${if (edge.from == node.id) "→" else "←"} ${edge.label}: ${byId[target]?.label ?: target}",
                            fontFamily = FontFamily.Monospace)
                    }
                }
            }
        },
        confirmButton = { TextButton(onClick = onFocus) { Text("Focus neighbors") } },
        dismissButton = { TextButton(onClick = onDismiss) { Text("Back to graph") } },
    )
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun PrologSourceDialog(
    document: PrologDocument, line: Int, isDraft: Boolean, canNavigate: Boolean,
    onDismiss: () -> Unit, onNavigate: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val clipboard = LocalClipboardManager.current
    val ranges = remember(document.text) { PrologGraphExplorerModel.lineRanges(document.text) }
    val highlighted = remember(document.text, tokens) {
        PrologVisualTransformation(tokens).filter(AnnotatedString(document.text)).text
    }
    val listState = rememberLazyListState()
    val targetLine = line.coerceIn(1, ranges.size)
    LaunchedEffect(document.source, document.text, targetLine) {
        listState.scrollToItem((targetLine - 3).coerceIn(0, ranges.lastIndex))
    }
    Dialog(onDismissRequest = onDismiss, properties = DialogProperties(usePlatformDefaultWidth = false)) {
        Surface(Modifier.fillMaxSize().systemBarsPadding(), color = tokens.background) {
            Column(Modifier.fillMaxSize().padding(16.dp), verticalArrangement = Arrangement.spacedBy(8.dp)) {
                Text(document.source, color = tokens.accentCyan, fontFamily = FontFamily.Monospace)
                Text(if (isDraft) "UNSAVED DRAFT · not loaded runtime state" else "WORKSPACE SOURCE · displayed revision is not verified as loaded",
                    color = if (isDraft) tokens.warning else tokens.textMuted, style = MaterialTheme.typography.bodySmall)
                FlowRow(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    TextButton(onClick = { clipboard.setText(AnnotatedString(document.text)) }) { Text("Copy source") }
                    TextButton(onClick = onNavigate, enabled = canNavigate) { Text("Open in IDE") }
                    TextButton(onClick = onDismiss) { Text("Close") }
                }
                if (!canNavigate) Text("Save the current editor draft before switching files in the IDE. Viewing here does not change it.", color = tokens.warning)
                Text("${ranges.size} lines · source anchor $targetLine · ${document.diagnostics.size} diagnostics", color = tokens.textMuted)
                SelectionContainer(Modifier.weight(1f)) {
                    LazyColumn(Modifier.fillMaxSize(), state = listState) {
                        itemsIndexed(ranges) { index, range ->
                            Row(Modifier.fillMaxWidth().background(if (index + 1 == targetLine) tokens.ambientGlow else Color.Transparent).padding(vertical = 3.dp)) {
                                Text("${index + 1}", modifier = Modifier.width(48.dp), color = tokens.textMuted, fontFamily = FontFamily.Monospace, fontSize = 13.sp)
                                Text(highlighted.subSequence(range.first, range.last + 1), modifier = Modifier.weight(1f),
                                    color = tokens.text, fontFamily = FontFamily.Monospace, fontSize = 13.sp, softWrap = true)
                            }
                        }
                    }
                }
            }
        }
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun ExplorerCanvas(graph: ExplorerGraph, selectedId: String?, onSelect: (String) -> Unit) {
    val tokens = LocalZaraTokens.current
    val density = LocalDensity.current
    val unit = density.density * maxOf(1f, density.fontScale)
    val positions = remember(graph) { PrologGraphExplorerModel.layout(graph) }
    val textMeasurer = rememberTextMeasurer()
    var viewport by remember { mutableStateOf(IntSize.Zero) }
    var zoom by remember { mutableStateOf(1f) }
    var pan by remember { mutableStateOf(Offset.Zero) }
    val worldWidth = (positions.values.maxOfOrNull { it.right } ?: 1f) * unit + 32f * unit
    val worldHeight = (positions.values.maxOfOrNull { it.bottom } ?: 1f) * unit + 32f * unit

    fun fit() {
        if (viewport.width <= 0 || viewport.height <= 0) return
        zoom = minOf(viewport.width / worldWidth, viewport.height / worldHeight, 1f).coerceAtLeast(0.00001f)
        pan = Offset((viewport.width - worldWidth * zoom) / 2f, (viewport.height - worldHeight * zoom) / 2f)
    }
    fun changeZoom(target: Float) {
        val next = target.coerceIn(0.00001f, 4f)
        val center = Offset(viewport.width / 2f, viewport.height / 2f)
        pan = center - (center - pan) * (next / zoom)
        zoom = next
    }
    LaunchedEffect(graph, viewport, unit) { fit() }
    FlowRow(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
        TextButton(onClick = ::fit) { Text("Fit all") }
        TextButton(onClick = { zoom = 1f; pan = Offset.Zero }) { Text("100%") }
        TextButton(onClick = { changeZoom(zoom / 1.4f) }, modifier = Modifier.semantics { contentDescription = "Zoom out" }) { Text("−") }
        TextButton(onClick = { changeZoom(zoom * 1.4f) }, modifier = Modifier.semantics { contentDescription = "Zoom in" }) { Text("＋") }
    }
    Canvas(Modifier.fillMaxWidth().height(320.dp).clipToBounds().background(tokens.surfaceInput)
        .onSizeChanged { viewport = it }
        .semantics { contentDescription = "Interactive logic graph. Pinch to zoom, drag to pan, tap to inspect. All nodes are also in the accessible index below." }
        .pointerInput(positions, unit) {
            detectTapGestures { point ->
                PrologGraphExplorerModel.hitTest(positions, (point.x - pan.x) / (zoom * unit), (point.y - pan.y) / (zoom * unit))?.let(onSelect)
            }
        }
        .pointerInput(positions, unit) {
            detectTransformGestures { centroid, drag, change, _ ->
                val next = (zoom * change).coerceIn(0.00001f, 4f)
                pan = centroid - (centroid - pan) * (next / zoom) + drag
                zoom = next
            }
        }) {
        withTransform({ translate(pan.x, pan.y); scale(zoom, zoom, pivot = Offset.Zero) }) {
            graph.edges.forEach { edge ->
                val from = positions[edge.from] ?: return@forEach
                val to = positions[edge.to] ?: return@forEach
                val forwards = to.left >= from.left
                val start = Offset((if (forwards) from.right else from.left) * unit, (from.top + 48f) * unit)
                val end = Offset((if (forwards) to.left else to.right) * unit, (to.top + 48f) * unit)
                val control1: Offset
                val control2: Offset
                val actualEnd: Offset
                if (edge.from == edge.to) {
                    control1 = start + Offset(48f * unit, -70f * unit)
                    actualEnd = Offset((from.left + 116f) * unit, from.top * unit)
                    control2 = actualEnd + Offset(48f * unit, -30f * unit)
                } else {
                    val direction = if (forwards) 1f else -1f
                    control1 = start + Offset(40f * unit * direction, 0f)
                    control2 = end - Offset(40f * unit * direction, 0f)
                    actualEnd = end
                }
                val color = if (edge.from == selectedId || edge.to == selectedId) tokens.focus else tokens.borderActive
                val path = Path().apply {
                    moveTo(start.x, start.y)
                    cubicTo(control1.x, control1.y, control2.x, control2.y, actualEnd.x, actualEnd.y)
                }
                val dash = if (edge.label == "defines") PathEffect.dashPathEffect(floatArrayOf(6f * unit, 4f * unit)) else null
                drawPath(path, color, style = Stroke(1.5f * unit, pathEffect = dash))
                val angle = atan2(actualEnd.y - control2.y, actualEnd.x - control2.x)
                val arrow = Path().apply {
                    moveTo(actualEnd.x, actualEnd.y)
                    lineTo(actualEnd.x - cos(angle - 0.5f) * 9f * unit, actualEnd.y - sin(angle - 0.5f) * 9f * unit)
                    lineTo(actualEnd.x - cos(angle + 0.5f) * 9f * unit, actualEnd.y - sin(angle + 0.5f) * 9f * unit)
                    close()
                }
                drawPath(arrow, color)
            }
            graph.nodes.forEach { node ->
                val box = positions[node.id] ?: return@forEach
                if (box.right * unit * zoom + pan.x < 0f || box.left * unit * zoom + pan.x > viewport.width ||
                    box.bottom * unit * zoom + pan.y < 0f || box.top * unit * zoom + pan.y > viewport.height) return@forEach
                val position = Offset(box.left * unit, box.top * unit)
                val nodeSize = Size((box.right - box.left) * unit, (box.bottom - box.top) * unit)
                val color = nodeColor(node.kind, tokens)
                val radius = if (node.kind == ExplorerNodeKind.FACT || node.kind == ExplorerNodeKind.FACT_CLAUSE) 16f else 4f
                drawRoundRect(tokens.surface, position, nodeSize, CornerRadius(radius * unit))
                drawRoundRect(if (node.id == selectedId) tokens.focus else color, position, nodeSize, CornerRadius(radius * unit),
                    style = Stroke((if (node.id == selectedId) 3f else 1.5f) * unit,
                        pathEffect = if (node.kind == ExplorerNodeKind.REFERENCE) PathEffect.dashPathEffect(floatArrayOf(5f * unit, 3f * unit)) else null))
                val constraints = Constraints(maxWidth = (nodeSize.width - 24f * unit).roundToInt().coerceAtLeast(1))
                fun label(text: String, y: Float, size: Int, tint: Color, lines: Int = 1) {
                    val measured = textMeasurer.measure(AnnotatedString(text.take(256)),
                        style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = size.sp),
                        maxLines = lines, overflow = TextOverflow.Ellipsis, constraints = constraints)
                    drawText(measured, color = tint, topLeft = position + Offset(12f * unit, y * unit))
                }
                label(node.kind.label, 8f, 10, color)
                label(node.label, 27f, 13, tokens.text, 2)
                label(node.provenance, 73f, 10, tokens.textMuted)
            }
        }
    }
    if (zoom < 0.45f) MutedNotice("Overview scale: zoom in for labels, or select any node in the index.")
}

private fun nodeColor(kind: ExplorerNodeKind, tokens: ZaraSemanticTokens): Color = when (kind) {
    ExplorerNodeKind.FACT, ExplorerNodeKind.FACT_CLAUSE -> tokens.accentCyan
    ExplorerNodeKind.RULE, ExplorerNodeKind.RULE_CLAUSE -> tokens.accentMagenta
    ExplorerNodeKind.MIXED -> tokens.secondary
    ExplorerNodeKind.REFERENCE -> tokens.warning
    ExplorerNodeKind.DIRECTIVE -> tokens.primary
}
