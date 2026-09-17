package ai.zara.app.ui

import ai.zara.app.prolog.LogicGraph
import ai.zara.app.prolog.LogicNodeKind
import ai.zara.app.prolog.PrologDocument
import ai.zara.app.prolog.PrologGraphBrowser
import ai.zara.app.prolog.PrologGraphLayout
import ai.zara.app.prolog.PrologGraphLocation
import ai.zara.app.prolog.PrologSourceLines
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.gestures.detectTransformGestures
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.widthIn
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberUpdatedState
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.geometry.CornerRadius
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.drawscope.withTransform
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.drawText
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.rememberTextMeasurer
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Constraints
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import kotlin.math.abs

@Composable
internal fun PrologGraphExplorer(
    documents: List<PrologDocument>,
    onOpenEditor: (PrologGraphLocation) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val browser = remember(documents) { PrologGraphBrowser.from(documents) }
    var filesOnly by rememberSaveable { mutableStateOf(false) }
    var predicatesOnly by rememberSaveable { mutableStateOf(false) }
    var scopeName by rememberSaveable { mutableStateOf("") }
    var query by rememberSaveable { mutableStateOf("") }
    var viewedSource by rememberSaveable { mutableStateOf("") }
    var viewedLine by rememberSaveable { mutableStateOf(1) }
    var viewedNode by rememberSaveable { mutableStateOf("") }
    val scope = scopeName.takeIf { name -> documents.any { it.source == name } }
    val graph = remember(browser, scope, predicatesOnly, query) {
        browser.visibleGraph(scope, predicatesOnly, query)
    }

    fun openNode(id: String) {
        val location = browser.locations(id).firstOrNull() ?: return
        viewedNode = id
        viewedSource = location.source
        viewedLine = location.line
    }

    SectionCard("FACT / RULE GRAPH") {
        Row(Modifier.fillMaxWidth().horizontalScroll(rememberScrollState())) {
            TextButton(onClick = { filesOnly = false }) {
                Text("Graph", color = if (!filesOnly) tokens.accentCyan else tokens.textMuted)
            }
            TextButton(onClick = { filesOnly = true }) {
                Text("All files (${documents.size})", color = if (filesOnly) tokens.accentCyan else tokens.textMuted)
            }
        }
        MutedNotice("Workspace source graph, including the current editor buffer. Not a live runtime trace. Reading a file does not change your draft.")
        if (filesOnly) {
            if (documents.isEmpty()) MutedNotice("No workspace sources yet.")
            LazyColumn(Modifier.fillMaxWidth().height(320.dp)) {
                items(documents, key = { it.source }) { document ->
                    Column(Modifier.fillMaxWidth().padding(vertical = 4.dp)) {
                        TextButton(onClick = {
                            viewedNode = ""
                            viewedSource = document.source
                            viewedLine = 1
                        }) {
                            Text(document.source, color = tokens.text, fontFamily = FontFamily.Monospace)
                        }
                        Text("${document.clauses.size} clauses · ${document.diagnostics.size} diagnostics",
                            color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
                        TextButton(onClick = {
                            scopeName = document.source
                            query = ""
                            filesOnly = false
                        }) { Text("Graph this file") }
                    }
                }
            }
        } else {
            Row(Modifier.fillMaxWidth().horizontalScroll(rememberScrollState())) {
                TextButton(onClick = { predicatesOnly = false }) {
                    Text("All nodes", color = if (!predicatesOnly) tokens.accentCyan else tokens.textMuted)
                }
                TextButton(onClick = { predicatesOnly = true }) {
                    Text("Predicates only", color = if (predicatesOnly) tokens.accentCyan else tokens.textMuted)
                }
                TextButton(onClick = { scopeName = ""; query = ""; predicatesOnly = false }) { Text("Show all") }
            }
            Text(scope ?: "All workspace files", color = tokens.textMuted, fontFamily = FontFamily.Monospace)
            OutlinedTextField(
                value = query,
                onValueChange = { query = it.take(256) },
                modifier = Modifier.fillMaxWidth(),
                label = { Text("Find predicate, clause or file") },
                singleLine = true,
                colors = studioFieldColors(),
            )
            Text("${graph.nodes.size} / ${browser.graph.nodes.size} nodes · ${graph.edges.size} links",
                color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
            if (graph.nodes.isEmpty()) {
                MutedNotice("No matching nodes. All files still includes empty, comment-only and invalid sources.")
            } else {
                BrowsableLogicCanvas(graph, onSelect = ::openNode)
                Text("ALL VISIBLE NODES · TAP TO VIEW SOURCE", color = tokens.accentCyan,
                    style = MaterialTheme.typography.labelSmall)
                LazyColumn(Modifier.fillMaxWidth().height(220.dp)) {
                    items(graph.nodes, key = { it.id }) { node ->
                        val locations = browser.locations(node.id)
                        Surface(
                            onClick = { openNode(node.id) },
                            modifier = Modifier.fillMaxWidth().heightIn(min = 48.dp),
                            color = tokens.surface,
                        ) {
                            Column(Modifier.padding(8.dp)) {
                                Text(node.label, color = tokens.text, fontFamily = FontFamily.Monospace)
                                val kind = if (locations.firstOrNull()?.definition == true) "definition" else "reference"
                                Text("${node.source}:${node.line} · $kind · ${locations.size} locations",
                                    color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
                            }
                        }
                    }
                }
            }
        }
    }
    if (viewedSource.isNotEmpty()) {
        PrologFileViewer(
            browser = browser,
            sourceName = viewedSource,
            initialLine = viewedLine,
            nodeId = viewedNode,
            onDismiss = { viewedSource = "" },
            onOpenEditor = { location ->
                viewedSource = ""
                onOpenEditor(location)
            },
        )
    }
}

@Composable
private fun BrowsableLogicCanvas(graph: LogicGraph, onSelect: (String) -> Unit) {
    val tokens = LocalZaraTokens.current
    val density = LocalDensity.current
    val layout = remember(graph.nodes, density.fontScale) { PrologGraphLayout(graph.nodes, density.fontScale) }
    val textMeasurer = rememberTextMeasurer(cacheSize = 256)
    val currentOnSelect by rememberUpdatedState(onSelect)
    val textStyle = MaterialTheme.typography.bodySmall.copy(fontFamily = FontFamily.Monospace, color = tokens.text)
    val labelConstraints = remember(density, layout.nodeWidth) {
        Constraints(maxWidth = with(density) { (layout.nodeWidth - 16f).dp.roundToPx() })
    }
    BoxWithConstraints(Modifier.fillMaxWidth()) {
        val viewportWidth = with(density) { maxWidth.toPx() }
        val viewportHeight = with(density) { 260.dp.toPx() }
        val fit = minOf(maxWidth.value / layout.width, 260f / layout.height).coerceAtMost(1f)
        var zoom by remember(layout, maxWidth) { mutableStateOf(fit) }
        var pan by remember(layout, maxWidth) { mutableStateOf(Offset.Zero) }
        val center = Offset(viewportWidth / 2f, viewportHeight / 2f)
        val worldWidth = layout.width * density.density
        val worldHeight = layout.height * density.density
        Column {
            Row(Modifier.fillMaxWidth().horizontalScroll(rememberScrollState())) {
                TextButton(onClick = { zoom = fit; pan = Offset.Zero }) { Text("Fit all") }
                TextButton(onClick = { zoom = (zoom / 1.5f).coerceAtLeast(fit) }) { Text("Zoom out") }
                TextButton(onClick = { zoom = (zoom * 1.5f).coerceAtMost(4f) }) { Text("Zoom in") }
            }
            Canvas(
                Modifier.fillMaxWidth().height(260.dp).clipToBounds()
                    .semantics { contentDescription = "${graph.nodes.size} labeled graph nodes. Pan or zoom; accessible node buttons are below." }
                    .pointerInput(layout, fit, viewportWidth, density) {
                        detectTransformGestures { centroid, movement, scale, _ ->
                            val next = (zoom * scale).coerceIn(fit, 4f)
                            pan = centroid - center - (centroid - center - pan) * (next / zoom) + movement
                            zoom = next
                            pan = Offset(
                                pan.x.coerceIn(-worldWidth * zoom, worldWidth * zoom),
                                pan.y.coerceIn(-worldHeight * zoom, worldHeight * zoom),
                            )
                        }
                    }
                    .pointerInput(layout, fit, viewportWidth, density) {
                        detectTapGestures { tap ->
                            val origin = center + pan - Offset(worldWidth, worldHeight) * (zoom / 2f)
                            val point = (tap - origin) / (zoom * density.density)
                            layout.hitTest(point.x, point.y)?.let(currentOnSelect)
                        }
                    },
            ) {
                val origin = center + pan - Offset(worldWidth, worldHeight) * (zoom / 2f)
                val nodeWidth = layout.nodeWidth * density.density
                val nodeHeight = layout.nodeHeight * density.density
                withTransform({ translate(origin.x, origin.y); scale(zoom, zoom, Offset.Zero) }) {
                    graph.edges.forEach { edge ->
                        val from = layout.positions[edge.from] ?: return@forEach
                        val to = layout.positions[edge.to] ?: return@forEach
                        val start = Offset(from.x * density.density + nodeWidth / 2f, from.y * density.density + nodeHeight / 2f)
                        val end = Offset(to.x * density.density + nodeWidth / 2f, to.y * density.density + nodeHeight / 2f)
                        val color = if (edge.label == "calls") tokens.secondary else tokens.border
                        if (start == end) {
                            drawArc(color, 20f, 310f, false,
                                topLeft = start - Offset(18.dp.toPx(), nodeHeight / 2f + 20.dp.toPx()),
                                size = Size(36.dp.toPx(), 36.dp.toPx()), style = Stroke(1.5.dp.toPx()))
                        } else {
                            val delta = end - start
                            val fraction = minOf(nodeWidth / (2f * abs(delta.x)), nodeHeight / (2f * abs(delta.y)))
                            val tip = end - delta * fraction
                            val direction = delta / delta.getDistance()
                            val perpendicular = Offset(-direction.y, direction.x)
                            drawLine(color, start, tip, 1.5.dp.toPx())
                            drawLine(color, tip, tip - direction * 8.dp.toPx() + perpendicular * 4.dp.toPx(), 1.5.dp.toPx())
                            drawLine(color, tip, tip - direction * 8.dp.toPx() - perpendicular * 4.dp.toPx(), 1.5.dp.toPx())
                        }
                    }
                    graph.nodes.forEach { node ->
                        val point = layout.positions.getValue(node.id)
                        val topLeft = Offset(point.x * density.density, point.y * density.density)
                        val screen = origin + topLeft * zoom
                        if (screen.x > viewportWidth || screen.y > viewportHeight ||
                            screen.x + nodeWidth * zoom < 0 || screen.y + nodeHeight * zoom < 0) return@forEach
                        val outline = if (node.kind == LogicNodeKind.PREDICATE) tokens.accentCyan else tokens.accentMagenta
                        drawRoundRect(tokens.surfaceInput, topLeft, Size(nodeWidth, nodeHeight), CornerRadius(8.dp.toPx()))
                        drawRoundRect(outline, topLeft, Size(nodeWidth, nodeHeight), CornerRadius(8.dp.toPx()), style = Stroke(1.5.dp.toPx()))
                        val label = textMeasurer.measure(
                            text = AnnotatedString(node.label), style = textStyle,
                            maxLines = 2, overflow = TextOverflow.Ellipsis, constraints = labelConstraints,
                        )
                        drawText(label, topLeft = topLeft + Offset(8.dp.toPx(), 8.dp.toPx()))
                    }
                }
            }
            Text("Cyan: predicates · magenta: clauses · arrows: calls / defines",
                color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
        }
    }
}

@Composable
private fun PrologFileViewer(
    browser: PrologGraphBrowser,
    sourceName: String,
    initialLine: Int,
    nodeId: String,
    onDismiss: () -> Unit,
    onOpenEditor: (PrologGraphLocation) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val clipboard = LocalClipboardManager.current
    var selectedSource by rememberSaveable(sourceName, initialLine) { mutableStateOf(sourceName) }
    var selectedLine by rememberSaveable(sourceName, initialLine) { mutableStateOf(initialLine) }
    var lineInput by rememberSaveable(selectedSource) { mutableStateOf(selectedLine.toString()) }
    val document = browser.documents.firstOrNull { it.source == selectedSource }
    val locations = browser.locations(nodeId)
    Dialog(onDismissRequest = onDismiss, properties = DialogProperties(usePlatformDefaultWidth = false)) {
        Surface(
            modifier = Modifier.fillMaxWidth().fillMaxHeight(0.92f).padding(12.dp),
            color = tokens.surface,
            shape = MaterialTheme.shapes.large,
            border = BorderStroke(1.dp, tokens.borderActive),
        ) {
            Column(Modifier.fillMaxSize().padding(12.dp), verticalArrangement = Arrangement.spacedBy(6.dp)) {
                Text("PROLOG FILE VIEWER", color = tokens.accentCyan, style = MaterialTheme.typography.labelSmall)
                Text(selectedSource, color = tokens.text, fontFamily = FontFamily.Monospace)
                Text("Read-only workspace buffer · runtime generation may differ", color = tokens.textMuted,
                    style = MaterialTheme.typography.bodySmall)
                Row(Modifier.fillMaxWidth().horizontalScroll(rememberScrollState())) {
                    TextButton(onClick = onDismiss) { Text("Close") }
                    if (document != null) {
                        TextButton(onClick = { clipboard.setText(AnnotatedString(document.text)) }) { Text("Copy source") }
                        TextButton(onClick = {
                            onOpenEditor(PrologGraphLocation(selectedSource, PrologSourceLines(document.text).clampLine(selectedLine), true))
                        }) { Text("Open in IDE") }
                    }
                }
                if (document == null) {
                    MutedNotice("This source is no longer in the workspace. Close and choose another file.")
                } else {
                    if (nodeId.isNotEmpty()) {
                        Text(if (locations.any { it.definition }) "Definitions" else "Reference only: definition is outside this workspace",
                            color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
                        Row(Modifier.fillMaxWidth().horizontalScroll(rememberScrollState())) {
                            locations.forEach { location ->
                                TextButton(onClick = {
                                    selectedSource = location.source
                                    selectedLine = location.line
                                    lineInput = location.line.toString()
                                }) { Text("${location.source}:${location.line}", fontFamily = FontFamily.Monospace) }
                            }
                        }
                    }
                    val lines = remember(document.text) { PrologSourceLines(document.text) }
                    val highlighted = remember(document.text, tokens) {
                        PrologVisualTransformation(tokens).filter(AnnotatedString(document.text)).text
                    }
                    val activeLine = lines.clampLine(selectedLine)
                    val scrollState = rememberLazyListState()
                    LaunchedEffect(document.source, document.text, activeLine) {
                        scrollState.scrollToItem(activeLine - 1)
                    }
                    Row(Modifier.fillMaxWidth()) {
                        OutlinedTextField(
                            value = lineInput,
                            onValueChange = { lineInput = it.take(9) },
                            modifier = Modifier.weight(1f),
                            label = { Text("Line 1–${lines.ranges.size}") },
                            singleLine = true,
                            colors = studioFieldColors(),
                        )
                        TextButton(onClick = {
                            selectedLine = lines.clampLine(lineInput.toIntOrNull() ?: activeLine)
                            lineInput = selectedLine.toString()
                        }, enabled = lineInput.toIntOrNull() != null) { Text("Go") }
                    }
                    SelectionContainer(Modifier.weight(1f)) {
                        LazyColumn(state = scrollState, modifier = Modifier.fillMaxSize()) {
                            items(lines.ranges.size, key = { it }) { index ->
                                val range = lines.ranges[index]
                                Surface(color = if (index + 1 == activeLine) tokens.ambientGlow else tokens.surface) {
                                    Row(Modifier.fillMaxWidth().padding(vertical = 3.dp)) {
                                        Text("${index + 1}", modifier = Modifier.widthIn(min = 40.dp).padding(end = 8.dp),
                                            color = tokens.textMuted, fontFamily = FontFamily.Monospace,
                                            style = MaterialTheme.typography.bodySmall)
                                        Text(highlighted.subSequence(range.start, range.endExclusive),
                                            modifier = Modifier.weight(1f), color = tokens.text,
                                            fontFamily = FontFamily.Monospace,
                                            style = MaterialTheme.typography.bodySmall)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
