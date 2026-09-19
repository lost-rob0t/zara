package ai.zara.editor.core

import java.util.UUID
import java.util.concurrent.atomic.AtomicLong

/** Immutable selection in UTF-16 offsets, matching Compose TextRange semantics. */
data class EditorSelection(val start: Int, val end: Int) {
    init {
        require(start >= 0) { "selection start must be >= 0" }
        require(end >= 0) { "selection end must be >= 0" }
    }

    val isCollapsed: Boolean get() = start == end
    val normalizedStart: Int get() = minOf(start, end)
    val normalizedEnd: Int get() = maxOf(start, end)
}

data class EditorDiagnostic(
    val message: String,
    val severity: Severity,
    val start: Int? = null,
    val end: Int? = null,
) {
    enum class Severity { INFO, WARNING, ERROR }
}

data class EditorSymbol(
    val name: String,
    val kind: String,
    val start: Int,
    val end: Int,
)

data class EditorSnapshot(
    val bufferId: String,
    val text: String,
    val revision: Long,
    val cursor: Int,
    val selection: EditorSelection,
    val languageId: String,
    val diagnostics: List<EditorDiagnostic> = emptyList(),
    val symbols: List<EditorSymbol> = emptyList(),
) {
    init {
        require(cursor in 0..text.length) { "cursor outside buffer" }
        require(selection.normalizedEnd <= text.length) { "selection outside buffer" }
    }
}

data class TextPatch(
    val start: Int,
    val end: Int,
    val replacement: String,
) {
    init {
        require(start >= 0) { "patch start must be >= 0" }
        require(end >= start) { "patch end must be >= start" }
    }
}

enum class EditOrigin { USER, VOICE, MODEL, PROLOG, PYTHON, TOOL }

data class EditorEditPlan(
    val baseRevision: Long,
    val patches: List<TextPatch>,
    val summary: String,
    val origin: EditOrigin,
    val requiresConfirmation: Boolean = false,
    val requestId: String = UUID.randomUUID().toString(),
)

sealed interface ApplyResult {
    data class Applied(val snapshot: EditorSnapshot) : ApplyResult
    data class Stale(val expectedRevision: Long, val actualRevision: Long) : ApplyResult
    data class Invalid(val reason: String) : ApplyResult
}

/**
 * Single-authority, revisioned text buffer.
 *
 * The caller may host this behind the #996 actor/window ABI. The class deliberately
 * exposes no mutable text reference: voice/model/tool work must return an edit plan
 * fenced to the revision it inspected.
 */
class RevisionedEditorBuffer(
    initialText: String = "",
    private val bufferId: String = UUID.randomUUID().toString(),
    private val languageId: String = "text",
) {
    private var text: String = initialText
    private var revision: Long = 0
    private var cursor: Int = initialText.length
    private var selection: EditorSelection = EditorSelection(cursor, cursor)
    private var diagnostics: List<EditorDiagnostic> = emptyList()
    private var symbols: List<EditorSymbol> = emptyList()

    @Synchronized
    fun snapshot(): EditorSnapshot = EditorSnapshot(
        bufferId = bufferId,
        text = text,
        revision = revision,
        cursor = cursor,
        selection = selection,
        languageId = languageId,
        diagnostics = diagnostics,
        symbols = symbols,
    )

    @Synchronized
    fun replaceFromUser(
        newText: String,
        newCursor: Int = newText.length,
        newSelection: EditorSelection = EditorSelection(newCursor, newCursor),
    ): EditorSnapshot {
        require(newCursor in 0..newText.length) { "cursor outside replacement text" }
        require(newSelection.normalizedEnd <= newText.length) { "selection outside replacement text" }
        text = newText
        cursor = newCursor
        selection = newSelection
        revision += 1
        return snapshot()
    }

    @Synchronized
    fun updateAnalysis(
        baseRevision: Long,
        newDiagnostics: List<EditorDiagnostic>,
        newSymbols: List<EditorSymbol>,
    ): Boolean {
        if (baseRevision != revision) return false
        diagnostics = newDiagnostics
        symbols = newSymbols
        return true
    }

    @Synchronized
    fun apply(plan: EditorEditPlan): ApplyResult {
        if (plan.baseRevision != revision) {
            return ApplyResult.Stale(plan.baseRevision, revision)
        }
        if (plan.patches.isEmpty()) {
            return ApplyResult.Invalid("edit plan has no patches")
        }
        val ordered = plan.patches.sortedBy { it.start }
        ordered.forEach { patch ->
            if (patch.end > text.length) {
                return ApplyResult.Invalid("patch range ${patch.start}..${patch.end} outside ${text.length}")
            }
        }
        ordered.zipWithNext().forEach { (left, right) ->
            if (left.end > right.start) {
                return ApplyResult.Invalid("overlapping patches are not allowed")
            }
        }

        var updated = text
        ordered.asReversed().forEach { patch ->
            updated = updated.replaceRange(patch.start, patch.end, patch.replacement)
        }
        text = updated
        revision += 1

        val last = ordered.last()
        val deltaBeforeLast = ordered
            .dropLast(1)
            .sumOf { it.replacement.length - (it.end - it.start) }
        cursor = (last.start + deltaBeforeLast + last.replacement.length).coerceIn(0, text.length)
        selection = EditorSelection(cursor, cursor)
        diagnostics = emptyList()
        symbols = emptyList()
        return ApplyResult.Applied(snapshot())
    }
}

data class OperationToken(
    val generation: Long,
    val baseRevision: Long,
)

/** Fences late STT/model/runtime replies and supports immediate cancellation. */
class OperationFence {
    private val generation = AtomicLong(0)

    fun begin(baseRevision: Long): OperationToken =
        OperationToken(generation.incrementAndGet(), baseRevision)

    fun cancel() {
        generation.incrementAndGet()
    }

    fun accepts(token: OperationToken, currentRevision: Long): Boolean =
        token.generation == generation.get() && token.baseRevision == currentRevision
}

sealed interface VoiceCodeIntent {
    data class Insert(val text: String) : VoiceCodeIntent
    data class ReplaceSelection(val text: String) : VoiceCodeIntent
    data class WrapSelection(val prefix: String, val suffix: String) : VoiceCodeIntent
    data class GoToLine(val line: Int) : VoiceCodeIntent
    data class GoToSymbol(val name: String) : VoiceCodeIntent
    data class Find(val query: String) : VoiceCodeIntent
    data class RenameSymbol(val oldName: String, val newName: String) : VoiceCodeIntent
    data class ExplainSelection(val prompt: String? = null) : VoiceCodeIntent
    data class Run(val target: String = "default") : VoiceCodeIntent
    data object Undo : VoiceCodeIntent
    data object Redo : VoiceCodeIntent
}

sealed interface VoiceAction {
    data class Edit(val plan: EditorEditPlan) : VoiceAction
    data class Navigate(val target: NavigationTarget) : VoiceAction
    data class ReadOnly(val command: ReadOnlyCommand) : VoiceAction
    data class RuntimeCommand(val command: String, val argument: String? = null) : VoiceAction
    data class LanguageService(val request: LanguageServiceRequest) : VoiceAction
    data class Rejected(val reason: String) : VoiceAction
}

sealed interface NavigationTarget {
    data class Line(val line: Int) : NavigationTarget
    data class Symbol(val name: String) : NavigationTarget
    data class Search(val query: String) : NavigationTarget
}

sealed interface ReadOnlyCommand {
    data class Explain(val start: Int, val end: Int, val prompt: String?) : ReadOnlyCommand
}

data class LanguageServiceRequest(
    val kind: Kind,
    val symbol: String,
    val replacement: String,
    val baseRevision: Long,
) {
    enum class Kind { RENAME }
}

/** Deterministic voice commands. Generative requests are delegated to a model planner above this layer. */
object VoiceCodePlanner {
    fun plan(intent: VoiceCodeIntent, snapshot: EditorSnapshot): VoiceAction = when (intent) {
        is VoiceCodeIntent.Insert -> VoiceAction.Edit(
            EditorEditPlan(
                baseRevision = snapshot.revision,
                patches = listOf(TextPatch(snapshot.cursor, snapshot.cursor, intent.text)),
                summary = "Insert spoken code",
                origin = EditOrigin.VOICE,
            ),
        )
        is VoiceCodeIntent.ReplaceSelection -> {
            if (snapshot.selection.isCollapsed) VoiceAction.Rejected("no active selection")
            else VoiceAction.Edit(
                EditorEditPlan(
                    baseRevision = snapshot.revision,
                    patches = listOf(
                        TextPatch(
                            snapshot.selection.normalizedStart,
                            snapshot.selection.normalizedEnd,
                            intent.text,
                        ),
                    ),
                    summary = "Replace selected code",
                    origin = EditOrigin.VOICE,
                    requiresConfirmation = true,
                ),
            )
        }
        is VoiceCodeIntent.WrapSelection -> {
            if (snapshot.selection.isCollapsed) VoiceAction.Rejected("no active selection")
            else VoiceAction.Edit(
                EditorEditPlan(
                    baseRevision = snapshot.revision,
                    patches = listOf(
                        TextPatch(
                            snapshot.selection.normalizedStart,
                            snapshot.selection.normalizedStart,
                            intent.prefix,
                        ),
                        TextPatch(
                            snapshot.selection.normalizedEnd,
                            snapshot.selection.normalizedEnd,
                            intent.suffix,
                        ),
                    ),
                    summary = "Wrap selected code",
                    origin = EditOrigin.VOICE,
                ),
            )
        }
        is VoiceCodeIntent.GoToLine -> {
            if (intent.line < 1) VoiceAction.Rejected("line must be >= 1")
            else VoiceAction.Navigate(NavigationTarget.Line(intent.line))
        }
        is VoiceCodeIntent.GoToSymbol -> VoiceAction.Navigate(NavigationTarget.Symbol(intent.name))
        is VoiceCodeIntent.Find -> VoiceAction.Navigate(NavigationTarget.Search(intent.query))
        is VoiceCodeIntent.RenameSymbol -> VoiceAction.LanguageService(
            LanguageServiceRequest(
                kind = LanguageServiceRequest.Kind.RENAME,
                symbol = intent.oldName,
                replacement = intent.newName,
                baseRevision = snapshot.revision,
            ),
        )
        is VoiceCodeIntent.ExplainSelection -> {
            if (snapshot.selection.isCollapsed) VoiceAction.Rejected("no active selection")
            else VoiceAction.ReadOnly(
                ReadOnlyCommand.Explain(
                    snapshot.selection.normalizedStart,
                    snapshot.selection.normalizedEnd,
                    intent.prompt,
                ),
            )
        }
        is VoiceCodeIntent.Run -> VoiceAction.RuntimeCommand("run", intent.target)
        VoiceCodeIntent.Undo -> VoiceAction.RuntimeCommand("undo")
        VoiceCodeIntent.Redo -> VoiceAction.RuntimeCommand("redo")
    }
}

data class LanguageAnalysis(
    val diagnostics: List<EditorDiagnostic> = emptyList(),
    val symbols: List<EditorSymbol> = emptyList(),
)

interface EditorLanguageService {
    val languageId: String
    fun analyze(snapshot: EditorSnapshot): LanguageAnalysis
    fun rename(snapshot: EditorSnapshot, symbol: String, replacement: String): EditorEditPlan?
}
