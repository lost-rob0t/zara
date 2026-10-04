package ai.zara.app.prolog

import ai.zara.app.accessibility.AccessibilityAutomationAction
import ai.zara.app.accessibility.AccessibilityAutomationAdapter
import ai.zara.app.accessibility.AccessibilityGlobalAction
import ai.zara.app.accessibility.AccessibilitySelector
import ai.zara.app.automation.AdbAutomationKey
import ai.zara.app.automation.AdbAutomationPort
import ai.zara.app.control.AndroidControlAccess
import ai.zara.app.device.AppSearchAdapter
import ai.zara.app.device.DeviceActionArguments
import ai.zara.app.device.DeviceActionErrorCode
import ai.zara.app.device.DeviceActionResult
import ai.zara.app.device.OpenAppAdapter
import ai.zara.app.device.OpenUriAdapter
import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture

sealed interface AndroidAutomationAction {
    data class OpenApp(val alias: String) : AndroidAutomationAction
    data class OpenUri(val uri: String) : AndroidAutomationAction
    data class SearchApp(val alias: String, val query: String) : AndroidAutomationAction
    data class UiClick(val selector: AccessibilitySelector) : AndroidAutomationAction
    data class UiSetText(val selector: AccessibilitySelector, val text: String) : AndroidAutomationAction
    data class UiScrollForward(val selector: AccessibilitySelector) : AndroidAutomationAction
    data class GlobalAction(val action: AccessibilityGlobalAction) : AndroidAutomationAction
    data class AdbTap(val x: Int, val y: Int) : AndroidAutomationAction
    data class AdbSwipe(
        val x1: Int,
        val y1: Int,
        val x2: Int,
        val y2: Int,
        val durationMs: Int,
    ) : AndroidAutomationAction
    data class AdbText(val text: String) : AndroidAutomationAction
    data class AdbKey(val key: AdbAutomationKey) : AndroidAutomationAction
    data class AdbWait(val durationMs: Int) : AndroidAutomationAction
}

data class AndroidAutomationPlan(
    val name: String,
    val actions: List<AndroidAutomationAction>,
)

sealed interface AndroidAutomationResult {
    data class Completed(val plan: AndroidAutomationPlan) : AndroidAutomationResult
    data class NeedsAccess(
        val plan: AndroidAutomationPlan,
        val actionIndex: Int,
        val access: AndroidControlAccess,
    ) : AndroidAutomationResult
    data class Failed(
        val plan: AndroidAutomationPlan,
        val actionIndex: Int,
        val error: DeviceActionErrorCode,
    ) : AndroidAutomationResult
}

object AndroidAutomationPlanParser {
    private val atom = Regex("[a-z][a-z0-9_]{0,63}")

    fun parse(name: String, term: String): AndroidAutomationPlan {
        require(atom.matches(name)) { "automation name must be a bounded Prolog atom" }
        val source = term.trim()
        require(source.startsWith("actions([") && source.endsWith("])")) {
            "automation result must be actions([...])"
        }
        val body = source.removePrefix("actions([").removeSuffix("])").trim()
        if (body.isEmpty()) return AndroidAutomationPlan(name, emptyList())
        val actionTerms = splitTopLevel(body)
        require(actionTerms.size <= 32) { "automation exceeds action limit" }
        return AndroidAutomationPlan(name, actionTerms.map(::parseAction))
    }

    private fun parseAction(term: String): AndroidAutomationAction {
        parseCall(term, "open_app", 1)?.let { args ->
            return AndroidAutomationAction.OpenApp(parseAtom(args.single(), "app alias"))
        }
        parseCall(term, "open_uri", 1)?.let { args ->
            return AndroidAutomationAction.OpenUri(parseText(args.single(), maxBytes = 2_048))
        }
        parseCall(term, "app_search", 2)?.let { args ->
            return AndroidAutomationAction.SearchApp(
                alias = parseAtom(args[0], "app alias"),
                query = parseText(args[1]),
            )
        }
        parseCall(term, "adb_tap", 2)?.let { args ->
            return AndroidAutomationAction.AdbTap(
                x = parseInt(args[0], "ADB x", 0, 16_384),
                y = parseInt(args[1], "ADB y", 0, 16_384),
            )
        }
        parseCall(term, "adb_swipe", 5)?.let { args ->
            return AndroidAutomationAction.AdbSwipe(
                x1 = parseInt(args[0], "ADB x1", 0, 16_384),
                y1 = parseInt(args[1], "ADB y1", 0, 16_384),
                x2 = parseInt(args[2], "ADB x2", 0, 16_384),
                y2 = parseInt(args[3], "ADB y2", 0, 16_384),
                durationMs = parseInt(args[4], "ADB swipe duration", 1, 5_000),
            )
        }
        parseCall(term, "adb_text", 1)?.let { args ->
            return AndroidAutomationAction.AdbText(parseText(args.single(), maxBytes = 512))
        }
        parseCall(term, "adb_key", 1)?.let { args ->
            return AndroidAutomationAction.AdbKey(
                AdbAutomationKey.fromAtom(parseAtom(args.single(), "ADB key"))
            )
        }
        parseCall(term, "adb_wait", 1)?.let { args ->
            return AndroidAutomationAction.AdbWait(
                parseInt(args.single(), "ADB wait duration", 0, 5_000)
            )
        }
        parseCall(term, "ui_click", 1)?.let { args ->
            return AndroidAutomationAction.UiClick(parseSelector(args.single()))
        }
        parseCall(term, "ui_set_text", 2)?.let { args ->
            return AndroidAutomationAction.UiSetText(
                selector = parseSelector(args[0]),
                text = parseText(args[1], maxBytes = 4 * 1024),
            )
        }
        parseCall(term, "ui_scroll_forward", 1)?.let { args ->
            return AndroidAutomationAction.UiScrollForward(parseSelector(args.single()))
        }
        parseCall(term, "global_action", 1)?.let { args ->
            val action = when (parseAtom(args.single(), "global action")) {
                "back" -> AccessibilityGlobalAction.Back
                "home" -> AccessibilityGlobalAction.Home
                "recents" -> AccessibilityGlobalAction.Recents
                "notifications" -> AccessibilityGlobalAction.Notifications
                else -> throw IllegalArgumentException("unsupported global action")
            }
            return AndroidAutomationAction.GlobalAction(action)
        }
        throw IllegalArgumentException("unsupported Android automation action")
    }

    private fun parseSelector(term: String): AccessibilitySelector {
        parseCall(term, "text", 1)?.let { args ->
            return AccessibilitySelector.Text(parseText(args.single()))
        }
        parseCall(term, "view_id", 1)?.let { args ->
            return AccessibilitySelector.ViewId(parseText(args.single()))
        }
        parseCall(term, "description", 1)?.let { args ->
            return AccessibilitySelector.Description(parseText(args.single()))
        }
        throw IllegalArgumentException("unsupported accessibility selector")
    }

    private fun parseCall(term: String, name: String, arity: Int): List<String>? {
        val trimmed = term.trim()
        if (!trimmed.startsWith("$name(") || !trimmed.endsWith(')')) return null
        val args = splitTopLevel(trimmed.substring(name.length + 1, trimmed.length - 1))
        require(args.size == arity) { "$name/$arity expected" }
        return args
    }

    private fun parseAtom(raw: String, label: String): String {
        val value = raw.trim()
        require(atom.matches(value)) { "$label must be a bounded atom" }
        return value
    }

    private fun parseInt(
        raw: String,
        label: String,
        minimum: Int,
        maximum: Int,
    ): Int {
        val value = raw.trim().toIntOrNull()
            ?: throw IllegalArgumentException("$label must be an integer")
        require(value in minimum..maximum) { "$label is out of range" }
        return value
    }

    private fun parseText(raw: String, maxBytes: Int = 512): String {
        val value = raw.trim()
        val decoded = when {
            value.length >= 2 && value.first() == '\'' && value.last() == '\'' ->
                unescape(value.substring(1, value.length - 1), '\'')
            value.length >= 2 && value.first() == '"' && value.last() == '"' ->
                unescape(value.substring(1, value.length - 1), '"')
            value.startsWith('[') && value.endsWith(']') -> parseTreallaCharList(value)
            else -> throw IllegalArgumentException("automation text must be quoted")
        }
        require(decoded.isNotBlank()) { "automation text must not be blank" }
        require(decoded.encodeToByteArray().size <= maxBytes) { "automation text exceeds byte limit" }
        require(decoded.none { it.code < 0x20 }) { "automation text contains control characters" }
        return decoded
    }

    private fun parseTreallaCharList(value: String): String {
        val body = value.substring(1, value.length - 1).trim()
        if (body.isEmpty()) return ""
        return buildString {
            splitTopLevel(body).forEach { raw ->
                val item = raw.trim()
                val decoded = when {
                    item.length >= 2 && item.first() == '\'' && item.last() == '\'' ->
                        unescape(item.substring(1, item.length - 1), '\'')
                    item.length >= 2 && item.first() == '"' && item.last() == '"' ->
                        unescape(item.substring(1, item.length - 1), '"')
                    else -> item
                }
                require(decoded.codePointCount(0, decoded.length) == 1) {
                    "automation text character list contains a non-character term"
                }
                append(decoded)
            }
        }
    }

    private fun unescape(value: String, quote: Char): String = buildString {
        var escaped = false
        value.forEach { character ->
            if (escaped) {
                append(
                    when (character) {
                        'n' -> '\n'
                        'r' -> '\r'
                        't' -> '\t'
                        '\\' -> '\\'
                        quote -> quote
                        else -> character
                    }
                )
                escaped = false
            } else if (character == '\\') {
                escaped = true
            } else {
                append(character)
            }
        }
        require(!escaped) { "automation text has a trailing escape" }
    }

    private fun splitTopLevel(value: String): List<String> {
        val result = mutableListOf<String>()
        val current = StringBuilder()
        var round = 0
        var square = 0
        var quote: Char? = null
        var escaped = false
        value.forEach { character ->
            if (quote != null) {
                current.append(character)
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == quote) quote = null
                return@forEach
            }
            when (character) {
                '\'', '"' -> {
                    quote = character
                    current.append(character)
                }
                '(' -> { round += 1; current.append(character) }
                ')' -> { round -= 1; current.append(character) }
                '[' -> { square += 1; current.append(character) }
                ']' -> { square -= 1; current.append(character) }
                ',' -> if (round == 0 && square == 0) {
                    result += current.toString().trim()
                    current.clear()
                } else current.append(character)
                else -> current.append(character)
            }
            require(round >= 0 && square >= 0) { "automation term is unbalanced" }
        }
        require(quote == null && round == 0 && square == 0) { "automation term is unbalanced" }
        if (current.isNotBlank()) result += current.toString().trim()
        return result
    }
}

class AndroidAutomationRunner(
    private val queryProlog: (String) -> CompletableFuture<LocalQueryResult>,
    private val openApp: OpenAppAdapter,
    private val openUri: OpenUriAdapter,
    private val appSearch: AppSearchAdapter,
    private val accessibility: AccessibilityAutomationAdapter,
    private val adb: AdbAutomationPort,
    private val accessGranted: (AndroidControlAccess) -> Boolean,
) {
    fun run(name: String): CompletableFuture<AndroidAutomationResult> {
        require(name.matches(Regex("[a-z][a-z0-9_]{0,63}"))) { "automation name is invalid" }
        return queryProlog("automation($name, Result)").thenApply { queryResult ->
            val term = queryResult.terms.singleOrNull()
                ?: throw IllegalArgumentException("automation must resolve to exactly one plan")
            val plan = AndroidAutomationPlanParser.parse(name, term)
            execute(plan)
        }
    }

    private fun execute(plan: AndroidAutomationPlan): AndroidAutomationResult {
        plan.actions.forEachIndexed { index, action ->
            if (requiresAccessibility(action) && !accessGranted(AndroidControlAccess.Accessibility)) {
                return AndroidAutomationResult.NeedsAccess(plan, index, AndroidControlAccess.Accessibility)
            }
            val outcome = when (action) {
                is AndroidAutomationAction.OpenApp -> openApp.execute(
                    DeviceActionArguments.OpenApp(action.alias),
                )
                is AndroidAutomationAction.OpenUri -> openUri.execute(
                    DeviceActionArguments.OpenUri(action.uri),
                )
                is AndroidAutomationAction.SearchApp -> appSearch.execute(
                    DeviceActionArguments.AppSearch(action.alias, action.query),
                )
                is AndroidAutomationAction.UiClick -> accessibility.execute(
                    AccessibilityAutomationAction.Click(action.selector),
                )
                is AndroidAutomationAction.UiSetText -> accessibility.execute(
                    AccessibilityAutomationAction.SetText(action.selector, action.text),
                )
                is AndroidAutomationAction.UiScrollForward -> accessibility.execute(
                    AccessibilityAutomationAction.ScrollForward(action.selector),
                )
                is AndroidAutomationAction.GlobalAction -> accessibility.execute(
                    AccessibilityAutomationAction.Global(action.action),
                )
                is AndroidAutomationAction.AdbTap -> adb.tap(action.x, action.y)
                is AndroidAutomationAction.AdbSwipe -> adb.swipe(
                    action.x1,
                    action.y1,
                    action.x2,
                    action.y2,
                    action.durationMs,
                )
                is AndroidAutomationAction.AdbText -> adb.typeText(action.text)
                is AndroidAutomationAction.AdbKey -> adb.key(action.key)
                is AndroidAutomationAction.AdbWait -> adb.wait(action.durationMs)
            }
            if (outcome is DeviceActionResult.Error) {
                return AndroidAutomationResult.Failed(plan, index, outcome.code)
            }
        }
        return AndroidAutomationResult.Completed(plan)
    }

    private fun requiresAccessibility(action: AndroidAutomationAction): Boolean = when (action) {
        is AndroidAutomationAction.UiClick,
        is AndroidAutomationAction.UiSetText,
        is AndroidAutomationAction.UiScrollForward,
        is AndroidAutomationAction.GlobalAction -> true
        is AndroidAutomationAction.OpenApp,
        is AndroidAutomationAction.OpenUri,
        is AndroidAutomationAction.SearchApp,
        is AndroidAutomationAction.AdbTap,
        is AndroidAutomationAction.AdbSwipe,
        is AndroidAutomationAction.AdbText,
        is AndroidAutomationAction.AdbKey,
        is AndroidAutomationAction.AdbWait -> false
    }
}
