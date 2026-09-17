package ai.zara.app.prolog

import ai.zara.app.device.AppSearchAdapter
import ai.zara.app.device.DeviceActionArguments
import ai.zara.app.device.DeviceActionErrorCode
import ai.zara.app.device.DeviceActionResult
import ai.zara.app.device.OpenAppAdapter
import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture

sealed interface AndroidAutomationAction {
    data class OpenApp(val alias: String) : AndroidAutomationAction
    data class SearchApp(val alias: String, val query: String) : AndroidAutomationAction
}

data class AndroidAutomationPlan(
    val name: String,
    val actions: List<AndroidAutomationAction>,
)

sealed interface AndroidAutomationResult {
    data class Completed(val plan: AndroidAutomationPlan) : AndroidAutomationResult
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
        val open = parseCall(term, "open_app", 1)
        if (open != null) {
            return AndroidAutomationAction.OpenApp(parseAtom(open.single(), "app alias"))
        }
        val search = parseCall(term, "app_search", 2)
        if (search != null) {
            return AndroidAutomationAction.SearchApp(
                alias = parseAtom(search[0], "app alias"),
                query = parseText(search[1]),
            )
        }
        throw IllegalArgumentException("unsupported Android automation action")
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

    private fun parseText(raw: String): String {
        val value = raw.trim()
        val decoded = when {
            value.length >= 2 && value.first() == '\'' && value.last() == '\'' ->
                unescape(value.substring(1, value.length - 1), '\'')
            value.length >= 2 && value.first() == '"' && value.last() == '"' ->
                unescape(value.substring(1, value.length - 1), '"')
            else -> throw IllegalArgumentException("automation text must be quoted")
        }
        require(decoded.isNotBlank()) { "automation text must not be blank" }
        require(decoded.encodeToByteArray().size <= 512) { "automation text exceeds byte limit" }
        require(decoded.none { it.code < 0x20 }) { "automation text contains control characters" }
        return decoded
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
    private val appSearch: AppSearchAdapter,
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
            val outcome = when (action) {
                is AndroidAutomationAction.OpenApp -> openApp.execute(
                    DeviceActionArguments.OpenApp(action.alias),
                )
                is AndroidAutomationAction.SearchApp -> appSearch.execute(
                    DeviceActionArguments.AppSearch(action.alias, action.query),
                )
            }
            if (outcome is DeviceActionResult.Error) {
                return AndroidAutomationResult.Failed(plan, index, outcome.code)
            }
        }
        return AndroidAutomationResult.Completed(plan)
    }
}
