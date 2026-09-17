package ai.zara.app.email

import org.json.JSONArray
import org.json.JSONObject

/**
 * Local Android tool boundary for the email plugin.
 *
 * EmailPrologPlugin owns model-context installation for the process lifetime.
 * The generated spam-rule path is supplied by trusted app configuration and is
 * never accepted from model/tool arguments.
 */
class AndroidEmailRuntimeBinding(
    private val plugin: AndroidEmailPlugin,
    private val generatedSpamRulePath: String,
) {
    init {
        require(generatedSpamRulePath.isNotBlank()) { "generated spam rule path is required" }
    }

    val toolNames: Set<String>
        get() = plugin.toolNames

    val mutatingToolNames: Set<String> = setOf(
        "email_send",
        "email_reply",
        "email_apply_rules",
        "email_refresh_spam_rules",
    )

    fun invoke(toolName: String, arguments: JSONObject = JSONObject()): JSONObject {
        require(toolName in plugin.toolNames) { "unknown email tool" }
        return when (toolName) {
            "email_accounts" -> JSONObject().put("accounts", JSONArray(plugin.accounts()))
            "email_search" -> JSONObject().put(
                "messages",
                JSONArray(
                    plugin.search(
                        arguments.requireString("account"),
                        arguments.optString("query", ""),
                        arguments.optInt("limit", 20),
                    ).map(::messageJson)
                )
            )
            "email_read" -> messageJson(
                plugin.read(arguments.requireString("account"), arguments.requireString("message_id"))
            )
            "email_send" -> {
                plugin.send(
                    arguments.requireString("account"),
                    arguments.requireString("to"),
                    arguments.requireString("subject"),
                    arguments.requireString("body"),
                    arguments.optString("cc", ""),
                    arguments.optString("bcc", ""),
                )
                JSONObject().put("status", "sent")
            }
            "email_reply" -> {
                plugin.reply(
                    arguments.requireString("account"),
                    arguments.requireString("message_id"),
                    arguments.requireString("body"),
                )
                JSONObject().put("status", "sent")
            }
            "email_classify_spam" -> JSONObject(
                plugin.classifySpam(
                    arguments.requireString("account"),
                    arguments.requireString("message_id"),
                )
            )
            "email_apply_rules" -> JSONObject(
                plugin.applyRules(
                    arguments.requireString("account"),
                    arguments.requireString("message_id"),
                    arguments.optBoolean("dry_run", true),
                )
            )
            "email_refresh_spam_rules" -> {
                val feeds = arguments.optJSONArray("feeds")?.let(::parseFeeds) ?: emptyList()
                JSONObject()
                    .put("rules", plugin.refreshSpamRules(feeds, generatedSpamRulePath))
                    .put("path", generatedSpamRulePath)
            }
            "email_prolog_api" -> JSONObject()
                .put("context", plugin.modelTurnContext)
                .put("tools", JSONArray(plugin.toolNames.toList()))
                .put("mutating_tools", JSONArray(mutatingToolNames.toList()))
            else -> error("unreachable")
        }
    }

    private fun parseFeeds(array: JSONArray): List<SpamFeed> = (0 until array.length()).map { index ->
        val item = array.getJSONObject(index)
        SpamFeed(
            name = item.requireString("name"),
            url = item.requireString("url"),
            kind = item.requireString("kind"),
            weight = item.optInt("weight", 50),
        )
    }

    private fun messageJson(message: AndroidEmailMessage): JSONObject = JSONObject()
        .put("id", message.id)
        .put("from", message.from)
        .put("to", message.to)
        .put("subject", message.subject)
        .put("date", message.date)
        .put("body", message.body)
        .put("message_id", message.messageId)

    private fun JSONObject.requireString(name: String): String {
        val value = optString(name, "").trim()
        require(value.isNotEmpty()) { "$name is required" }
        return value
    }
}
