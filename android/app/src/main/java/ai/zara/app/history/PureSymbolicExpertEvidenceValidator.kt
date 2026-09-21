package ai.zara.app.history

import org.json.JSONArray
import org.json.JSONObject

/** Fail-closed trust fence for persisted expert evidence in pure-symbolic mode. */
internal object PureSymbolicExpertEvidenceValidator {
    private val forbiddenMetadataKeys = setOf(
        "usage",
        "provider",
        "provider_calls",
        "providers_enabled",
        "provider_id",
        "provider_name",
        "max_model_calls",
        "model",
        "model_id",
        "model_name",
        "model_provider",
        "tokens",
        "token_usage",
        "prompt_tokens",
        "completion_tokens",
        "input_tokens",
        "output_tokens",
        "total_tokens",
    )

    fun requireTrusted(encoded: String, name: String) {
        val evidence = try {
            JSONArray(encoded)
        } catch (error: Exception) {
            throw IllegalArgumentException("$name must be valid JSON expert evidence", error)
        }
        for (index in 0 until evidence.length()) {
            val entry = evidence.optJSONObject(index)
                ?: throw IllegalArgumentException("$name[$index] must be a JSON object")
            validateObject(entry, "$name[$index]", allowModelCalls = true)
        }
    }

    private fun validateObject(
        value: JSONObject,
        path: String,
        allowModelCalls: Boolean,
    ) {
        if (value.has("expert_id")) {
            val expertId = value.opt("expert_id")
            require(expertId is String && expertId.isNotEmpty()) {
                "$path.expert_id must be a non-empty string"
            }
            require(value.has("model_calls")) {
                "$path.model_calls is required for typed expert evidence"
            }
        }

        val keys = value.keys()
        while (keys.hasNext()) {
            val key = keys.next()
            val memberPath = "$path.$key"
            val member = value.opt(key)
            if (key == "model_calls" && allowModelCalls) {
                require((member is Int || member is Long) && (member as Number).toLong() == 0L) {
                    "$namePrefix model_calls must be an exact integer zero at $memberPath"
                }
                continue
            }
            require(key != "model_calls" && !isForbiddenMetadataKey(key)) {
                "$namePrefix forbids provider/model/token usage metadata at $memberPath"
            }
            validateNested(member, memberPath)
        }
    }

    private fun validateNested(value: Any?, path: String) {
        when (value) {
            is JSONObject -> validateObject(value, path, allowModelCalls = false)
            is JSONArray -> {
                for (index in 0 until value.length()) {
                    validateNested(value.opt(index), "$path[$index]")
                }
            }
        }
    }

    private fun isForbiddenMetadataKey(key: String): Boolean {
        val normalized = key.lowercase()
        return normalized in forbiddenMetadataKeys ||
            normalized.startsWith("provider_") ||
            normalized.endsWith("_tokens")
    }

    private const val namePrefix = "expertEvidenceJson"
}
