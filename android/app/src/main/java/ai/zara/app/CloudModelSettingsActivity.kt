package ai.zara.app

import ai.zara.app.model.AndroidCloudModelStorage
import ai.zara.app.model.CloudModelConfig
import ai.zara.app.model.CloudModelProvider
import android.app.Activity
import android.os.Bundle
import android.text.InputType
import android.view.View
import android.view.ViewGroup
import android.widget.AdapterView
import android.widget.ArrayAdapter
import android.widget.Button
import android.widget.CheckBox
import android.widget.EditText
import android.widget.LinearLayout
import android.widget.ScrollView
import android.widget.Spinner
import android.widget.TextView
import java.io.File

class CloudModelSettingsActivity : Activity() {
    private lateinit var endpointField: EditText
    private lateinit var modelField: EditText
    private lateinit var appNameField: EditText
    private lateinit var apiKeyField: EditText
    private lateinit var enabledCheck: CheckBox
    private lateinit var presetSpinner: Spinner
    private lateinit var policyText: TextView
    private lateinit var statusText: TextView

    private val runtimeRoot: File by lazy {
        File(noBackupFilesDir, "zara/prolog-runtime").also { root ->
            check(root.mkdirs() || root.isDirectory) { "Model settings directory is unavailable" }
        }
    }
    private val configStore by lazy { AndroidCloudModelStorage.configStore(runtimeRoot) }
    private val apiKeyStore by lazy { AndroidCloudModelStorage.apiKeyStore(runtimeRoot) }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        title = "Model Providers"
        val current = configStore.load()
        setContentView(buildView(current))
    }

    private fun buildView(current: CloudModelConfig): View {
        val content = LinearLayout(this).apply {
            orientation = LinearLayout.VERTICAL
            setPadding(dp(20), dp(20), dp(20), dp(32))
        }

        content.addView(TextView(this).apply {
            text = "Cloud model providers"
            textSize = 24f
        })
        content.addView(TextView(this).apply {
            text = "API keys are wrapped by Android Keystore. Prolog may define llm_app_name/1; secrets never belong in Prolog source."
            textSize = 14f
            setPadding(0, dp(8), 0, dp(16))
        })

        enabledCheck = CheckBox(this).apply {
            text = "Enable cloud model fallback"
            isChecked = current.enabled
        }
        content.addView(enabledCheck)

        content.addView(label("Provider"))
        presetSpinner = Spinner(this)
        val presets = ProviderPreset.entries
        presetSpinner.adapter = ArrayAdapter(
            this,
            android.R.layout.simple_spinner_dropdown_item,
            presets.map(ProviderPreset::label),
        )
        presetSpinner.setSelection(indexFor(current))
        content.addView(presetSpinner, matchWidth())

        content.addView(label("OpenAI-compatible base URL"))
        endpointField = EditText(this).apply {
            inputType = InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_VARIATION_URI
            setText(current.endpoint)
            setSingleLine(true)
        }
        content.addView(endpointField, matchWidth())

        content.addView(label("Model"))
        modelField = EditText(this).apply {
            setText(current.model)
            hint = "provider/model-id"
            setSingleLine(true)
        }
        content.addView(modelField, matchWidth())

        content.addView(label("LLM app name"))
        appNameField = EditText(this).apply {
            setText(current.appName)
            hint = CloudModelConfig.DEFAULT_APP_NAME
            setSingleLine(true)
        }
        content.addView(appNameField, matchWidth())

        content.addView(label("API key"))
        apiKeyField = EditText(this).apply {
            inputType = InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_VARIATION_PASSWORD
            hint = if (apiKeyStore.exists()) "Stored securely — leave blank to keep" else "Paste API key"
            setSingleLine(true)
        }
        content.addView(apiKeyField, matchWidth())

        policyText = TextView(this).apply {
            textSize = 13f
            setPadding(0, dp(12), 0, dp(12))
        }
        content.addView(policyText)

        val save = Button(this).apply {
            text = "Save provider"
            setOnClickListener { save() }
        }
        content.addView(save, matchWidth())

        val clearKey = Button(this).apply {
            text = "Clear stored API key"
            setOnClickListener {
                runCatching { apiKeyStore.clear() }
                    .onSuccess {
                        apiKeyField.text.clear()
                        apiKeyField.hint = "Paste API key"
                        statusText.text = "Stored API key cleared."
                    }
                    .onFailure { statusText.text = it.message ?: "Could not clear API key." }
            }
        }
        content.addView(clearKey, matchWidth())

        statusText = TextView(this).apply {
            text = if (apiKeyStore.exists()) "API key configured." else "No API key configured."
            textSize = 13f
            setPadding(0, dp(14), 0, 0)
        }
        content.addView(statusText)

        presetSpinner.onItemSelectedListener = object : AdapterView.OnItemSelectedListener {
            override fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
                applyPreset(presets[position])
            }

            override fun onNothingSelected(parent: AdapterView<*>?) = Unit
        }
        applyPreset(presets[presetSpinner.selectedItemPosition], preserveEndpoint = true)

        return ScrollView(this).apply { addView(content) }
    }

    private fun save() {
        val preset = ProviderPreset.entries[presetSpinner.selectedItemPosition]
        val endpoint = preset.endpoint ?: endpointField.text.toString().trim()
        val result = runCatching {
            val saved = configStore.save(
                CloudModelConfig(
                    enabled = enabledCheck.isChecked,
                    provider = preset.provider,
                    endpoint = endpoint,
                    model = modelField.text.toString(),
                    appName = appNameField.text.toString(),
                )
            )
            val key = apiKeyField.text.toString()
            if (key.isNotBlank()) {
                apiKeyStore.save(key)
                apiKeyField.text.clear()
                apiKeyField.hint = "Stored securely — leave blank to keep"
            }
            saved
        }
        result.onSuccess { saved ->
            statusText.text = buildString {
                append("Saved ")
                append(saved.provider.wireName)
                append(" · ")
                append(saved.model.ifBlank { "model not selected" })
                if (saved.provider.codingOnly) append(" · explicit /code requests only")
                if (apiKeyStore.exists()) append(" · API key configured")
            }
        }.onFailure { error ->
            statusText.text = error.message ?: "Provider settings could not be saved."
        }
    }

    private fun applyPreset(preset: ProviderPreset, preserveEndpoint: Boolean = false) {
        endpointField.isEnabled = preset.endpoint == null
        if (!preserveEndpoint && preset.endpoint != null) endpointField.setText(preset.endpoint)
        policyText.text = when (preset) {
            ProviderPreset.ZAI_CODING ->
                "Z.AI Coding Plan uses the coding-only endpoint. Zara will never use this profile for ordinary assistant fallback; invoke it with /code <task>."
            ProviderPreset.OPENROUTER ->
                "OpenRouter uses the OpenAI-compatible chat-completions API. The configured LLM app name is sent as X-Title."
            ProviderPreset.STARINTEL ->
                "StarIntel preset uses llm.starintel.actor as a generic OpenAI-compatible endpoint."
            ProviderPreset.STATINTEL ->
                "StatIntel preset uses llm.statintel.actor as a generic OpenAI-compatible endpoint."
            ProviderPreset.GENERIC ->
                "Generic mode accepts any HTTPS OpenAI-compatible base URL and appends /chat/completions."
        }
    }

    private fun indexFor(config: CloudModelConfig): Int = when {
        config.provider == CloudModelProvider.ZAI_CODING_PLAN -> ProviderPreset.ZAI_CODING.ordinal
        config.provider == CloudModelProvider.OPENROUTER -> ProviderPreset.OPENROUTER.ordinal
        config.endpoint == CloudModelConfig.DEFAULT_STARINTEL_ENDPOINT -> ProviderPreset.STARINTEL.ordinal
        config.endpoint == CloudModelConfig.STATINTEL_ENDPOINT -> ProviderPreset.STATINTEL.ordinal
        else -> ProviderPreset.GENERIC.ordinal
    }

    private fun label(text: String): TextView = TextView(this).apply {
        this.text = text
        textSize = 14f
        setPadding(0, dp(14), 0, dp(4))
    }

    private fun matchWidth() = LinearLayout.LayoutParams(
        ViewGroup.LayoutParams.MATCH_PARENT,
        ViewGroup.LayoutParams.WRAP_CONTENT,
    )

    private fun dp(value: Int): Int = (value * resources.displayMetrics.density).toInt()

    private enum class ProviderPreset(
        val label: String,
        val provider: CloudModelProvider,
        val endpoint: String?,
    ) {
        GENERIC("Generic OpenAI-compatible", CloudModelProvider.OPENAI_COMPATIBLE, null),
        STARINTEL(
            "StarIntel / llm.starintel.actor",
            CloudModelProvider.OPENAI_COMPATIBLE,
            CloudModelConfig.DEFAULT_STARINTEL_ENDPOINT,
        ),
        STATINTEL(
            "StatIntel / llm.statintel.actor",
            CloudModelProvider.OPENAI_COMPATIBLE,
            CloudModelConfig.STATINTEL_ENDPOINT,
        ),
        OPENROUTER(
            "OpenRouter",
            CloudModelProvider.OPENROUTER,
            CloudModelConfig.OPENROUTER_ENDPOINT,
        ),
        ZAI_CODING(
            "Z.AI Coding Plan",
            CloudModelProvider.ZAI_CODING_PLAN,
            CloudModelConfig.ZAI_CODING_ENDPOINT,
        ),
    }
}
