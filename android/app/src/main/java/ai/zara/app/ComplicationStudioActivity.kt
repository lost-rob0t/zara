package ai.zara.app

import ai.zara.app.model.AndroidCloudModelStorage
import ai.zara.app.model.CloudModelPurpose
import ai.zara.app.prolog.PrologProjectIdentityResolver
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.wear.WearComplicationTransfer
import ai.zara.ui.complication.PrologComplicationTemplateCompiler
import android.app.Activity
import android.os.Bundle
import android.text.InputType
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import android.widget.EditText
import android.widget.LinearLayout
import android.widget.ScrollView
import android.widget.TextView
import java.io.File

class ComplicationStudioActivity : Activity() {
    private lateinit var requestField: EditText
    private lateinit var sourceField: EditText
    private lateinit var statusText: TextView
    private lateinit var generateButton: Button
    private lateinit var pushButton: Button

    private val runtimeRoot: File by lazy {
        File(noBackupFilesDir, "zara/prolog-runtime").also { root ->
            check(root.mkdirs() || root.isDirectory) { "Model runtime directory is unavailable" }
        }
    }
    private val workspace: PrologWorkspace by lazy {
        PrologWorkspace(File(filesDir, "prolog-workspace"))
    }
    private val cloudModel by lazy { AndroidCloudModelStorage.coordinator(runtimeRoot) }
    private val transfer by lazy { WearComplicationTransfer(this) }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        title = "Prolog Complications"
        setContentView(buildView())
    }

    override fun onDestroy() {
        if (::generateButton.isInitialized) generateButton.isEnabled = false
        runCatching { cloudModel.close() }
        super.onDestroy()
    }

    private fun buildView(): View {
        val content = LinearLayout(this).apply {
            orientation = LinearLayout.VERTICAL
            setPadding(dp(20), dp(20), dp(20), dp(32))
        }

        content.addView(TextView(this).apply {
            text = "LLM → Prolog → Wear complication"
            textSize = 24f
        })
        content.addView(TextView(this).apply {
            text = "The LLM only designs facts. Zara validates a facts-only template before saving it and syncing a typed payload to your watch. Generated Prolog is never executed as watch code."
            textSize = 14f
            setPadding(0, dp(8), 0, dp(16))
        })

        content.addView(label("Describe the complication"))
        requestField = EditText(this).apply {
            hint = "Example: a short text tile that says NODE OK with title StarIntel"
            minLines = 3
            maxLines = 6
            inputType = InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_FLAG_MULTI_LINE
        }
        content.addView(requestField, matchWidth())

        generateButton = Button(this).apply {
            text = "Generate Prolog with selected LLM"
            setOnClickListener { generateWithLlm() }
        }
        content.addView(generateButton, matchWidth())

        content.addView(label("Prolog template"))
        sourceField = EditText(this).apply {
            setText(PrologComplicationTemplateCompiler.example())
            minLines = 8
            maxLines = 18
            inputType = InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_FLAG_MULTI_LINE
            typeface = android.graphics.Typeface.MONOSPACE
        }
        content.addView(sourceField, matchWidth())

        val validateButton = Button(this).apply {
            text = "Validate"
            setOnClickListener { validateOnly() }
        }
        content.addView(validateButton, matchWidth())

        pushButton = Button(this).apply {
            text = "Save + copy active template to watch"
            setOnClickListener { saveAndPush() }
        }
        content.addView(pushButton, matchWidth())

        statusText = TextView(this).apply {
            text = "Ready. Cloud provider settings control which LLM generates templates."
            textSize = 13f
            setPadding(0, dp(14), 0, 0)
        }
        content.addView(statusText)

        return ScrollView(this).apply { addView(content) }
    }

    private fun generateWithLlm() {
        val request = requestField.text.toString().trim()
        if (request.isEmpty()) {
            statusText.text = "Describe the complication first."
            return
        }
        val provider = runCatching { cloudModel.reloadConfig() }.getOrElse { error ->
            statusText.text = error.message ?: "Could not load model provider settings."
            return
        }
        if (!provider.config.enabled) {
            statusText.text = "Cloud model is disabled. Configure Model Providers first."
            return
        }
        val identity = PrologProjectIdentityResolver.resolve(workspace.listSources())
        val prompt = PrologComplicationTemplateCompiler.llmPrompt(request)
        generateButton.isEnabled = false
        pushButton.isEnabled = false
        statusText.text = "Generating a facts-only Prolog template…"
        cloudModel.generate(
            prompt = prompt,
            purpose = CloudModelPurpose.CODING,
            effectiveAppName = identity.effectiveLlmAppName(provider.config.appName),
        ).whenComplete { result, error ->
            runOnUiThread {
                generateButton.isEnabled = true
                pushButton.isEnabled = true
                if (error != null) {
                    statusText.text = rootMessage(error)
                    return@runOnUiThread
                }
                val source = stripMarkdownFence(result?.text.orEmpty())
                val compiled = runCatching { PrologComplicationTemplateCompiler.compile(source) }
                compiled.onSuccess { template ->
                    sourceField.setText(source.trim() + "\n")
                    statusText.text = "Generated and validated ${template.id} (${template.type.atom}). Review it, then copy it to the watch."
                }.onFailure { compileError ->
                    sourceField.setText(source)
                    statusText.text = "LLM output failed validation: ${compileError.message ?: "invalid template"}. Edit it or regenerate."
                }
            }
        }
    }

    private fun validateOnly() {
        runCatching {
            PrologComplicationTemplateCompiler.compile(sourceField.text.toString())
        }.onSuccess { template ->
            statusText.text = "Valid ${template.type.atom} template: ${template.id}."
        }.onFailure { error ->
            statusText.text = error.message ?: "Invalid complication template."
        }
    }

    private fun saveAndPush() {
        val template = runCatching {
            PrologComplicationTemplateCompiler.compile(sourceField.text.toString())
        }.getOrElse { error ->
            statusText.text = error.message ?: "Invalid complication template."
            return
        }
        val sourceName = "complication_${template.id}.pl"
        val source = sourceField.text.toString().trimEnd() + "\n"
        runCatching { workspace.saveSource(sourceName, source) }.onFailure { error ->
            statusText.text = error.message ?: "Could not save complication source."
            return
        }
        pushButton.isEnabled = false
        statusText.text = "Saved $sourceName. Copying active template through Wear Data Layer…"
        transfer.pushActive(template)
            .addOnSuccessListener {
                transfer.connectedWatchCount()
                    .addOnSuccessListener { count ->
                        pushButton.isEnabled = true
                        statusText.text = if (count > 0) {
                            "Copied ${template.id} to Wear Data Layer for $count connected watch node(s)."
                        } else {
                            "Template queued in Wear Data Layer. No watch is connected right now; it will sync when the watch reconnects."
                        }
                    }
                    .addOnFailureListener {
                        pushButton.isEnabled = true
                        statusText.text = "Template copied to Wear Data Layer. Watch connection count is unavailable."
                    }
            }
            .addOnFailureListener { error ->
                pushButton.isEnabled = true
                statusText.text = error.message ?: "Could not copy template to the watch."
            }
    }

    private fun stripMarkdownFence(raw: String): String {
        val text = raw.trim()
        if (!text.startsWith("```")) return text
        val lines = text.lines().toMutableList()
        if (lines.isNotEmpty() && lines.first().trim().startsWith("```")) lines.removeAt(0)
        if (lines.isNotEmpty() && lines.last().trim() == "```") lines.removeAt(lines.lastIndex)
        return lines.joinToString("\n").trim()
    }

    private fun rootMessage(error: Throwable): String {
        var current = error
        while (current.cause != null && current.cause !== current) current = current.cause!!
        return current.message ?: current::class.java.simpleName
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
}
