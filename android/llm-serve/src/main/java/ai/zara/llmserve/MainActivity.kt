package ai.zara.llmserve

import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelQuantization
import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.ViewGroup
import android.widget.ArrayAdapter
import android.widget.Button
import android.widget.EditText
import android.widget.LinearLayout
import android.widget.Spinner
import android.widget.TextView

class MainActivity : Activity() {
    private lateinit var statusView: TextView
    private lateinit var modelId: EditText
    private lateinit var modelVersion: EditText
    private lateinit var contextTokens: EditText
    private lateinit var quantization: Spinner
    private lateinit var backend: Spinner

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        render()
    }

    override fun onResume() {
        super.onResume()
        refreshStatus()
    }

    override fun onActivityResult(
        requestCode: Int,
        resultCode: Int,
        data: Intent?,
    ) {
        super.onActivityResult(requestCode, resultCode, data)
        if (requestCode != REQUEST_MODEL || resultCode != RESULT_OK) return
        val uri = data?.data ?: return
        val quant = quantization.selectedItem?.toString().orEmpty()
        if (quant == SELECT_QUANTIZATION) {
            statusView.text = "Choose an explicit quantization before importing."
            return
        }

        val id = modelId.text.toString().trim()
        val version = modelVersion.text.toString().trim()
        val context = contextTokens.text.toString().toIntOrNull()
        if (id.isEmpty() || version.isEmpty() || context == null) {
            statusView.text = "Model id, version, and context tokens are required."
            return
        }

        val serviceIntent = Intent(this, LlmServeService::class.java).apply {
            action = LlmServeService.ACTION_IMPORT_MODEL
            setData(uri)
            addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
            putExtra(LlmServeService.EXTRA_MODEL_ID, id)
            putExtra(LlmServeService.EXTRA_MODEL_VERSION, version)
            putExtra(LlmServeService.EXTRA_QUANTIZATION, quant)
            putExtra(
                LlmServeService.EXTRA_BACKEND,
                backend.selectedItem.toString(),
            )
            putExtra(LlmServeService.EXTRA_CONTEXT_TOKENS, context)
        }
        startForegroundService(serviceIntent)
        statusView.text = "Importing and verifying model…"
    }

    private fun render() {
        val layout = LinearLayout(this).apply {
            orientation = LinearLayout.VERTICAL
            setPadding(48, 48, 48, 48)
        }

        layout.addView(
            TextView(this).apply {
                textSize = 24f
                text = "Zara LLM Serve"
            }
        )
        layout.addView(
            TextView(this).apply {
                text =
                    "Ollama-compatible local endpoint: " +
                        "http://127.0.0.1:11434"
                setPadding(0, 16, 0, 16)
            }
        )

        statusView = TextView(this).apply {
            textSize = 16f
            setPadding(0, 8, 0, 24)
        }
        layout.addView(statusView)

        modelId = EditText(this).apply {
            hint = "Model id"
            setText("zara-local")
        }
        modelVersion = EditText(this).apply {
            hint = "Model version"
            setText("1")
        }
        contextTokens = EditText(this).apply {
            hint = "Max context tokens"
            inputType = android.text.InputType.TYPE_CLASS_NUMBER
            setText("4096")
        }
        layout.addView(modelId)
        layout.addView(modelVersion)
        layout.addView(contextTokens)

        quantization = Spinner(this).apply {
            val values =
                listOf(SELECT_QUANTIZATION) +
                    LocalModelQuantization.entries.map { item -> item.wireName }
            adapter = ArrayAdapter(
                this@MainActivity,
                android.R.layout.simple_spinner_dropdown_item,
                values,
            )
        }
        backend = Spinner(this).apply {
            adapter = ArrayAdapter(
                this@MainActivity,
                android.R.layout.simple_spinner_dropdown_item,
                LocalModelBackend.entries.map { item -> item.name },
            )
        }
        layout.addView(quantization)
        layout.addView(backend)

        layout.addView(
            Button(this).apply {
                text = "Import .litertlm model"
                setOnClickListener {
                    startActivityForResult(
                        Intent(Intent.ACTION_OPEN_DOCUMENT).apply {
                            addCategory(Intent.CATEGORY_OPENABLE)
                            type = "application/octet-stream"
                        },
                        REQUEST_MODEL,
                    )
                }
            }
        )

        layout.addView(
            Button(this).apply {
                text = "Start server"
                setOnClickListener {
                    startForegroundService(
                        Intent(
                            this@MainActivity,
                            LlmServeService::class.java,
                        ).apply {
                            action = LlmServeService.ACTION_START
                        }
                    )
                    statusView.text = "Starting…"
                }
            }
        )

        layout.addView(
            Button(this).apply {
                text = "Stop server"
                setOnClickListener {
                    startService(
                        Intent(
                            this@MainActivity,
                            LlmServeService::class.java,
                        ).apply {
                            action = LlmServeService.ACTION_STOP
                        }
                    )
                }
            }
        )

        layout.addView(
            Button(this).apply {
                text = "Refresh status"
                setOnClickListener { refreshStatus() }
            }
        )

        setContentView(
            layout,
            ViewGroup.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT,
            ),
        )
        refreshStatus()
    }

    private fun refreshStatus() {
        if (!::statusView.isInitialized) return
        val status =
            getSharedPreferences(LlmServeService.PREFS, MODE_PRIVATE)
                .getString(LlmServeService.KEY_STATUS, "stopped")
        statusView.text = status
    }

    companion object {
        private const val REQUEST_MODEL = 11434
        private const val SELECT_QUANTIZATION = "Select quantization"
    }
}
