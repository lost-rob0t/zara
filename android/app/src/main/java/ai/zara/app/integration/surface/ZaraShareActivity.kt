package ai.zara.app.integration.surface

import ai.zara.app.ZaraApplication
import ai.zara.app.ui.UiOperationFailure
import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.Gravity
import android.widget.LinearLayout
import android.widget.ProgressBar
import android.widget.TextView

class ZaraShareActivity : Activity() {
    private lateinit var status: TextView
    private lateinit var response: TextView

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val payload = sharedPayload(intent)
        if (payload.isBlank()) {
            finish()
            return
        }

        val density = resources.displayMetrics.density
        val padding = (20 * density).toInt()
        val root = LinearLayout(this).apply {
            orientation = LinearLayout.VERTICAL
            gravity = Gravity.CENTER_HORIZONTAL
            setPadding(padding, padding, padding, padding)
        }
        status = TextView(this).apply {
            text = "Asking Zara about shared content…"
            textSize = 18f
        }
        response = TextView(this).apply {
            textSize = 16f
            setPadding(0, padding, 0, 0)
        }
        root.addView(status)
        root.addView(ProgressBar(this))
        root.addView(response)
        setContentView(root)

        val prompt = "The user explicitly shared this Android content with Zara. Work with it directly:\n\n$payload"
        (application as ZaraApplication).appSession.submitText(prompt).whenComplete { result, error ->
            runOnUiThread {
                if (error != null) {
                    status.text = "Zara could not process the share"
                    response.text = UiOperationFailure.summarize(error)
                } else if (result != null) {
                    status.text = if (result.success) "Zara" else "Zara returned an error"
                    response.text = result.text
                }
            }
        }
    }

    private fun sharedPayload(intent: Intent): String {
        val values = linkedSetOf<String>()
        if (intent.action == Intent.ACTION_SEND || intent.action == Intent.ACTION_SEND_MULTIPLE) {
            intent.getStringExtra(Intent.EXTRA_TEXT)?.let(values::add)
            intent.getStringExtra(Intent.EXTRA_SUBJECT)?.let { subject ->
                values.add("Subject: $subject")
            }
            intent.clipData?.let { clip ->
                for (index in 0 until clip.itemCount.coerceAtMost(MAX_CLIP_ITEMS)) {
                    val item = clip.getItemAt(index)
                    item.text?.toString()?.let(values::add)
                    item.uri?.toString()?.let(values::add)
                    item.intent?.toUri(0)?.let(values::add)
                }
            }
            @Suppress("DEPRECATION")
            intent.getParcelableArrayListExtra<android.net.Uri>(Intent.EXTRA_STREAM)
                ?.take(MAX_CLIP_ITEMS)
                ?.forEach { values.add(it.toString()) }
        }
        intent.dataString?.let(values::add)
        return values.joinToString("\n").take(MAX_SHARE_CHARS)
    }

    private companion object {
        const val MAX_CLIP_ITEMS = 32
        const val MAX_SHARE_CHARS = 256 * 1024
    }
}
