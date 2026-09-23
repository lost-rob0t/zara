package ai.zara.app.widget

import android.app.Activity
import android.appwidget.AppWidgetManager
import android.content.ComponentName
import android.os.Bundle
import android.widget.TextView
import java.io.File

class WidgetEvidenceActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        when (intent.getStringExtra(EXTRA_COMMAND)) {
            COMMAND_PIN -> requestWidgetPin(intent.getStringExtra(EXTRA_KIND))
            COMMAND_SNAPSHOT -> projectSnapshot(intent.getStringExtra(EXTRA_STATE))
            else -> failClosed("UNKNOWN_COMMAND")
        }
    }

    private fun requestWidgetPin(kind: String?) {
        val provider = when (kind) {
            "assistant" -> ZaraAssistantWidgetProvider::class.java
            "runtime" -> ZaraRuntimeWidgetProvider::class.java
            "actions" -> ZaraActionsWidgetProvider::class.java
            else -> return failClosed("UNKNOWN_WIDGET_KIND")
        }
        val manager = AppWidgetManager.getInstance(this)
        if (!manager.isRequestPinAppWidgetSupported) {
            failClosed("PIN_UNSUPPORTED")
            return
        }
        if (!manager.requestPinAppWidget(ComponentName(this, provider), null, null)) {
            failClosed("PIN_REJECTED")
            return
        }
        finish()
    }

    private fun projectSnapshot(state: String?) {
        val file = File(noBackupFilesDir, "zara/widget-runtime.bin")
        val store = WidgetRuntimeSnapshotStore(file)
        val now = System.currentTimeMillis()
        when (state) {
            "fresh" -> store.save(
                WidgetRuntimeSnapshot(
                    remote = "CONNECTED",
                    local = "LOCAL READY",
                    mode = "AUTO",
                    capturedAtEpochMillis = now,
                ),
            )
            "stale" -> store.save(
                WidgetRuntimeSnapshot(
                    remote = "CONNECTED",
                    local = "LOCAL READY",
                    mode = "AUTO",
                    capturedAtEpochMillis = now - WidgetRuntimeSnapshotStore.DEFAULT_FRESHNESS_MILLIS - 1,
                ),
            )
            "corrupt" -> {
                check(file.parentFile?.exists() == true || file.parentFile?.mkdirs() == true)
                file.writeText("CORRUPT\n")
            }
            else -> {
                failClosed("UNKNOWN_RUNTIME_STATE")
                return
            }
        }
        ZaraWidgetUpdater.refreshAll(this)
        finish()
    }

    private fun failClosed(message: String) {
        setContentView(TextView(this).apply { text = message })
    }

    companion object {
        const val EXTRA_COMMAND = "command"
        const val EXTRA_KIND = "kind"
        const val EXTRA_STATE = "state"
        private const val COMMAND_PIN = "pin"
        private const val COMMAND_SNAPSHOT = "snapshot"
    }
}
