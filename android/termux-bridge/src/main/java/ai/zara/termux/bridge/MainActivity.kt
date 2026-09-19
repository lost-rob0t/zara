package ai.zara.termux.bridge

import android.Manifest
import android.app.Activity
import android.content.Intent
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Bundle
import android.provider.Settings
import android.view.ViewGroup
import android.widget.Button
import android.widget.LinearLayout
import android.widget.TextView

class MainActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        render()
    }

    override fun onResume() {
        super.onResume()
        render()
    }

    private fun render() {
        val state = TermuxBridgeStateMachine.resolve(
            termuxInstalled = isTermuxInstalled(),
            runCommandPermissionGranted =
                checkSelfPermission(TERMUX_RUN_COMMAND_PERMISSION) == PackageManager.PERMISSION_GRANTED,
            probeAttempted = false,
            probeSucceeded = false,
        )

        val layout = LinearLayout(this).apply {
            orientation = LinearLayout.VERTICAL
            setPadding(48, 48, 48, 48)
        }

        layout.addView(TextView(this).apply {
            textSize = 24f
            text = "Zara Termux Bridge"
        })
        layout.addView(TextView(this).apply {
            textSize = 16f
            text = state.message()
            setPadding(0, 24, 0, 24)
        })

        layout.addView(Button(this).apply {
            text = "Open bridge permissions"
            setOnClickListener {
                startActivity(
                    Intent(
                        Settings.ACTION_APPLICATION_DETAILS_SETTINGS,
                        Uri.parse("package:$packageName"),
                    ),
                )
            }
        })

        layout.addView(Button(this).apply {
            text = "Open Termux"
            isEnabled = state.termuxInstalled
            setOnClickListener {
                packageManager.getLaunchIntentForPackage(TERMUX_PACKAGE)?.let(::startActivity)
            }
        })

        setContentView(
            layout,
            ViewGroup.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT,
            ),
        )
    }

    private fun isTermuxInstalled(): Boolean =
        try {
            packageManager.getPackageInfo(TERMUX_PACKAGE, 0)
            true
        } catch (_: PackageManager.NameNotFoundException) {
            false
        }

    private fun TermuxBridgeState.message(): String =
        when (phase) {
            TermuxBridgePhase.TERMUX_MISSING ->
                "Termux is not installed. Install a supported Termux build before enabling the Linux worker runtime."
            TermuxBridgePhase.RUN_COMMAND_PERMISSION_REQUIRED ->
                "Grant Zara Termux Bridge the Run commands in Termux environment permission."
            TermuxBridgePhase.EXTERNAL_APPS_CHECK_REQUIRED ->
                "Permission granted. Zara still needs to probe Termux. Termux must have allow-external-apps=true."
            TermuxBridgePhase.EXTERNAL_APPS_OR_TERMUX_SETUP_REQUIRED ->
                "The Termux probe failed. Check allow-external-apps=true and the Zara worker bootstrap."
            TermuxBridgePhase.READY ->
                "Termux integration probe succeeded."
        }

    companion object {
        private const val TERMUX_PACKAGE = "com.termux"
        private const val TERMUX_RUN_COMMAND_PERMISSION = "com.termux.permission.RUN_COMMAND"
    }
}
