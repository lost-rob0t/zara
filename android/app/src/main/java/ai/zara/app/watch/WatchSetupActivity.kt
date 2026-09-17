package ai.zara.app.watch

import ai.zara.app.ui.WatchSetupSurface
import android.Manifest
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue

class WatchSetupActivity : ComponentActivity() {
    private lateinit var controller: WatchSetupController
    private var state by mutableStateOf(WatchSetupState())

    private val nearbyPermissionLauncher = registerForActivityResult(
        ActivityResultContracts.RequestMultiplePermissions(),
    ) { grants ->
        if (::controller.isInitialized && grants[Manifest.permission.BLUETOOTH_CONNECT] != false) {
            controller.scan()
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        controller = WatchSetupController(this)
        state = controller.state()
        controller.setObserver { next -> runOnUiThread { state = next } }

        setContent {
            WatchSetupSurface(
                state = state,
                onScan = ::scanWithNearbyPermission,
                onHostChanged = controller::setHost,
                onPairPortChanged = controller::setPairPort,
                onConnectPortChanged = controller::setConnectPort,
                onPairCodeChanged = controller::setPairCode,
                onUseEndpoint = controller::useEndpoint,
                onPair = controller::pair,
                onConnect = controller::connect,
                onInstall = controller::installLatest,
                onDisconnect = controller::disconnect,
            )
        }
    }

    private fun scanWithNearbyPermission() {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.S) {
            controller.scan()
            return
        }
        val permissions = arrayOf(
            Manifest.permission.BLUETOOTH_CONNECT,
            Manifest.permission.BLUETOOTH_SCAN,
        )
        val missing = permissions.filter {
            checkSelfPermission(it) != PackageManager.PERMISSION_GRANTED
        }
        if (missing.isEmpty()) {
            controller.scan()
        } else {
            nearbyPermissionLauncher.launch(missing.toTypedArray())
        }
    }

    override fun onDestroy() {
        if (::controller.isInitialized) controller.setObserver(null)
        super.onDestroy()
    }
}
