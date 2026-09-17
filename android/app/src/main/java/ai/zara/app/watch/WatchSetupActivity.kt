package ai.zara.app.watch

import ai.zara.app.ui.WatchSetupSurface
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue

class WatchSetupActivity : ComponentActivity() {
    private lateinit var controller: WatchSetupController
    private var state by mutableStateOf(WatchSetupState())

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        controller = WatchSetupController(this)
        state = controller.state()
        controller.setObserver { next -> runOnUiThread { state = next } }

        setContent {
            WatchSetupSurface(
                state = state,
                onScan = controller::scan,
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

    override fun onDestroy() {
        if (::controller.isInitialized) controller.setObserver(null)
        super.onDestroy()
    }
}
