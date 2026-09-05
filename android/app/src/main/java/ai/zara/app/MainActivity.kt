package ai.zara.app

import ai.zara.app.auth.AndroidEnrollmentRepository
import ai.zara.app.runtime.ClientStateStore
import ai.zara.app.runtime.RuntimeEvent
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.reduce
import ai.zara.app.runtime.toRuntimeReadiness
import ai.zara.app.ui.ZaraApp
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import java.io.File

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        val restored = ClientStateStore(File(noBackupFilesDir, "zara/client-state.bin")).load()
        var runtimeState = restored?.let(RuntimeState::fromRestored) ?: RuntimeState.initial()
        val enrollment = AndroidEnrollmentRepository.create(this).state().toRuntimeReadiness()
        runtimeState = reduce(runtimeState, RuntimeEvent.EnrollmentObserved(enrollment))

        setContent {
            ZaraApp(
                runtimeState = runtimeState,
                sourceSha = BuildConfig.SOURCE_SHA,
            )
        }
    }
}
