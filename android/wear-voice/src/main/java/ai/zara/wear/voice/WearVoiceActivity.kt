package ai.zara.wear.voice

import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import android.Manifest
import android.content.pm.PackageManager
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.core.content.ContextCompat
import androidx.wear.compose.material3.MaterialTheme
import androidx.wear.compose.material3.Text

private enum class VoiceBootstrapState {
    PermissionRequired,
    SharedRuntimePending,
}

class WearVoiceActivity : ComponentActivity() {
    private var state by mutableStateOf(VoiceBootstrapState.PermissionRequired)

    private val requestMicrophone = registerForActivityResult(
        ActivityResultContracts.RequestPermission()
    ) { granted ->
        state = if (granted) {
            VoiceBootstrapState.SharedRuntimePending
        } else {
            VoiceBootstrapState.PermissionRequired
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        state = if (hasMicrophonePermission()) {
            VoiceBootstrapState.SharedRuntimePending
        } else {
            VoiceBootstrapState.PermissionRequired
        }

        setContent {
            ZaraVoiceSurface(state)
        }

        if (!hasMicrophonePermission()) {
            requestMicrophone.launch(Manifest.permission.RECORD_AUDIO)
        }
    }

    private fun hasMicrophonePermission(): Boolean =
        ContextCompat.checkSelfPermission(this, Manifest.permission.RECORD_AUDIO) ==
            PackageManager.PERMISSION_GRANTED
}

@Composable
private fun ZaraVoiceSurface(state: VoiceBootstrapState) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)

    MaterialTheme {
        Box(
            modifier = Modifier
                .fillMaxSize()
                .background(tokens.background),
            contentAlignment = Alignment.Center,
        ) {
            Column(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(horizontal = 30.dp, vertical = 24.dp),
                horizontalAlignment = Alignment.CenterHorizontally,
                verticalArrangement = Arrangement.Center,
            ) {
                VoiceSigil()
                Spacer(Modifier.size(16.dp))
                Text(
                    text = "ZARA VOICE",
                    color = tokens.text,
                    fontFamily = FontFamily.Monospace,
                    fontWeight = FontWeight.SemiBold,
                    fontSize = 17.sp,
                    letterSpacing = 2.1.sp,
                )
                Text(
                    text = when (state) {
                        VoiceBootstrapState.PermissionRequired -> "MICROPHONE PERMISSION REQUIRED"
                        VoiceBootstrapState.SharedRuntimePending -> "VOICE RUNTIME NOT WIRED YET"
                    },
                    modifier = Modifier.padding(top = 14.dp),
                    color = when (state) {
                        VoiceBootstrapState.PermissionRequired -> tokens.warning
                        VoiceBootstrapState.SharedRuntimePending -> tokens.accentCyan
                    },
                    fontFamily = FontFamily.Monospace,
                    fontSize = 9.sp,
                    letterSpacing = 1.1.sp,
                    textAlign = TextAlign.Center,
                )
                Text(
                    text = when (state) {
                        VoiceBootstrapState.PermissionRequired ->
                            "Grant microphone access once. Zara Voice will never claim listening until capture owns the mic."
                        VoiceBootstrapState.SharedRuntimePending ->
                            "This app is now the dedicated instant-launch target. Authenticated capture/playback will bind to the shared #866 runtime instead of copying the phone voice stack."
                    },
                    modifier = Modifier.padding(top = 10.dp),
                    color = tokens.textMuted,
                    fontSize = 10.sp,
                    textAlign = TextAlign.Center,
                )
            }
        }
    }
}

@Composable
private fun VoiceSigil() {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    Canvas(Modifier.size(82.dp)) {
        val center = Offset(size.width / 2f, size.height / 2f)
        val radius = size.minDimension * 0.34f
        val stroke = size.minDimension * 0.03f
        drawCircle(
            color = tokens.borderActive,
            radius = radius,
            center = center,
            style = Stroke(width = stroke),
        )
        drawCircle(
            color = tokens.accentMagenta,
            radius = radius * 0.36f,
            center = center,
            style = Stroke(width = stroke * 1.2f),
        )
        drawLine(
            color = tokens.accentCyan,
            start = Offset(center.x, center.y - radius * 0.62f),
            end = Offset(center.x, center.y + radius * 0.62f),
            strokeWidth = stroke,
        )
    }
}
