package ai.zara.wear

import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.wear.compose.material3.MaterialTheme
import androidx.wear.compose.material3.Text

class WearMainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            ZaraWearClient()
        }
    }
}

@Composable
internal fun ZaraWearClient() {
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
                    .verticalScroll(rememberScrollState())
                    .padding(horizontal = 28.dp, vertical = 22.dp),
                horizontalAlignment = Alignment.CenterHorizontally,
                verticalArrangement = Arrangement.Center,
            ) {
                ZaraWearSigil(74.dp)
                Spacer(Modifier.size(14.dp))
                Text(
                    text = "ZARA",
                    color = tokens.text,
                    fontFamily = FontFamily.Monospace,
                    fontWeight = FontWeight.SemiBold,
                    fontSize = 18.sp,
                    letterSpacing = 2.4.sp,
                )
                Text(
                    text = "SYMBOLIC WATCH CLIENT",
                    modifier = Modifier.padding(top = 4.dp),
                    color = tokens.textMuted,
                    fontFamily = FontFamily.Monospace,
                    fontSize = 9.sp,
                    letterSpacing = 1.4.sp,
                    textAlign = TextAlign.Center,
                )
                Text(
                    text = "STANDALONE • NOT ENROLLED",
                    modifier = Modifier.padding(top = 18.dp),
                    color = tokens.accentCyan,
                    fontFamily = FontFamily.Monospace,
                    fontSize = 9.sp,
                    letterSpacing = 1.1.sp,
                    textAlign = TextAlign.Center,
                )
                Text(
                    text = "Shared Zara transport and enrollment wiring follows in #866. Voice launch is owned by #867.",
                    modifier = Modifier.padding(top = 10.dp),
                    color = tokens.textMuted,
                    fontSize = 11.sp,
                    textAlign = TextAlign.Center,
                )
            }
        }
    }
}

@Composable
private fun ZaraWearSigil(size: Dp) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    Canvas(Modifier.size(size)) {
        val center = Offset(this.size.width / 2f, this.size.height / 2f)
        val stroke = this.size.minDimension * 0.032f
        val radius = this.size.minDimension * 0.34f
        drawCircle(
            color = tokens.borderActive,
            radius = radius,
            center = center,
            style = Stroke(width = stroke),
        )
        drawLine(
            color = tokens.accentMagenta,
            start = Offset(center.x, center.y - radius * 1.35f),
            end = Offset(center.x, center.y + radius * 1.35f),
            strokeWidth = stroke,
        )
        drawLine(
            color = tokens.accentCyan,
            start = Offset(center.x - radius, center.y),
            end = Offset(center.x + radius, center.y),
            strokeWidth = stroke * 0.65f,
        )
        drawCircle(
            color = tokens.accentMagenta,
            radius = stroke * 1.5f,
            center = Offset(center.x, center.y - radius * 1.35f),
        )
        drawCircle(
            color = tokens.accentCyan,
            radius = stroke * 1.5f,
            center = Offset(center.x, center.y + radius * 1.35f),
        )
    }
}
