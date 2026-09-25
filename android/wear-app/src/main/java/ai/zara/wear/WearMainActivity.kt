package ai.zara.wear

import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import android.content.ActivityNotFoundException
import android.content.Intent
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
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.wear.compose.material3.Button
import androidx.wear.compose.material3.MaterialTheme
import androidx.wear.compose.material3.Text

class WearMainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val runtime = WearCompanionRuntime.get(applicationContext)
        runtime.start()
        setContent {
            ZaraWearClient(runtime)
        }
    }
}

@Composable
internal fun ZaraWearClient(runtime: WearCompanionRuntime) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    var linkState by remember { mutableStateOf<WearCompanionLinkState>(runtime.state()) }

    DisposableEffect(runtime) {
        runtime.observe { state -> linkState = state }
        onDispose { runtime.observe(null) }
    }

    val presentation = WearClientPresentation.from(linkState)

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
                    .padding(horizontal = 22.dp, vertical = 18.dp),
                horizontalAlignment = Alignment.CenterHorizontally,
                verticalArrangement = Arrangement.Center,
            ) {
                ZaraWearSigil(46.dp)
                Spacer(Modifier.size(8.dp))
                Text(
                    text = presentation.statusLabel,
                    color = if (presentation.linkLive) tokens.accentCyan else tokens.textMuted,
                    fontFamily = FontFamily.Monospace,
                    fontWeight = FontWeight.SemiBold,
                    fontSize = 11.sp,
                    letterSpacing = 1.6.sp,
                    textAlign = TextAlign.Center,
                )
                presentation.phoneName?.let { phone ->
                    Text(
                        text = phone,
                        modifier = Modifier.padding(top = 4.dp),
                        color = tokens.text,
                        fontFamily = FontFamily.Monospace,
                        fontSize = 10.sp,
                        letterSpacing = 1.sp,
                        textAlign = TextAlign.Center,
                    )
                }
                presentation.conversationAct?.let { act ->
                    Spacer(Modifier.size(12.dp))
                    ConversationCard(presentation, act)
                }
                presentation.rejectionNotice?.let { notice ->
                    Text(
                        text = notice,
                        modifier = Modifier.padding(top = 8.dp),
                        color = tokens.textMuted,
                        fontFamily = FontFamily.Monospace,
                        fontSize = 9.sp,
                        textAlign = TextAlign.Center,
                    )
                }
                if (presentation.statusLabel == "SEARCHING FOR PHONE") {
                    Text(
                        text = "Open Zara on your paired phone — pairing continues automatically.",
                        modifier = Modifier.padding(top = 12.dp),
                        color = tokens.textMuted,
                        fontSize = 10.sp,
                        textAlign = TextAlign.Center,
                    )
                }
                Spacer(Modifier.size(14.dp))
                VoiceLaunchButton()
            }
        }
    }
}

@Composable
private fun ConversationCard(presentation: WearClientPresentation, act: String) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)

    Column(horizontalAlignment = Alignment.CenterHorizontally) {
        Text(
            text = act,
            color = tokens.accentMagenta,
            fontFamily = FontFamily.Monospace,
            fontWeight = FontWeight.SemiBold,
            fontSize = 12.sp,
            letterSpacing = 1.2.sp,
            textAlign = TextAlign.Center,
        )
        presentation.discourseEntities.take(3).forEach { entity ->
            Text(
                text = entity,
                modifier = Modifier.padding(top = 4.dp),
                color = tokens.text,
                fontFamily = FontFamily.Monospace,
                fontSize = 9.sp,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis,
                textAlign = TextAlign.Center,
            )
        }
        presentation.unresolvedQuestions.take(2).forEach { question ->
            Text(
                text = "? $question",
                modifier = Modifier.padding(top = 4.dp),
                color = tokens.accentCyan,
                fontFamily = FontFamily.Monospace,
                fontSize = 9.sp,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis,
                textAlign = TextAlign.Center,
            )
        }
        if (presentation.verifiedOutcomeCount > 0) {
            Text(
                text = "✓ ${presentation.verifiedOutcomeCount} verified",
                modifier = Modifier.padding(top = 4.dp),
                color = tokens.textMuted,
                fontFamily = FontFamily.Monospace,
                fontSize = 9.sp,
            )
        }
        presentation.generationLabel?.let { label ->
            Text(
                text = label,
                modifier = Modifier.padding(top = 6.dp),
                color = tokens.textMuted,
                fontFamily = FontFamily.Monospace,
                fontSize = 8.sp,
                letterSpacing = 0.8.sp,
            )
        }
    }
}

@Composable
private fun VoiceLaunchButton() {
    val context = LocalContext.current
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    var missing by remember { mutableStateOf(false) }

    Button(
        onClick = {
            try {
                context.startActivity(Intent("ai.zara.action.WEAR_VOICE"))
            } catch (_: ActivityNotFoundException) {
                missing = true
            }
        },
    ) {
        Text(
            text = if (missing) "VOICE NOT INSTALLED" else "ZARA VOICE",
            color = tokens.accentCyan,
            fontFamily = FontFamily.Monospace,
            fontSize = 10.sp,
            letterSpacing = 1.2.sp,
        )
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
