package ai.zara.store

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme {
                ZaraStoreScreen()
            }
        }
    }
}

@Composable
private fun ZaraStoreScreen() {
    Surface(modifier = Modifier.fillMaxSize()) {
        Column(
            modifier = Modifier.padding(24.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                text = "Zara Store",
                style = MaterialTheme.typography.headlineMedium,
                fontWeight = FontWeight.Bold,
            )
            Text(
                text = "F-Droid compatible package and plugin distribution",
                style = MaterialTheme.typography.titleMedium,
            )
            Text(
                text = "Foundation mode: signed repository metadata and Zara package identity are wired first. APK installation remains disabled until the Android installer authority slice lands.",
                style = MaterialTheme.typography.bodyLarge,
            )
            Text(
                text = "Source ${BuildConfig.SOURCE_SHA.take(12)}",
                style = MaterialTheme.typography.labelMedium,
            )
        }
    }
}
