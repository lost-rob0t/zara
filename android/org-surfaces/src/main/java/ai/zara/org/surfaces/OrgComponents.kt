package ai.zara.org.surfaces

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ColumnScope
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp

@Composable
fun OrgPanel(
    modifier: Modifier = Modifier,
    active: Boolean = false,
    content: @Composable ColumnScope.() -> Unit,
) {
    val tokens = LocalOrgTokens.current
    Column(
        modifier = modifier
            .fillMaxWidth()
            .background(if (active) tokens.surfaceElevated else tokens.surface)
            .border(
                width = 1.dp,
                color = if (active) tokens.borderActive else tokens.border,
                shape = RoundedCornerShape(6.dp),
            )
            .padding(10.dp),
        content = content,
    )
}

@Composable
fun OrgStatusDot(
    color: Color,
    modifier: Modifier = Modifier,
) {
    Box(
        modifier = modifier
            .padding(top = 6.dp)
            .size(8.dp)
            .background(color, CircleShape),
    )
}

@Composable
fun OrgMutedText(text: String, modifier: Modifier = Modifier) {
    Text(
        text,
        modifier = modifier,
        color = MaterialTheme.colorScheme.onSurfaceVariant,
        style = MaterialTheme.typography.labelSmall,
    )
}
