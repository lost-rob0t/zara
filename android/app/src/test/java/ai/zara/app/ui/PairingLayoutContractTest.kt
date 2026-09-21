package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PairingLayoutContractTest {
    @Test
    fun pairingSetupActionReservesLayoutSpaceInsteadOfCoveringRouteTabs() {
        val source = File("src/main/java/ai/zara/app/ui/PairingZaraApp.kt").readText()
        val shell = source.substringBefore("@Composable\ninternal fun PairingSetupAction")

        assertTrue(shell.contains("Column(Modifier.fillMaxSize())"))
        assertTrue(shell.contains("Box(Modifier.weight(1f).fillMaxWidth())"))
        assertTrue(shell.contains(".windowInsetsPadding(WindowInsets.navigationBars)"))
        assertFalse(shell.contains(".align(Alignment.TopEnd)"))
        assertFalse(shell.contains("WindowInsets.statusBars"))
    }
}
