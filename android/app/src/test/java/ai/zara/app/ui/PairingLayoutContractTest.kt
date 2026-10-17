package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PairingLayoutContractTest {
    @Test
    fun pairingActionLivesInConnectionInsteadOfTakingSpaceFromEveryScreen() {
        val shell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val connection = shell.substringAfter("AppRoute.Connection -> {")
            .substringBefore("AppRoute.Updates -> {")

        assertFalse(shell.contains("PairingSetupAction("))
        assertFalse(File("src/main/java/ai/zara/app/ui/PairingZaraApp.kt").exists())
        assertTrue(connection.contains("Scan pairing QR"))
        assertTrue(connection.contains("state.enrollment != EnrollmentReadiness.Ready"))
    }
}
