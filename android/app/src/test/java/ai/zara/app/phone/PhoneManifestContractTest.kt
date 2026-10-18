package ai.zara.app.phone

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class PhoneManifestContractTest {
    @Test
    fun phoneBridgeDeclaresSmsAndTelecomSurfaces() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("android.permission.SEND_SMS"))
        assertTrue(manifest.contains("android.permission.RECEIVE_SMS"))
        assertTrue(manifest.contains(".phone.SmsReceivedReceiver"))
        assertTrue(manifest.contains(".phone.ZaraCallScreeningService"))
        assertTrue(manifest.contains(".phone.ZaraInCallService"))
        assertTrue(manifest.contains("android.telecom.CallScreeningService"))
        assertTrue(manifest.contains("android.telecom.InCallService"))
    }
}
