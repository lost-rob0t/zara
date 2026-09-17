package ai.zara.app.watch

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class WatchInstallPolicyTest {
    @Test
    fun wearOsThreeAndNewerRejectBluetoothSideload() {
        assertFalse(WatchInstallPolicy.bluetoothSideloadSupported)
        assertEquals(WatchInstallTransport.WIFI_ADB, WatchInstallPolicy.installTransport)
        assertTrue(WatchInstallPolicy.transportNotice.contains("Wear OS 3+"))
        assertTrue(WatchInstallPolicy.transportNotice.contains("Bluetooth"))
        assertTrue(WatchInstallPolicy.transportNotice.contains("Wi-Fi"))
    }

    @Test
    fun pairingInputRequiresValidHostPortAndSixDigitCode() {
        assertEquals(
            WatchPairingInput("192.168.1.25", 37123, "123456"),
            WatchInstallInput.parsePairing(" 192.168.1.25 ", "37123", "12 34 56"),
        )
        assertNull(WatchInstallInput.parsePairing("", "37123", "123456"))
        assertNull(WatchInstallInput.parsePairing("192.168.1.25", "0", "123456"))
        assertNull(WatchInstallInput.parsePairing("192.168.1.25", "65536", "123456"))
        assertNull(WatchInstallInput.parsePairing("192.168.1.25", "37123", "12345"))
    }

    @Test
    fun connectionInputRequiresValidHostAndPort() {
        assertEquals(
            WatchConnectionInput("192.168.1.25", 41481),
            WatchInstallInput.parseConnection("192.168.1.25", "41481"),
        )
        assertNull(WatchInstallInput.parseConnection("", "41481"))
        assertNull(WatchInstallInput.parseConnection("192.168.1.25", "not-a-port"))
    }
}
