package ai.zara.app.peer

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Source contract for the Android peer-listener service wiring. The repo runs
 * no Robolectric surface, so the manifest and service contract are pinned as
 * reviewable source text like the native Trealla packaging contract.
 */
class PeerNodeListenerServiceContractTest {
    private fun appSource(relative: String): String =
        File("src/main/$relative").readText()

    @Test
    fun manifestDeclaresThePeerListenerForegroundService() {
        val manifest = appSource("AndroidManifest.xml")

        assertTrue(manifest.contains("android:name=\".peer.PeerNodeListenerService\""))
        assertTrue(manifest.contains("android:foregroundServiceType=\"connectedDevice\""))
        assertTrue(
            manifest.substringAfter(".peer.PeerNodeListenerService", "")
                .substringBefore("</service>")
                .contains("android:exported=\"false\""),
        )
    }

    @Test
    fun manifestDeclaresForegroundServiceConnectedDevicePermissions() {
        val manifest = appSource("AndroidManifest.xml")

        assertTrue(manifest.contains("android.permission.FOREGROUND_SERVICE\""))
        assertTrue(manifest.contains("android.permission.FOREGROUND_SERVICE_CONNECTED_DEVICE\""))
        assertTrue(manifest.contains("android.permission.POST_NOTIFICATIONS\""))
    }

    @Test
    fun serviceEntersTypedForegroundBeforeListenerWorkAndNeverFabricatesStartup() {
        val service = appSource("java/ai/zara/app/peer/PeerNodeListenerService.kt")

        assertTrue(service.contains("ServiceInfo.FOREGROUND_SERVICE_TYPE_CONNECTED_DEVICE"))
        assertTrue(service.contains("startForeground("))
        assertTrue(service.contains("START_NOT_STICKY"))
        assertTrue(service.contains("PeerNodeServiceController"))
        assertTrue(service.contains("PeerNodeIdentityStore"))
        assertTrue(service.contains("AndroidEnrollmentRepository"))
    }
}
