package ai.zara.app.prolog.ipc

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PrologIpcContractTest {
    @Test
    fun exportedServiceIsSignatureProtectedAndReusesCanonicalSession() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val service = File("src/main/java/ai/zara/app/prolog/ipc/PrologIpcService.kt").readText()
        val settings = File("../settings.gradle.kts").readText()

        assertTrue(settings.contains("include(\":prolog-ipc\")"))
        assertTrue(manifest.contains("ai.zara.permission.PROLOG"))
        assertTrue(manifest.contains("android:protectionLevel=\"signature\""))
        assertTrue(manifest.contains(".prolog.ipc.PrologIpcService"))
        assertTrue(manifest.contains("android:exported=\"true\""))
        assertTrue(manifest.contains("android:permission=\"ai.zara.permission.PROLOG\""))
        assertTrue(service.contains("(application as ZaraApplication).appSession"))
        assertTrue(service.contains("queryLocalProlog"))
        assertFalse(service.contains("LocalZaraServer("))
        assertFalse(service.contains("NativeTreallaBridge("))
    }
}
