package ai.zara.app.smartthings

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class SmartThingsApplicationWiringContractTest {
    @Test
    fun applicationInstallsPrologSourceBeforeSessionAndExposesController() {
        val application = projectFile("app/src/main/java/ai/zara/app/ZaraApplication.kt").readText()

        val install = application.indexOf("SmartThingsPrologPlugin.install")
        val session = application.indexOf("AndroidAppSession(this)")
        assertTrue(install >= 0)
        assertTrue(session > install)
        assertTrue(application.contains("val smartThingsPlugin: SmartThingsAndroidPlugin"))
        assertTrue(application.contains("queryProlog = appSession::queryLocalProlog"))
    }

    private fun projectFile(path: String): File {
        val direct = File(path)
        if (direct.exists()) return direct
        return File("android", path)
    }
}
