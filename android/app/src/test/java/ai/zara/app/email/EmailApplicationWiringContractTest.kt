package ai.zara.app.email

import org.junit.Assert.assertTrue
import org.junit.Test
import java.io.File

class EmailApplicationWiringContractTest {
    @Test
    fun emailPrologInstallsBeforeAndroidSessionStarts() {
        val source = projectFile("app/src/main/java/ai/zara/app/ZaraApplication.kt").readText()
        val install = source.indexOf("EmailPrologPlugin.install")
        val session = source.indexOf("AndroidAppSession(this)")

        assertTrue("email Prolog installer must be wired", install >= 0)
        assertTrue("email Prolog API must load before local server startup", install < session)
    }

    @Test
    fun canonicalEmailRulesAreStagedIntoAndroidAssets() {
        val build = projectFile("app/build.gradle.kts").readText()
        assertTrue(build.contains("../../kb/email_rules.pl"))
        assertTrue(build.contains("prolog/shared/kb"))
    }

    private fun projectFile(relative: String): File =
        File(System.getProperty("user.dir")).let { root ->
            if (root.name == "android") File(root, relative) else File(root, "android/$relative")
        }
}
