package ai.zara.org.home

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgAppWiringContractTest {
    private val source = File("src/main/java/ai/zara/org/home/MainActivity.kt").readText()
    private val manifest = File("src/main/AndroidManifest.xml").readText()
    private val gradle = File("build.gradle.kts").readText()

    @Test fun `installs standalone with its own application identity`() {
        assertTrue(gradle.contains("applicationId = \"ai.zara.org.home\""))
        assertTrue(manifest.contains("android.intent.action.MAIN"))
        assertTrue(manifest.contains("android.intent.category.LAUNCHER"))
    }

    @Test fun `renders only its focused surface over the shared org stack`() {
        assertTrue(source.contains("OrgTheme"))
        assertTrue(source.contains("OrgWorkspaceScreen("))
        assertTrue(source.contains("HomeSurface(model)"))
    }

    @Test fun `stays independent of the flagship and the Zara phone app`() {
        assertFalse(source.contains("ai.zara.org.app"))
        assertFalse(gradle.contains("project(\":app\")"))
        assertFalse(gradle.contains("project(\":org-app\")"))
    }

    @Test fun `keeps the optional shared workspace queryable`() {
        assertTrue(manifest.contains("ai.zara.org.permission.ORG_HOME"))
        assertTrue(manifest.contains("ai.zara.org.sync.home"))
    }

    @Test fun `never forks canonical storage or parser authority`() {
        assertFalse(source.contains("Room.databaseBuilder"))
        assertFalse(source.contains("SQLiteDatabase"))
        assertFalse(source.contains("OrgParser.parse("))
    }
}
