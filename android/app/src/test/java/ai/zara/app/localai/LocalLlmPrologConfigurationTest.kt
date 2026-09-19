package ai.zara.app.localai

import ai.zara.app.prolog.PrologWorkspace
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class LocalLlmPrologConfigurationTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun `managed local llm facts preserve manual Prolog source`() {
        val workspace = PrologWorkspace(temporary.newFolder("workspace"))
        workspace.saveSource(
            "config.pl",
            "% operator source\nconfig(theme, outrun).\ncustom_rule(ok).\n",
        )
        val store = LocalLlmPrologConfigStore(workspace)
        val configuration = LocalLlmConfiguration(
            apiEnabled = true,
            background = true,
            apiPort = 11435,
            maxOutputTokens = 384,
        )

        val saved = store.save(configuration)

        assertTrue(saved.text.contains("% operator source"))
        assertTrue(saved.text.contains("custom_rule(ok)."))
        assertTrue(saved.text.contains("config(local_llm_api_enabled, true)."))
        assertTrue(saved.text.contains("config(local_llm_background, true)."))
        assertTrue(saved.text.contains("config(local_llm_api_port, 11435)."))
        assertTrue(saved.text.contains("config(local_llm_max_output_tokens, 384)."))
        assertEquals(configuration, store.readManaged())
        assertFalse(saved.text.contains("api_key"))
        assertFalse(saved.text.contains("bearer_secret"))
    }

    @Test
    fun `saving again replaces only the managed local llm block`() {
        val workspace = PrologWorkspace(temporary.newFolder("replace"))
        workspace.saveSource("config.pl", "manual_fact(kept).\n")
        val store = LocalLlmPrologConfigStore(workspace)

        store.save(LocalLlmConfiguration(apiEnabled = true, apiPort = 11435))
        val saved = store.save(
            LocalLlmConfiguration(
                apiEnabled = false,
                background = false,
                apiPort = 18080,
                maxOutputTokens = 512,
            ),
        )

        assertEquals(1, "% BEGIN ZARA LOCAL LLM CONFIG".toRegex().findAll(saved.text).count())
        assertTrue(saved.text.contains("manual_fact(kept)."))
        assertFalse(saved.text.contains("config(local_llm_api_enabled, true)."))
        assertTrue(saved.text.contains("config(local_llm_api_enabled, false)."))
        assertEquals(18080, store.readManaged().apiPort)
    }

    @Test
    fun `configuration rejects unsafe api port and output bounds`() {
        assertThrows(IllegalArgumentException::class.java) {
            LocalLlmConfiguration(apiPort = 80)
        }
        assertThrows(IllegalArgumentException::class.java) {
            LocalLlmConfiguration(apiPort = 65_536)
        }
        assertThrows(IllegalArgumentException::class.java) {
            LocalLlmConfiguration(maxOutputTokens = 0)
        }
        assertThrows(IllegalArgumentException::class.java) {
            LocalLlmConfiguration(maxOutputTokens = 4_097)
        }
    }

    @Test
    fun `runtime UI edits canonical Prolog config instead of a second preference store`() {
        val ui = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(ui.contains("LOCAL LLM API"))
        assertTrue(ui.contains("http://127.0.0.1:"))
        assertTrue(ui.contains("config.pl"))
        assertTrue(ui.contains("onSaveLocalLlmConfiguration"))
        assertTrue(activity.contains("onSaveLocalLlmConfiguration"))
        assertTrue(session.contains("saveLocalLlmConfiguration"))
        assertFalse(activity.contains("local-llm.bin"))
        assertFalse(ui.contains("0.0.0.0"))
    }
}
