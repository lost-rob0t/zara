package ai.zara.app.ui

import ai.zara.app.ui.extensions.PrologUiTermParser
import ai.zara.app.ui.extensions.UiContributionKind
import ai.zara.app.ui.extensions.UiPlatform
import ai.zara.app.ui.extensions.UiSlot
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class UiPrologExtensionTest {
    @Test
    fun prologUiSevenTermUsesSamePortableSchema() {
        val contribution = PrologUiTermParser.parse(
            "ui(logic,drawer,surface,\"Logic +\",\"route:logic\",20,[desktop,android])"
        )

        assertEquals("logic", contribution.id)
        assertEquals(UiSlot.DRAWER, contribution.slot)
        assertEquals(UiContributionKind.SURFACE, contribution.kind)
        assertEquals("Logic +", contribution.label)
        assertEquals("route:logic", contribution.action)
        assertEquals(setOf(UiPlatform.DESKTOP, UiPlatform.ANDROID), contribution.platforms)
    }

    @Test
    fun canonicalComposeShellConsumesExtensionSlotsAndUngatesPlugins() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("rememberAndroidUiContributions(localServerState.generation)"))
        assertTrue(source.contains("AndroidDrawerUiExtensions(uiContributions"))
        assertTrue(source.contains("UiSlot.CHAT_TOP"))
        assertTrue(source.contains("UiSlot.CHAT_BOTTOM"))
        assertTrue(source.contains("UiSlot.SETTINGS"))
        assertTrue(source.contains("PluginExtensionsSurface("))
        assertTrue(source.contains("Plugins(\"Plugins\", \"⬡\")"))
        assertFalse(source.contains("AppSurface.Plugins -> GatedSurface"))
    }

    @Test
    fun pluginUiActionDoesNotFallThroughToChatBeforeTypedHostDispatcherExists() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("action.startsWith(\"plugin:\") -> Unit"))
        assertFalse(
            source.contains(
                "action.startsWith(\"plugin:\") -> onSendText(action.removePrefix(\"plugin:\"))"
            )
        )
    }
}
