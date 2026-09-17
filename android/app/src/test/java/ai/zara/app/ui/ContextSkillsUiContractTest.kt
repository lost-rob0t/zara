package ai.zara.app.ui

import ai.zara.app.ui.extensions.UiContribution
import ai.zara.app.ui.extensions.UiContributionKind
import ai.zara.app.ui.extensions.UiPlatform
import ai.zara.app.ui.extensions.UiSlot
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ContextSkillsUiContractTest {
    @Test
    fun boundedTypedCoreCommandSchemeIsAcceptedByUiContributionAbi() {
        val contribution = UiContribution(
            id = "compact-context",
            slot = UiSlot.CHAT_BOTTOM,
            kind = UiContributionKind.BUTTON,
            label = "Compact",
            action = "command:context.compact",
            priority = 100,
            platforms = setOf(UiPlatform.ANDROID),
            owner = "core:context",
        )

        assertEquals("command:context.compact", contribution.action)
    }

    @Test
    fun chatCompactUsesTypedCoreCommandAndNeverFallsThroughToModelText() {
        val appSource = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val extensionSource = File("src/main/java/ai/zara/app/ui/AndroidUiExtensions.kt").readText()

        assertTrue(appSource.contains("action.startsWith(\"command:\")"))
        assertTrue(appSource.contains("onCoreCommand"))
        assertFalse(
            appSource.contains(
                "action.startsWith(\"command:\") -> onSendText(action.removePrefix(\"command:\"))"
            )
        )
        assertTrue(extensionSource.contains("UiSlot.CHAT_BOTTOM"))
        assertTrue(extensionSource.contains("command:context.compact"))
        assertTrue(extensionSource.contains("Compact"))
    }

    @Test
    fun skillsIsANativeBrowserRouteInsteadOfAGatedPlaceholder() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("Skills(\"Skills\""))
        assertTrue(source.contains("AppSurface.Skills -> SkillBrowserSurface("))
        assertFalse(source.contains("AppSurface.Skills -> GatedSurface"))
    }

    @Test
    fun skillBrowserRendersMetadataEntriesWithoutInstructionBodies() {
        val file = File("src/main/java/ai/zara/app/ui/SkillBrowserSurface.kt")
        assertTrue("Skill Browser renderer must exist", file.isFile)
        val source = file.readText()

        assertTrue(source.contains("SkillBrowserEntry"))
        assertTrue(source.contains("description"))
        assertTrue(source.contains("source"))
        assertFalse(source.contains("entry.body"))
        assertFalse(source.contains("skill.body"))
    }
}
