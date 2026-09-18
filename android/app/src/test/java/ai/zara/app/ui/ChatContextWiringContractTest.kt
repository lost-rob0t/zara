package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class ChatContextWiringContractTest {
    @Test
    fun mainChatExposesProjectsAsAFirstClassShortcut() {
        val app = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val controls = File("src/main/java/ai/zara/app/ui/ChatContextControls.kt").readText()

        assertTrue(app.contains("ProjectsMainShortcut("))
        assertTrue(app.contains("navigation = navigation.selectRoute(AppRoute.Projects)"))
        assertTrue(controls.contains("contentDescription = \"Open Projects\""))
        assertTrue(controls.contains("\"◇ Projects\""))
    }

    @Test
    fun plusButtonOwnsUploadTextContextAndAddChatToProjectActions() {
        val app = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val controls = File("src/main/java/ai/zara/app/ui/ChatContextControls.kt").readText()

        assertTrue(app.contains("ChatPlusButton("))
        assertTrue(controls.contains("\"Upload files\""))
        assertTrue(controls.contains("\"Add text context\""))
        assertTrue(controls.contains("\"Add chat to project\""))
        assertTrue(controls.contains("\"Open Projects\""))
    }

    @Test
    fun hostUsesSafImporterAndContextualizesTheRealSubmittedTurn() {
        val host = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(host.contains("ActivityResultContracts.OpenMultipleDocuments()"))
        assertTrue(host.contains("AndroidChatContextImporter.importAll(contentResolver, uris)"))
        assertTrue(host.contains("contextualizeUserText(text, attachments)"))
        assertTrue(host.contains("appSession.submitText(contextualizedText)"))
        assertTrue(host.contains("appSession.submitProjectText("))
    }

    @Test
    fun addChatToProjectBindsRemoteConversationAndCopiesExplicitContext() {
        val host = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(host.contains("runtimeState.selectedConversationId"))
        assertTrue(host.contains("projectStore.bindConversation(target.id, conversationId)"))
        assertTrue(host.contains("contextStore.copyScope("))
        assertTrue(host.contains("ORDINARY_CHAT_CONTEXT_SCOPE"))
        assertTrue(host.contains("projectStore.select(target.id)"))
    }

    @Test
    fun explicitContextIsVisibleAndRemovableBeforeSend() {
        val app = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val controls = File("src/main/java/ai/zara/app/ui/ChatContextControls.kt").readText()

        assertTrue(app.contains("ChatContextStrip("))
        assertTrue(app.contains("onRemove = onRemoveContextAttachment"))
        assertTrue(controls.contains("Remove context"))
    }
}
