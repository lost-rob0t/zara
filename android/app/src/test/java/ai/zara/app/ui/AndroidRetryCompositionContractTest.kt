package ai.zara.app.ui

import java.nio.file.Files
import java.nio.file.Path
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidRetryCompositionContractTest {
    private val root = Path.of("..").toAbsolutePath().normalize()

    @Test fun `retry reuses canonical failed user turn and auto retry is bounded`() {
        val activity = Files.readString(
            root.resolve("app/src/main/java/ai/zara/app/MainActivity.kt")
        )
        val store = Files.readString(
            root.resolve("app/src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt")
        )

        assertTrue(activity.contains("TurnRetryPolicy.shouldAutoRetry"))
        assertTrue(activity.contains("conversationStore.retryFailedTurn("))
        assertTrue(activity.contains("TurnRetryPolicy.MAX_ATTEMPTS"))
        assertFalse(
            "manual retry must not append the same user message via submitChatText",
            activity.contains(
                "onRetryTurn = { text ->\n" +
                    "                    val selected = conversationState.selectedConversation\n" +
                    "                    val project = selected?.projectId?.let(projectState::project)\n" +
                    "                    if (selected != null) submitChatText(text, selected, project)"
            ),
        )

        assertTrue(store.contains("fun retryFailedTurn("))
        assertTrue(store.contains("HistoryMessageStatus.Error"))
        assertTrue(store.contains("HistoryMessageStatus.Pending"))
        assertTrue(store.contains("attempt >= maxAttempts"))
    }

    @Test fun `retry UI exposes automatic retry and capped attempt state`() {
        val ui = Files.readString(root.resolve("app/src/main/java/ai/zara/app/ui/ZaraApp.kt"))
        assertTrue(ui.contains("retryStatusLabel(failure)"))
        assertTrue(ui.contains("Retrying automatically"))
        assertTrue(ui.contains("Attempt 2/2") || ui.contains("failure.maxAttempts"))
    }
}
