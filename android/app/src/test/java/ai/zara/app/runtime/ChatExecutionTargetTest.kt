package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ChatExecutionTargetTest {
    @Test
    fun `wire vocabulary is closed and has no auto target`() {
        assertEquals(
            listOf("local", "direct_provider", "zara_server"),
            ChatExecutionTarget.entries.map { it.wireName },
        )
        assertEquals(ChatExecutionTarget.LOCAL, ChatExecutionTarget.parse("local"))
        assertEquals(ChatExecutionTarget.DIRECT_PROVIDER, ChatExecutionTarget.parse("direct_provider"))
        assertEquals(ChatExecutionTarget.ZARA_SERVER, ChatExecutionTarget.parse("zara_server"))
    }

    @Test(expected = IllegalArgumentException::class)
    fun `unknown target fails closed`() {
        ChatExecutionTarget.parse("auto")
    }

    @Test
    fun `direct provider unavailability never falls back to local`() {
        val selection = ChatExecutionSelection(ChatExecutionTarget.DIRECT_PROVIDER, generation = 4)
        val status = selection.status(
            ChatExecutionPrerequisites(
                localReady = true,
                directProviderReady = false,
                zaraServerReady = true,
            )
        )

        assertEquals(ChatExecutionTarget.DIRECT_PROVIDER, status.target)
        assertFalse(status.ready)
        assertEquals(ChatExecutionUnavailableReason.DIRECT_PROVIDER_UNAVAILABLE, status.unavailableReason)
    }

    @Test
    fun `zara server unavailability never falls back to provider`() {
        val selection = ChatExecutionSelection(ChatExecutionTarget.ZARA_SERVER, generation = 8)
        val status = selection.status(
            ChatExecutionPrerequisites(
                localReady = true,
                directProviderReady = true,
                zaraServerReady = false,
            )
        )

        assertEquals(ChatExecutionTarget.ZARA_SERVER, status.target)
        assertFalse(status.ready)
        assertEquals(ChatExecutionUnavailableReason.ZARA_SERVER_UNAVAILABLE, status.unavailableReason)
    }

    @Test
    fun `switching target advances generation and fences stale completion`() {
        val local = ChatExecutionSelection.initial()
        val provider = local.select(ChatExecutionTarget.DIRECT_PROVIDER)
        val server = provider.select(ChatExecutionTarget.ZARA_SERVER)

        assertEquals(0L, local.generation)
        assertEquals(1L, provider.generation)
        assertEquals(2L, server.generation)
        assertFalse(server.accepts(provider.generation))
        assertTrue(server.accepts(server.generation))
    }

    @Test
    fun `reselecting same target does not churn generation`() {
        val selection = ChatExecutionSelection(ChatExecutionTarget.LOCAL, generation = 9)

        assertEquals(selection, selection.select(ChatExecutionTarget.LOCAL))
    }

    @Test(expected = IllegalStateException::class)
    fun `generation overflow fails closed`() {
        ChatExecutionSelection(ChatExecutionTarget.LOCAL, Long.MAX_VALUE)
            .select(ChatExecutionTarget.DIRECT_PROVIDER)
    }

    @Test
    fun `local unavailability is reported honestly`() {
        val status = ChatExecutionSelection.initial().status(
            ChatExecutionPrerequisites(
                localReady = false,
                directProviderReady = true,
                zaraServerReady = true,
            )
        )

        assertEquals(ChatExecutionTarget.LOCAL, status.target)
        assertFalse(status.ready)
        assertEquals(ChatExecutionUnavailableReason.LOCAL_UNAVAILABLE, status.unavailableReason)
    }
}
