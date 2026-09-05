package ai.zara.app.assistant

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantLifecycleFenceTest {
    @Test
    fun `shutdown invalidates an in-flight assistant start`() {
        val fence = AssistantLifecycleFence()
        val token = fence.beginStart()

        assertTrue(fence.isCurrent(token))
        fence.invalidate()

        assertFalse(fence.isCurrent(token))
    }

    @Test
    fun `new start after invalidation cannot revive stale token`() {
        val fence = AssistantLifecycleFence()
        val stale = fence.beginStart()
        fence.invalidate()
        val fresh = fence.beginStart()

        assertFalse(fence.isCurrent(stale))
        assertTrue(fence.isCurrent(fresh))
    }

    @Test
    fun `duplicate invalidation keeps all prior starts stale`() {
        val fence = AssistantLifecycleFence()
        val first = fence.beginStart()
        fence.invalidate()
        val second = fence.beginStart()
        fence.invalidate()

        assertFalse(fence.isCurrent(first))
        assertFalse(fence.isCurrent(second))
    }
}
