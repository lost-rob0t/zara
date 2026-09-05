package ai.zara.app.assistant

import ai.zara.app.runtime.RoleOutcome
import org.junit.Assert.assertEquals
import org.junit.Test

class AssistantRoleControllerTest {
    @Test
    fun `unavailable platform reports platform unavailable without checking role`() {
        val platform = FakeAssistantRolePlatform(available = false, held = true)
        val observed = mutableListOf<RoleOutcome>()

        val outcome = AssistantRoleController(platform, observed::add).assess()

        assertEquals(RoleOutcome.PLATFORM_UNAVAILABLE, outcome)
        assertEquals(listOf(RoleOutcome.PLATFORM_UNAVAILABLE), observed)
        assertEquals(0, platform.heldChecks)
    }

    @Test
    fun `available held role reports held`() {
        val platform = FakeAssistantRolePlatform(available = true, held = true)
        val observed = mutableListOf<RoleOutcome>()

        val outcome = AssistantRoleController(platform, observed::add).assess()

        assertEquals(RoleOutcome.HELD, outcome)
        assertEquals(listOf(RoleOutcome.HELD), observed)
        assertEquals(1, platform.heldChecks)
    }

    @Test
    fun `available missing role reports not held`() {
        val platform = FakeAssistantRolePlatform(available = true, held = false)
        val observed = mutableListOf<RoleOutcome>()

        val outcome = AssistantRoleController(platform, observed::add).assess()

        assertEquals(RoleOutcome.NOT_HELD, outcome)
        assertEquals(listOf(RoleOutcome.NOT_HELD), observed)
        assertEquals(1, platform.heldChecks)
    }

    @Test
    fun `role request completion trusts observed role state`() {
        val platform = FakeAssistantRolePlatform(available = true, held = false)
        val observed = mutableListOf<RoleOutcome>()
        val controller = AssistantRoleController(platform, observed::add)

        val outcome = controller.completeRequest()

        assertEquals(RoleOutcome.NOT_HELD, outcome)
        assertEquals(listOf(RoleOutcome.NOT_HELD), observed)
    }

    @Test
    fun `role request completion reports held when platform confirms ownership`() {
        val platform = FakeAssistantRolePlatform(available = true, held = true)
        val observed = mutableListOf<RoleOutcome>()
        val controller = AssistantRoleController(platform, observed::add)

        val outcome = controller.completeRequest()

        assertEquals(RoleOutcome.HELD, outcome)
        assertEquals(listOf(RoleOutcome.HELD), observed)
    }

    private class FakeAssistantRolePlatform(
        private val available: Boolean,
        private val held: Boolean,
    ) : AssistantRolePlatform {
        var heldChecks: Int = 0
            private set

        override fun isAvailable(): Boolean = available

        override fun isHeld(): Boolean {
            heldChecks += 1
            return held
        }
    }
}
