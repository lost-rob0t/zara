package ai.zara.app.history

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class SymbolicProjectScopeContractTest {
    @Test
    fun `new projectless conversation starts at generation zero`() {
        val scope = SymbolicProjectScopeContract.next(null, null)

        assertNull(scope.projectId)
        assertEquals(0L, scope.projectGeneration)
    }

    @Test
    fun `first project binding starts generation one`() {
        val scope = SymbolicProjectScopeContract.next(null, " project-a ")

        assertEquals("project-a", scope.projectId)
        assertEquals(1L, scope.projectGeneration)
    }

    @Test
    fun `same project preserves canonical generation`() {
        val current = projection(projectId = "project-a", projectGeneration = 4L)

        val scope = SymbolicProjectScopeContract.next(current, "project-a")

        assertEquals("project-a", scope.projectId)
        assertEquals(4L, scope.projectGeneration)
    }

    @Test
    fun `project switch advances generation exactly once`() {
        val current = projection(projectId = "project-a", projectGeneration = 4L)

        val scope = SymbolicProjectScopeContract.next(current, "project-b")

        assertEquals("project-b", scope.projectId)
        assertEquals(5L, scope.projectGeneration)
    }

    @Test
    fun `detaching project also advances stale context fence`() {
        val current = projection(projectId = "project-a", projectGeneration = 4L)

        val scope = SymbolicProjectScopeContract.next(current, null)

        assertNull(scope.projectId)
        assertEquals(5L, scope.projectGeneration)
    }

    @Test(expected = IllegalArgumentException::class)
    fun `control characters are rejected from project identity`() {
        SymbolicProjectScopeContract.next(null, "project-a\nproject-b")
    }

    private fun projection(
        projectId: String?,
        projectGeneration: Long,
    ): SymbolicConversationProjection = SymbolicConversationProjection(
        conversationId = "conversation-project-scope",
        projectionGeneration = 1L,
        runtimeGeneration = 1L,
        turnId = "turn-project-scope",
        outcome = "pending",
        projectId = projectId,
        projectGeneration = projectGeneration,
        dialogueAct = "conversation",
        dialogueStateJson = "{}",
        providersEnabled = false,
        maxModelCalls = 0L,
        providerCalls = 0L,
        modelCalls = 0L,
    )
}
