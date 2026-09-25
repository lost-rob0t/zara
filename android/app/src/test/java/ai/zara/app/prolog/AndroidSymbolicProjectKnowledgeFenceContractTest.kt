package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * P0 composition contract: a conversation moved between projects must not feed the previous
 * project's durable symbolic knowledge into the next natural turn.
 *
 * Project metadata stays owned by CanonicalConversationStore and dialogue state stays owned by the
 * canonical PortableConversationStore projection. This test forbids fixing the boundary with an
 * Android-local context cache or second project/conversation store.
 */
class AndroidSymbolicProjectKnowledgeFenceContractTest {
    @Test
    fun `factory resets durable dialogue context when canonical project identity changes`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()

        assertTrue(
            "natural-turn preparation must detect that the requested canonical project differs " +
                "from the project recorded by the last symbolic projection",
            factory.contains("projectScope.projectId != current.projectId"),
        )
        assertTrue(
            "a project change must enter the canonical dialogue router with empty Context0 instead " +
                "of decoding the prior project's continuation",
            factory.contains("SymbolicDialogueContextCodec.emptyContextTerm"),
        )
        assertTrue(
            "pending projection construction must know when project-scoped knowledge is being reset",
            factory.contains("resetProjectKnowledge"),
        )
        listOf(
            "discourseEntitiesJson",
            "unresolvedQuestionsJson",
            "expertEvidenceJson",
            "verifiedFactsJson",
        ).forEach { field ->
            assertTrue(
                "$field must be cleared instead of copied across a canonical project change",
                factory.contains("$field = if (resetProjectKnowledge) \"[]\" else base.$field"),
            )
        }
    }
}
