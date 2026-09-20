package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class AndroidPureSymbolicConversationFactoryTest {
    @Test
    fun frameResolverQueryMatchesCanonicalAndroidResolverContract() {
        val query = AndroidPureSymbolicConversationFactory.frameResolverQuery(
            "  set a timer for \"five\"\\minutes\nplease  ",
        )

        assertEquals(
            "resolve_frames(\"set a timer for \\\"five\\\"\\\\minutes\\nplease\", passive, [], Frames), member(Result, Frames)",
            query,
        )
    }

    @Test
    fun frameResolverQueryRejectsBlankInput() {
        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.frameResolverQuery("   ")
        }
    }

    @Test
    fun frameResolverQueryKeepsAndroidRuntimeBound() {
        val oversized = "x".repeat(8_193)

        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.frameResolverQuery(oversized)
        }
    }
}
