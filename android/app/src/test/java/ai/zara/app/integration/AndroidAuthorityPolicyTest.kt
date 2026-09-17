package ai.zara.app.integration

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidAuthorityPolicyTest {
    @Test
    fun `fresh policy defaults to standard`() {
        val snapshot = AndroidAuthorityParser.parse(emptyList())

        assertEquals(AndroidAuthorityLevel.STANDARD, snapshot.global)
        assertEquals(AndroidConfirmationMode.SENSITIVE, snapshot.confirmation)
        assertTrue(snapshot.allows(AndroidBackend.INTENT, AndroidAuthorityLevel.STANDARD))
        assertFalse(snapshot.allows(AndroidBackend.ROOT, AndroidAuthorityLevel.UNRESTRICTED))
    }

    @Test
    fun `unrestricted fact grants every backend at unrestricted level`() {
        val snapshot = AndroidAuthorityParser.parse(
            listOf(
                """
                android_authority(unrestricted).
                android_confirmation(unrestricted, none).
                """.trimIndent(),
            ),
        )

        assertEquals(AndroidAuthorityLevel.UNRESTRICTED, snapshot.global)
        assertEquals(AndroidConfirmationMode.NONE, snapshot.confirmation)
        AndroidBackend.entries.forEach { backend ->
            assertTrue(snapshot.allows(backend, AndroidAuthorityLevel.UNRESTRICTED))
        }
    }

    @Test
    fun `backend override can widen only that backend`() {
        val snapshot = AndroidAuthorityParser.parse(
            listOf(
                """
                android_authority(standard).
                android_backend(accessibility, unrestricted).
                """.trimIndent(),
            ),
        )

        assertEquals(AndroidAuthorityLevel.UNRESTRICTED, snapshot.levelFor(AndroidBackend.ACCESSIBILITY))
        assertEquals(AndroidAuthorityLevel.STANDARD, snapshot.levelFor(AndroidBackend.INTENT))
        assertTrue(snapshot.allows(AndroidBackend.ACCESSIBILITY, AndroidAuthorityLevel.UNRESTRICTED))
        assertFalse(snapshot.allows(AndroidBackend.INTENT, AndroidAuthorityLevel.ELEVATED))
    }

    @Test
    fun `backend override can also narrow unrestricted global policy`() {
        val snapshot = AndroidAuthorityParser.parse(
            listOf(
                """
                android_authority(unrestricted).
                android_backend(shell, locked).
                """.trimIndent(),
            ),
        )

        assertEquals(AndroidAuthorityLevel.LOCKED, snapshot.levelFor(AndroidBackend.SHELL))
        assertFalse(snapshot.allows(AndroidBackend.SHELL, AndroidAuthorityLevel.STANDARD))
        assertTrue(snapshot.allows(AndroidBackend.ROOT, AndroidAuthorityLevel.UNRESTRICTED))
    }

    @Test
    fun `comments and unrelated Prolog do not affect authority`() {
        val snapshot = AndroidAuthorityParser.parse(
            listOf(
                """
                % android_authority(unrestricted).
                config(theme, outrun).
                effective(Key, Result) :- config(Key, Result).
                android_authority(elevated). % explicit local choice
                """.trimIndent(),
            ),
        )

        assertEquals(AndroidAuthorityLevel.ELEVATED, snapshot.global)
    }

    @Test
    fun `conflicting global authority facts fail closed instead of picking one`() {
        assertThrows(IllegalArgumentException::class.java) {
            AndroidAuthorityParser.parse(
                listOf(
                    "android_authority(standard).\nandroid_authority(unrestricted).\n",
                ),
            )
        }
    }

    @Test
    fun `unknown level or backend is rejected when using authority predicates`() {
        assertThrows(IllegalArgumentException::class.java) {
            AndroidAuthorityParser.parse(listOf("android_authority(godmode)."))
        }
        assertThrows(IllegalArgumentException::class.java) {
            AndroidAuthorityParser.parse(listOf("android_backend(telepathy, unrestricted)."))
        }
    }
}
