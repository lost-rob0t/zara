package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class CanonicalExpertProductionWiringContractTest {
    @Test
    fun pureSymbolicChatInjectsExistingCanonicalExpertOwnerIntoFactory() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val marker = "pureSymbolicSubmit = AndroidPureSymbolicConversationFactory.create("
        check(source.contains(marker)) {
            "MainActivity must construct the production pure-symbolic conversation factory"
        }
        val factoryCall = source.substringAfter(marker)
            .substringBefore("\n            )::submit")

        assertTrue(
            "Production Android pure-symbolic chat must inject the existing canonical expert " +
                "invocation owner; the factory's null default makes natural expert turns fail closed",
            factoryCall.contains("canonicalExpertInvocationPort ="),
        )
        assertFalse(
            "Production must never satisfy canonical expert composition with a null authority",
            factoryCall.contains("canonicalExpertInvocationPort = null"),
        )
    }
}
