package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class CanonicalExpertOwnerSourceContractTest {
    @Test
    fun productionCompositionSourcesCanonicalExpertPortFromExistingAppSessionOwner() {
        val sessionSource = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val activitySource = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val marker = "pureSymbolicSubmit = AndroidPureSymbolicConversationFactory.create("
        check(activitySource.contains(marker)) {
            "MainActivity must construct the production pure-symbolic conversation factory"
        }
        val factoryCall = activitySource.substringAfter(marker)
            .substringBefore("\n            )::submit")

        assertTrue(
            "AndroidAppSession must expose the existing canonical expert invocation owner to UI composition",
            sessionSource.contains("fun canonicalExpertInvocationPort(): CanonicalExpertInvocationPort"),
        )
        assertTrue(
            "MainActivity must source canonical expert invocation from AndroidAppSession rather than fabricate one",
            factoryCall.contains("canonicalExpertInvocationPort = appSession.canonicalExpertInvocationPort()"),
        )
        assertFalse(
            "AndroidAppSession must not fabricate an inline CanonicalExpertInvocationPort authority",
            sessionSource.contains("object : CanonicalExpertInvocationPort"),
        )
        assertFalse(
            "UI composition must not construct activation handles locally",
            activitySource.contains("ActivationHandle("),
        )
    }
}
