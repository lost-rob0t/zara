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
            "AndroidAppSession must expose a non-null existing canonical expert invocation owner to UI composition",
            Regex(
                """fun\s+canonicalExpertInvocationPort\(\):\s+CanonicalExpertInvocationPort\s*(?:=|\{)""",
            ).containsMatchIn(sessionSource),
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

        val ownerMethod = sessionSource.substringAfter(
            "fun canonicalExpertInvocationPort(): CanonicalExpertInvocationPort",
            missingDelimiterValue = "",
        ).substringBefore("\n    fun ")
        assertFalse(
            "Canonical expert owner transport must not execute experts through raw Android Prolog queries",
            ownerMethod.contains("queryLocalProlog(") || ownerMethod.contains("localServer.query("),
        )
        assertFalse(
            "Canonical expert owner transport must not mint activation authority in AndroidAppSession",
            ownerMethod.contains("ActivationHandle("),
        )
        assertFalse(
            "Canonical expert owner transport must not construct a replacement port implementation",
            ownerMethod.contains("CanonicalExpertInvocationPort {") ||
                ownerMethod.contains("CanonicalExpertInvocationPort{"),
        )
    }

    @Test
    fun productionCompositionRequiresExplicitCoreOwnerProviderWithoutAndroidAuthority() {
        val sessionSource = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val applicationSource = File("src/main/java/ai/zara/app/ZaraApplication.kt").readText()
        val sessionConstructor = sessionSource
            .substringAfter("class AndroidAppSession(")
            .substringBefore(") : AutoCloseable {")

        assertTrue(
            "AndroidAppSession must require an explicit canonical expert provider from composition",
            Regex(
                """canonicalExpertInvocationPortProvider:\s*\(\)\s*->\s*CanonicalExpertInvocationPort\??""",
            ).containsMatchIn(sessionConstructor),
        )
        assertFalse(
            "AndroidAppSession must not silently default the canonical expert owner to null",
            sessionConstructor.contains("= { null }"),
        )
        assertTrue(
            "ZaraApplication must pass the canonical expert provider explicitly into AndroidAppSession",
            applicationSource.contains(
                "canonicalExpertInvocationPortProvider = canonicalExpertInvocationPortProvider",
            ),
        )
        assertFalse(
            "ZaraApplication must not fabricate a CanonicalExpertInvocationPort implementation",
            applicationSource.contains("object : CanonicalExpertInvocationPort"),
        )
        assertFalse(
            "ZaraApplication must not mint activation authority",
            applicationSource.contains("ActivationHandle("),
        )
        assertFalse(
            "ZaraApplication canonical expert composition must not route through raw local Prolog",
            applicationSource.contains("canonicalExpertInvocationPortProvider = ::queryLocalProlog") ||
                applicationSource.contains("canonicalExpertInvocationPortProvider = appSession::queryLocalProlog"),
        )
    }
}
