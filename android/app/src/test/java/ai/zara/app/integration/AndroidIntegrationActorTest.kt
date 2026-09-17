package ai.zara.app.integration

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidIntegrationActorTest {
    @Test
    fun `unrestricted policy selects strongest backend that supports operation`() {
        val policy = AndroidAuthorityPolicy.fromSources {
            listOf("android_authority(unrestricted).")
        }
        val ordinary = FakeBackend(
            AndroidBackend.INTENT,
            AndroidAuthorityLevel.ELEVATED,
            setOf("launch"),
            "app:10000",
        )
        val root = FakeBackend(
            AndroidBackend.ROOT,
            AndroidAuthorityLevel.UNRESTRICTED,
            setOf("shell"),
            "root:0",
        )
        val actor = AndroidIntegrationActor(
            policy = policy,
            backends = listOf(ordinary, root),
            executor = Executors.newSingleThreadExecutor(),
        )

        try {
            val result = actor.execute(
                AndroidOperationRequest("auto", "shell", mapOf("command" to "id")),
            ).get(1, TimeUnit.SECONDS)

            assertTrue(result.success)
            assertEquals("root", result.backend)
            assertEquals("root:0", result.identity)
            assertEquals("ok:shell", result.output)
            assertEquals(1, root.executions)
            assertEquals(0, ordinary.executions)
        } finally {
            actor.close()
        }
    }

    @Test
    fun `dropping policy immediately removes raw authority without rebuilding`() {
        var source = "android_authority(unrestricted)."
        val policy = AndroidAuthorityPolicy.fromSources { listOf(source) }
        val root = FakeBackend(
            AndroidBackend.ROOT,
            AndroidAuthorityLevel.UNRESTRICTED,
            setOf("shell"),
            "root:0",
        )
        val actor = AndroidIntegrationActor(policy, listOf(root))

        try {
            assertTrue(
                actor.execute(AndroidOperationRequest("root", "shell"))
                    .get(1, TimeUnit.SECONDS)
                    .success,
            )

            source = "android_authority(standard)."
            val denied = actor.execute(AndroidOperationRequest("root", "shell"))
                .get(1, TimeUnit.SECONDS)

            assertFalse(denied.success)
            assertEquals(AndroidOperationError.AUTHORITY_DENIED, denied.error)
            assertEquals(1, root.executions)
        } finally {
            actor.close()
        }
    }

    @Test
    fun `explicit backend override can grant accessibility while global stays standard`() {
        val policy = AndroidAuthorityPolicy.fromSources {
            listOf(
                """
                android_authority(standard).
                android_backend(accessibility, unrestricted).
                """.trimIndent(),
            )
        }
        val accessibility = FakeBackend(
            AndroidBackend.ACCESSIBILITY,
            AndroidAuthorityLevel.UNRESTRICTED,
            setOf("gesture"),
            "accessibility:app",
        )
        val actor = AndroidIntegrationActor(policy, listOf(accessibility))

        try {
            val result = actor.execute(AndroidOperationRequest("accessibility", "gesture"))
                .get(1, TimeUnit.SECONDS)
            assertTrue(result.success)
        } finally {
            actor.close()
        }
    }

    @Test
    fun `unavailable backend reports truthful state instead of fake success`() {
        val policy = AndroidAuthorityPolicy.fromSources {
            listOf("android_authority(unrestricted).")
        }
        val backend = FakeBackend(
            AndroidBackend.ROOT,
            AndroidAuthorityLevel.UNRESTRICTED,
            setOf("shell"),
            "root:0",
            available = false,
        )
        val actor = AndroidIntegrationActor(policy, listOf(backend))

        try {
            val result = actor.execute(AndroidOperationRequest("root", "shell"))
                .get(1, TimeUnit.SECONDS)
            assertFalse(result.success)
            assertEquals(AndroidOperationError.BACKEND_UNAVAILABLE, result.error)
        } finally {
            actor.close()
        }
    }

    private class FakeBackend(
        override val backend: AndroidBackend,
        override val minimumAuthority: AndroidAuthorityLevel,
        private val operations: Set<String>,
        override val identity: String,
        var available: Boolean = true,
    ) : AndroidOperationBackend {
        var executions = 0

        override fun isAvailable(): Boolean = available

        override fun supports(operation: String): Boolean = operation in operations

        override fun execute(request: AndroidOperationRequest): AndroidOperationResult {
            executions += 1
            return AndroidOperationResult.completed(
                backend = backend.atom,
                identity = identity,
                output = "ok:${request.operation}",
            )
        }
    }
}
