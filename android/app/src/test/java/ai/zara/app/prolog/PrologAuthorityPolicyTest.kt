package ai.zara.app.prolog

import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalZaraServer
import java.io.File
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class PrologAuthorityPolicyTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun workspaceRulesCannotSmuggleEffectfulCallsPastSafeQueryText() {
        val document = PrologSourceAnalyzer.analyze(
            "unsafe.pl",
            "evil(Result) :- shell('id'), Result = ok.\n",
        )

        val diagnostics = PrologAuthorityPolicy.validate(document)

        assertTrue(diagnostics.any { it.message.contains("shell") })
    }

    @Test
    fun privateWorkspaceAllowsOnlySchemaAndValidatedOperatorDirectives() {
        val safe = PrologSourceAnalyzer.analyze(
            "safe.pl",
            """
                :- zara_schema(signal, 2, [atom, atom]).
                :- op(600, xfx, because).
                signal(alice, red).
                explain(Entity, Result) :- signal(Entity, Result).
            """.trimIndent(),
        )
        assertTrue(PrologAuthorityPolicy.validate(safe).isEmpty())

        val unsafe = PrologSourceAnalyzer.analyze(
            "unsafe.pl",
            ":- initialization(shell('id')).\n",
        )
        assertTrue(PrologAuthorityPolicy.validate(unsafe).isNotEmpty())
    }

    @Test
    fun adHocQueriesAlsoRejectNetworkDatabaseFfiAndConcurrencyAuthority() {
        listOf(
            "http_open(url, Result)",
            "tcp_connect(socket, Result)",
            "sqlite3_open(path, Result, Status)",
            "use_foreign_module(Result, [])",
            "thread_create(Result, worker)",
        ).forEach { query ->
            try {
                PrologAuthorityPolicy.requireSafeQuery(query)
                throw AssertionError("effectful query was accepted: $query")
            } catch (_: IllegalArgumentException) {
            }
        }
    }

    @Test
    fun localServerRejectsPersistedUnsafeSourceBeforeTreallaInitialization() {
        val root = temporary.newFolder("unsafe-workspace")
        File(root, "unsafe.pl").writeText("evil(Result) :- shell('id'), Result = ok.\n")
        val bridge = RecordingBridge()
        val server = LocalZaraServer(
            bridge = bridge,
            corePath = "/private/core.pl",
            workspace = PrologWorkspace(root),
        )

        val state = server.start().get(2, TimeUnit.SECONDS)

        assertEquals(LocalServerPhase.FAILED, state.phase)
        assertTrue(state.failure.orEmpty().contains("shell"))
        assertEquals(0, bridge.initializeCount)
        assertTrue(bridge.consulted.isEmpty())
        server.close()
    }

    private class RecordingBridge : TreallaBridge {
        var initializeCount = 0
        val consulted = mutableListOf<String>()

        override fun initialize(coreAssetPath: String) {
            initializeCount += 1
        }

        override fun consult(sourcePath: String) {
            consulted += sourcePath
        }

        override fun evaluate(query: String): List<String> = listOf("ok")

        override fun shutdown() = Unit
    }
}
