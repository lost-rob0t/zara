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
    fun untrustedWorkspaceAllowsOnlySchemaAndValidatedOperatorDirectives() {
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
    fun adHocQueriesRejectEffectfulMetaQuotedFileAndDynamicGoalAuthority() {
        listOf(
            "http_open(url, Result)",
            "tcp_connect(socket, Result)",
            "sqlite3_open(path, Result, Status)",
            "use_foreign_module(Result, [])",
            "thread_create(Result, worker)",
            "read_file_to_string('/etc/passwd', Result, [])",
            "'shell'('id'), Result = ok",
            "findall(X, Goal, Result)",
            "Goal =.. [shell, id], Result = Goal",
            "Goal, Result = ok",
            "\\+ Goal, Result = ok",
        ).forEach { query ->
            try {
                PrologAuthorityPolicy.requireSafeQuery(query)
                throw AssertionError("effectful query was accepted: $query")
            } catch (_: IllegalArgumentException) {
            }
        }
    }

    @Test
    fun workspaceMetaRulesCannotExecuteStoredGoalsIndirectlyWhenUntrusted() {
        val metaCall = PrologSourceAnalyzer.analyze(
            "unsafe_meta.pl",
            "run(Result) :- payload(Goal), findall(X, Goal, Result).\n",
        )
        val dynamicGoal = PrologSourceAnalyzer.analyze(
            "unsafe_goal.pl",
            "payload(noop).\nrun(Result) :- payload(Goal), Goal, Result = ok.\n",
        )
        val negatedDynamicGoal = PrologSourceAnalyzer.analyze(
            "unsafe_negated_goal.pl",
            "payload(noop).\nrun(Result) :- payload(Goal), \\+ Goal, Result = ok.\n",
        )
        val storedEffectfulTerm = PrologSourceAnalyzer.analyze(
            "unsafe_fact.pl",
            "payload(shell('id')).\n",
        )

        assertTrue(PrologAuthorityPolicy.validate(metaCall).any { it.message.contains("findall") })
        assertTrue(PrologAuthorityPolicy.validate(dynamicGoal).any { it.message.contains("dynamic variable goal") })
        assertTrue(PrologAuthorityPolicy.validate(negatedDynamicGoal).any { it.message.contains("meta goal") })
        assertTrue(PrologAuthorityPolicy.validate(storedEffectfulTerm).any { it.message.contains("shell") })
    }

    @Test
    fun localServerConsultsTrustedOperatorEffectfulSource() {
        val root = temporary.newFolder("trusted-workspace")
        val workspace = PrologWorkspace(root)
        workspace.saveSource("operator.pl", "run(Result) :- shell('id'), Result = ok.\n")
        val bridge = RecordingBridge()
        val server = LocalZaraServer(
            bridge = bridge,
            corePath = "/private/core.pl",
            workspace = workspace,
        )

        val state = server.start().get(2, TimeUnit.SECONDS)

        assertEquals(LocalServerPhase.READY, state.phase)
        assertEquals(1, bridge.initializeCount)
        assertEquals(listOf("operator.pl"), bridge.consulted.map { File(it).name })
        server.close()
    }

    @Test
    fun untrustedBundleRejectsEffectfulSourceBeforePersistence() {
        val workspace = PrologWorkspace(temporary.newFolder("import-workspace"))
        workspace.saveSource("trusted.pl", "trusted(Result) :- Result = ok.\n")
        val source = "evil(Result) :- shell('id'), Result = ok.\n"
        val bundle = buildString {
            append("ZARA-PROLOG-WORKSPACE/1\n")
            append("SOURCE imported.pl ${source.encodeToByteArray().size}\n")
            append(source)
            append("END-SOURCE\n")
        }

        try {
            workspace.importBundle(bundle)
            throw AssertionError("untrusted effectful import was accepted")
        } catch (_: IllegalArgumentException) {
        }

        assertEquals(listOf("trusted.pl"), workspace.listSources().map(PrologSource::name))
    }

    @Test
    fun invalidReloadPreflightLeavesLastGoodRuntimeAlive() {
        val root = temporary.newFolder("reload-workspace")
        val workspace = PrologWorkspace(root)
        workspace.saveSource("safe.pl", "safe(Result) :- Result = ok.\n")
        val bridge = RecordingBridge()
        val server = LocalZaraServer(bridge, "/private/core.pl", workspace)

        assertEquals(LocalServerPhase.READY, server.start().get(2, TimeUnit.SECONDS).phase)
        assertEquals(1, bridge.initializeCount)
        assertEquals(0, bridge.shutdownCount)

        File(root, "safe.pl").writeText("safe(Result) :- (Result = ok.\n")
        val rejected = server.reload().get(2, TimeUnit.SECONDS)

        assertEquals(LocalServerPhase.FAILED, rejected.phase)
        assertEquals(LocalServerPhase.READY, server.state().phase)
        assertEquals(1, server.state().generation)
        assertEquals(1, bridge.initializeCount)
        assertEquals(0, bridge.shutdownCount)

        File(root, "safe.pl").writeText("safe(Result) :- Result = ok.\n")
        assertEquals(LocalServerPhase.READY, server.reload().get(2, TimeUnit.SECONDS).phase)
        assertEquals(2, bridge.initializeCount)
        assertEquals(1, bridge.shutdownCount)
        server.close()
    }

    private class RecordingBridge : TreallaBridge {
        var initializeCount = 0
        var shutdownCount = 0
        val consulted = mutableListOf<String>()

        override fun initialize(coreAssetPath: String) {
            initializeCount += 1
        }

        override fun consult(sourcePath: String) {
            consulted += sourcePath
        }

        override fun evaluate(query: String): List<String> = listOf("ok")

        override fun shutdown() {
            shutdownCount += 1
        }
    }
}
