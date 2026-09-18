package ai.zara.app.runtime

import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.prolog.TreallaBridge
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class LocalZaraServerTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun localServerBootsConsultsWorkspaceAndRunsQueriesOnOneActorThread() {
        val bridge = RecordingTreallaBridge()
        val workspace = PrologWorkspace(temporary.newFolder("workspace"))
        workspace.saveSource("family.pl", "parent(alice, bob).\n")
        val server = LocalZaraServer(
            bridge = bridge,
            corePath = "/private/semantic_core.pl",
            workspace = workspace,
        )

        val ready = server.start().get(2, TimeUnit.SECONDS)
        val result = server.query("parent(alice, Result)").get(2, TimeUnit.SECONDS)

        assertEquals(LocalServerPhase.READY, ready.phase)
        assertEquals(listOf("bob"), result.terms)
        assertEquals(listOf("/private/semantic_core.pl"), bridge.initialized)
        assertEquals(listOf("family.pl"), bridge.consulted.map { it.substringAfterLast('/') })
        assertEquals(1, bridge.threadNames.distinct().size)
        assertTrue(bridge.threadNames.distinct().single().contains("zara-local-server"))
        server.close()
    }

    @Test
    fun reloadRecreatesRuntimeSoEditedFactsDoNotAccumulate() {
        val bridge = RecordingTreallaBridge()
        val workspace = PrologWorkspace(temporary.newFolder("reload"))
        workspace.saveSource("facts.pl", "color(sky, blue).\n")
        val server = LocalZaraServer(bridge, "/private/core.pl", workspace)
        server.start().get(2, TimeUnit.SECONDS)

        workspace.saveSource("facts.pl", "color(sky, gray).\n")
        server.reload().get(2, TimeUnit.SECONDS)

        assertEquals(2, bridge.initialized.size)
        assertEquals(1, bridge.shutdownCount)
        assertEquals(2, bridge.consulted.size)
        server.close()
    }

    @Test
    fun unsafeQueryFailsBeforeNativeRuntimeReceivesIt() {
        val bridge = RecordingTreallaBridge()
        val server = LocalZaraServer(
            bridge,
            "/private/core.pl",
            PrologWorkspace(temporary.newFolder("safe")),
        )
        server.start().get(2, TimeUnit.SECONDS)

        val failure = runCatching {
            server.query("consult(Result)").get(2, TimeUnit.SECONDS)
        }.exceptionOrNull()

        assertTrue(failure != null)
        assertTrue(bridge.queries.isEmpty())
        server.close()
    }

    @Test
    fun failedReloadCanRecoverAfterWorkspaceRollback() {
        val bridge = RecordingTreallaBridge()
        val workspace = PrologWorkspace(temporary.newFolder("recover"))
        workspace.saveSource("facts.pl", "ready(Result) :- Result = yes.\n")
        val server = LocalZaraServer(bridge, "/private/core.pl", workspace)
        server.start().get(2, TimeUnit.SECONDS)
        bridge.failNextConsult = true

        val failed = server.reload().get(2, TimeUnit.SECONDS)
        val recovered = server.reload().get(2, TimeUnit.SECONDS)

        assertEquals(LocalServerPhase.FAILED, failed.phase)
        assertEquals(LocalServerPhase.READY, recovered.phase)
        server.close()
    }

    @Test
    fun cancelledQueuedQueryNeverReachesNativeRuntime() {
        val bridge = BlockingTreallaBridge()
        val server = LocalZaraServer(
            bridge,
            "/private/core.pl",
            PrologWorkspace(temporary.newFolder("cancelled")),
        )
        server.start().get(2, TimeUnit.SECONDS)

        val first = server.query("first(Result)")
        assertTrue(bridge.firstQueryEntered.await(2, TimeUnit.SECONDS))

        val cancelled = server.query("second(Result)")
        assertTrue(cancelled.cancel(true))

        bridge.releaseFirstQuery.countDown()
        first.get(2, TimeUnit.SECONDS)
        server.query("third(Result)").get(2, TimeUnit.SECONDS)

        assertTrue(cancelled.isCancelled)
        assertEquals(listOf("first(Result)", "third(Result)"), bridge.queries)
        server.close()
    }

    private class RecordingTreallaBridge : TreallaBridge {
        val initialized = mutableListOf<String>()
        val consulted = mutableListOf<String>()
        val queries = mutableListOf<String>()
        val threadNames = mutableListOf<String>()
        var shutdownCount = 0
        var failNextConsult = false

        override fun initialize(coreAssetPath: String) {
            initialized += coreAssetPath
            threadNames += Thread.currentThread().name
        }

        override fun consult(sourcePath: String) {
            consulted += sourcePath
            threadNames += Thread.currentThread().name
            if (failNextConsult) {
                failNextConsult = false
                error("source rejected")
            }
        }

        override fun evaluate(query: String): List<String> {
            queries += query
            threadNames += Thread.currentThread().name
            return listOf("bob")
        }

        override fun shutdown() {
            shutdownCount += 1
            threadNames += Thread.currentThread().name
        }
    }

    private class BlockingTreallaBridge : TreallaBridge {
        val firstQueryEntered = CountDownLatch(1)
        val releaseFirstQuery = CountDownLatch(1)
        val queries = mutableListOf<String>()

        override fun initialize(coreAssetPath: String) = Unit

        override fun consult(sourcePath: String) = Unit

        override fun evaluate(query: String): List<String> {
            queries += query
            if (query == "first(Result)") {
                firstQueryEntered.countDown()
                check(releaseFirstQuery.await(2, TimeUnit.SECONDS)) { "Timed out waiting to release first query" }
            }
            return listOf("ok")
        }

        override fun shutdown() = Unit
    }
}
