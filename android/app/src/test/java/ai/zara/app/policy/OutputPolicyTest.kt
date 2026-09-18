package ai.zara.app.policy

import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.prolog.TreallaBridge
import ai.zara.app.runtime.LocalZaraServer
import ai.zara.app.runtime.LocalServerPhase
import java.nio.file.Files
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import org.junit.Test

class OutputPolicyTest {
    @Test fun cleanAnswerUsesOneModelCall() {
        var calls = 0
        val review = PolicyReview(
            generate = { calls++; CompletableFuture.completedFuture("answer") },
            inspect = { CompletableFuture.completedFuture(PolicyAdvice("", 1)) },
        )
        check(review.answer("question").join() == "answer" && calls == 1)
    }

    @Test fun repairIsBoundedAndOnlyAdviceEntersRevision() {
        val prompts = mutableListOf<String>()
        var outcome: PolicyOutcome? = null
        val review = PolicyReview(
            generate = { prompts += it; CompletableFuture.completedFuture("All tests pass.") },
            inspect = { CompletableFuture.completedFuture(PolicyAdvice("Cite observed test evidence.", 1)) },
            observe = { outcome = it },
        )
        check(review.answer("question").join() == "All tests pass.")
        check(prompts.size == 2 && prompts[1].contains("Cite observed test evidence."))
        check(outcome == PolicyOutcome.UNRESOLVED)
    }

    @Test fun cancellationStopsFurtherWork() {
        val pending = CompletableFuture<String>()
        val result = PolicyReview(
            generate = { pending }, inspect = { error("Cancelled generation must not be inspected") },
        ).answer("question")
        check(result.cancel(true) && pending.isCancelled)
        val inspection = CompletableFuture<PolicyAdvice>()
        val duringReview = PolicyReview(
            generate = { CompletableFuture.completedFuture("answer") }, inspect = { inspection },
        ).answer("question")
        check(duringReview.cancel(true) && inspection.isCancelled)
    }

    @Test fun reloadRejectsStaleRevision() {
        var reads = 0
        var outcome: PolicyOutcome? = null
        val review = PolicyReview(
            generate = { CompletableFuture.completedFuture(if (reads == 0) "original" else "revision") },
            inspect = { reads++; CompletableFuture.completedFuture(PolicyAdvice("Review.", reads.toLong())) },
            observe = { outcome = it },
        )
        check(review.answer("question").join() == "original" && outcome == PolicyOutcome.STALE)
    }

    @Test fun failedInspectionAndDisabledPolicyDoNotLoop() {
        var calls = 0
        val unavailable = PolicyReview(
            generate = { calls++; CompletableFuture.completedFuture("original") },
            inspect = { throw IllegalStateException("native failure") },
        )
        check(unavailable.answer("question").join() == "original" && calls == 1)
        val disabled = PolicyReview(
            generate = { calls++; CompletableFuture.completedFuture("original") },
            inspect = { CompletableFuture.completedFuture(PolicyAdvice("", 1, false)) },
        )
        check(disabled.answer("question").join() == "original" && calls == 2)
    }

    @Test fun wireNeverInterpolatesModelTextAsCode() {
        check(PolicyWire.query("'), halt. 😀").matches(Regex("zara_policy:advise_codes\\(\\[[0-9,]*], Result\\)")))
        check(PolicyWire.decode(listOf("[1,72,105,128512]"), 3).guidance == "Hi😀")
        check(!PolicyWire.decode(listOf("[0]"), 3).enabled)
        for (bad in listOf("[0,65]", "[1,halt]", "[1,55296]", "[1,-1]", "[2]", "[]", "[1,1]")) {
            check(runCatching { PolicyWire.decode(listOf(bad), 3) }.isFailure)
        }
        check(runCatching { PolicyWire.query("x".repeat(32769)) }.isFailure)
        check(runCatching { PolicyWire.query("\uD800") }.isFailure)
    }

    @Test fun packagedAssetsAndConfigPreserveOperatorEdits() {
        val root = Files.createTempDirectory("policy-assets-").toFile()
        try {
            val entry = PolicyAssets.stage(root) { name -> ("source " + name).toByteArray() }
            check(entry.name == "policy.pl" && root.resolve("defaults.pl").isFile)
            val config = root.resolve("config.pl")
            PolicyAssets.seedConfig(config)
            check(config.readText().contains("zara_policy:option(mode, advice)"))
            config.writeText("operator-owned")
            PolicyAssets.seedConfig(config)
            check(config.readText() == "operator-owned")
            check(runCatching { PolicyAssets.stage(root) { ByteArray(65537) } }.isFailure)
        } finally { root.deleteRecursively() }
    }

    @Test fun policyUsesExistingActorAndReloadGeneration() {
        val root = Files.createTempDirectory("policy-server-").toFile()
        val events = mutableListOf<String>()
        val threads = mutableSetOf<String>()
        val bridge = object : TreallaBridge {
            override fun initialize(coreAssetPath: String) { events += "core"; threads += Thread.currentThread().name }
            override fun consult(sourcePath: String) { events += java.io.File(sourcePath).name; threads += Thread.currentThread().name }
            override fun evaluate(query: String): List<String> {
                check(query.matches(Regex("zara_policy:advise_codes\\(\\[[0-9,]*], Result\\)")))
                threads += Thread.currentThread().name
                return listOf("[1,72,105]")
            }
            override fun shutdown() { threads += Thread.currentThread().name }
        }
        val workspace = PrologWorkspace(root.resolve("workspace"))
        workspace.saveSource("policy-config.pl", ":- multifile zara_policy:option/2.\n")
        val server = LocalZaraServer(bridge, "core.pl", workspace, "policy.pl")
        try {
            check(server.start().get(2, TimeUnit.SECONDS).phase == LocalServerPhase.READY)
            check(events == listOf("core", "policy.pl", "policy-config.pl"))
            val report = server.inspectPolicy("'), halt. 😀").get(2, TimeUnit.SECONDS)
            check(report.guidance == "Hi" && report.generation == 1L)
            check(server.reload().get(2, TimeUnit.SECONDS).generation == 2L)
            check(server.inspectPolicy("answer").get(2, TimeUnit.SECONDS).generation == 2L)
            check(threads == setOf("zara-local-server"))
        } finally { server.close(); root.deleteRecursively() }
        check(runCatching { server.inspectPolicy("answer").join() }.isFailure)
    }
}
