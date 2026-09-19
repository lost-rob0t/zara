package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class PrologStudioTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun queryPolicyAllowsDataQueriesAndRejectsEffectfulMetaPredicates() {
        assertEquals(
            "ancestor(alice, Result)",
            PrologQueryPolicy.requireSafe(" ?- ancestor(alice, Result). "),
        )
        listOf(
            "consult('/sdcard/payload.pl')",
            "call(shell('id'))",
            "open('/data/local/tmp/x', write, S)",
            "process_create(path(sh), ['-c','id'], [])",
            "assertz(owner(root))",
            "abolish(user:goal/1)",
        ).forEach { query ->
            try {
                PrologQueryPolicy.requireSafe(query)
                throw AssertionError("unsafe query was accepted: $query")
            } catch (_: IllegalArgumentException) {
            }
        }
    }

    @Test
    fun queryPolicyRequiresBoundedResultVariable() {
        try {
            PrologQueryPolicy.requireSafe("ancestor(alice, bob)")
            throw AssertionError("query without Result was accepted")
        } catch (error: IllegalArgumentException) {
            assertTrue(error.message.orEmpty().contains("Result"))
        }
    }

    @Test
    fun analyzerBuildsFactAndRuleGraphWithLineProvenance() {
        val source = """
            parent(alice, bob).
            parent(bob, charlie).
            ancestor(X, Y) :- parent(X, Y).
            ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
        """.trimIndent()

        val document = PrologSourceAnalyzer.analyze("family.pl", source)

        assertTrue(document.diagnostics.isEmpty())
        assertEquals(4, document.clauses.size)
        assertEquals(2, document.clauses.count { it.kind == PrologClauseKind.FACT })
        assertEquals(2, document.clauses.count { it.kind == PrologClauseKind.RULE })
        assertTrue(document.graph.nodes.any { it.id == "predicate:ancestor/2" })
        assertTrue(document.graph.nodes.any { it.id == "predicate:parent/2" })
        assertTrue(document.graph.edges.any {
            it.from == "predicate:ancestor/2" && it.to == "predicate:parent/2"
        })
        assertTrue(document.graph.nodes.all { it.source == "family.pl" })
        assertTrue(document.graph.nodes.any { it.line == 4 })
    }

    @Test
    fun analyzerReportsUnterminatedAndUnbalancedSourceWithoutCrashing() {
        val document = PrologSourceAnalyzer.analyze(
            "broken.pl",
            "parent(alice, bob.\nrule(X) :- parent(X, Y)",
        )

        assertFalse(document.diagnostics.isEmpty())
        assertTrue(document.diagnostics.any { it.message.contains("Unbalanced") })
        assertTrue(document.diagnostics.any { it.message.contains("period") })
    }

    @Test
    fun workspacePersistsOnlyPrivatePlFilesAndRejectsTraversal() {
        val workspace = PrologWorkspace(temporary.newFolder("workspace"))

        workspace.saveSource("family.pl", "parent(alice, bob).\n")

        assertEquals(listOf("family.pl"), workspace.listSources().map { it.name })
        assertEquals("parent(alice, bob).\n", workspace.readSource("family.pl").text)
        listOf("../escape.pl", "/sdcard/escape.pl", "notes.txt", ".hidden.pl").forEach { name ->
            try {
                workspace.saveSource(name, "x.")
                throw AssertionError("unsafe workspace name was accepted: $name")
            } catch (_: IllegalArgumentException) {
            }
        }
    }

    @Test
    fun workspaceSeedsExamplesWithoutOverwritingUserChanges() {
        val workspace = PrologWorkspace(temporary.newFolder("examples"))

        workspace.seedExamples(PrologExampleCatalog.examples)
        val first = workspace.readSource("family.pl").text
        workspace.saveSource("family.pl", "% user version\nparent(me, prolog).\n")
        workspace.seedExamples(PrologExampleCatalog.examples)

        assertTrue(first.contains("ancestor"))
        assertEquals("% user version\nparent(me, prolog).\n", workspace.readSource("family.pl").text)
        assertTrue(workspace.listSources().size >= 3)
    }

    @Test
    fun workspaceCanRollbackANewSourceWithoutEscapingItsRoot() {
        val workspace = PrologWorkspace(temporary.newFolder("rollback"))
        workspace.saveSource("temporary.pl", "temporary(ok).\n")

        assertTrue(workspace.deleteSource("temporary.pl"))
        assertTrue(workspace.listSources().isEmpty())
    }

    @Test
    fun schemaValidatorChecksDeclaredArityAndArgumentTypes() {
        val source = """
            :- zara_schema(person, 2, [atom, integer]).
            person(alice, 42).
            person(bob, 42.5).
            person(charlie).
        """.trimIndent()

        val diagnostics = PrologSchemaValidator.validate(
            PrologSourceAnalyzer.analyze("people.pl", source),
        )

        assertEquals(2, diagnostics.size)
        assertTrue(diagnostics.any { it.message.contains("argument 2") })
        assertTrue(diagnostics.any { it.message.contains("person/1") })
    }

    @Test
    fun completionIncludesLanguageAndWorkspacePredicateSignatures() {
        val document = PrologSourceAnalyzer.analyze(
            "family.pl",
            "parent(alice, bob).\nancestor(X, Y) :- parent(X, Y).\n",
        )

        val completions = PrologCompletionEngine.complete("anc", 3, listOf(document))

        assertEquals("ancestor/2", completions.first().label)
        assertEquals("ancestor(\${1:Arg1}, \${2:Arg2})", completions.first().insertion)
    }

    @Test
    fun localEmbeddingIsVersionedDeterministicAndCanBeDisabled() {
        val enabled = LocalEmbeddingConfiguration(enabled = true)
        val disabled = enabled.copy(enabled = false)

        assertEquals("zara-token-hash-1", enabled.modelVersion)
        assertEquals(
            LocalEmbeddingModel.embed("ancestor parent", enabled),
            LocalEmbeddingModel.embed("ancestor parent", enabled),
        )
        assertEquals(96, LocalEmbeddingModel.embed("ancestor parent", enabled).size)
        assertTrue(LocalEmbeddingModel.embed("ancestor parent", disabled).isEmpty())
    }

    @Test
    fun lexerProducesStableSemanticTokens() {
        val source = ":- module(expert, []).\n% note\nrisk(Person, 0.9) :- signal(Person, \"red\")."
        val tokens = PrologLexer.lex(source)

        assertTrue(tokens.any { it.kind == PrologTokenKind.DIRECTIVE && it.text(source) == ":-" })
        assertTrue(tokens.any { it.kind == PrologTokenKind.COMMENT && it.text(source) == "% note" })
        assertTrue(tokens.any { it.kind == PrologTokenKind.VARIABLE && it.text(source) == "Person" })
        assertTrue(tokens.any { it.kind == PrologTokenKind.NUMBER && it.text(source) == "0.9" })
        assertTrue(tokens.any { it.kind == PrologTokenKind.STRING && it.text(source) == "\"red\"" })
    }

    @Test
    fun editorHistorySupportsUndoRedoAndInvalidatesRedoOnEdit() {
        var history = PrologEditorHistory.initial("fact(one).", 10)
        history = history.edit("fact(two).", 10)
        history = history.undo()
        assertEquals("fact(one).", history.current.text)
        history = history.redo()
        assertEquals("fact(two).", history.current.text)
        history = history.undo().edit("fact(three).", 12)
        assertFalse(history.canRedo)
    }

    @Test
    fun workspaceRenameAndBundleRoundTripStayInsidePrivateRoot() {
        val first = PrologWorkspace(temporary.newFolder("bundle-source"))
        first.saveSource("facts.pl", "fact(one).\n")
        first.saveSource("rules.pl", "rule(X) :- fact(X).\n")
        assertEquals("knowledge.pl", first.renameSource("facts.pl", "knowledge.pl").name)

        val bundle = first.exportBundle()
        val second = PrologWorkspace(temporary.newFolder("bundle-target"))
        second.importBundle(bundle)

        assertEquals(first.listSources(), second.listSources())
        try {
            first.renameSource("knowledge.pl", "../escape.pl")
            throw AssertionError("workspace rename escaped private root")
        } catch (_: IllegalArgumentException) {
        }
    }

    @Test
    fun searchIsBoundedAndReportsLineAndColumn() {
        val matches = PrologSearch.find(
            listOf(PrologSource("facts.pl", "fact(one).\nfact(two).\n")),
            "fact",
            limit = 1,
        )

        assertEquals(1, matches.size)
        assertEquals(1, matches.single().line)
        assertEquals(1, matches.single().column)
    }

    @Test
    fun localCommandRouterInvokesOnlyDeclaredExpertEntries() {
        val catalog = PrologWorkspaceCatalog.from(
            listOf(
                PrologSource(
                    "expert.pl",
                    "expert_activation(triage, inspect).\ntriage_decision(Entity, review).\ntriage_explain(Entity, Result) :- Result = review(Entity).\n",
                ),
            ),
        )
        assertEquals("triage_explain(alice, Result)", LocalNaturalLanguageExpertRouter.query("inspect alice", catalog))

        assertEquals(
            "triage_explain(alice, Result)",
            LocalPrologCommand.parse("/expert triage_explain alice", catalog).query,
        )
        assertEquals(
            "triage_explain(alice, Result)",
            LocalPrologCommand.parse("/triage alice", catalog).query,
        )
        assertEquals(
            "triage_explain(alice, Result)",
            LocalPrologCommand.parse("/triage_explain alice", catalog).query,
        )
        assertEquals(
            "triage_explain(alice, Result)",
            LocalPrologCommand.parse("/prolog triage_explain(alice, Result)", catalog).query,
        )
        listOf(
            "/expert missing alice",
            "/expert triage_explain '); shell(id).",
            "/expert triage_decision alice",
            "/missing alice",
            "/triage '); shell(id).",
        ).forEach { text ->
            try {
                LocalPrologCommand.parse(text, catalog)
                throw AssertionError("unsafe local expert command accepted: $text")
            } catch (_: IllegalArgumentException) {
            }
        }
    }

    @Test
    fun workspaceCatalogSeparatesFactsRulesSchemasAndExperts() {
        val catalog = PrologWorkspaceCatalog.from(
            listOf(PrologSource("expert.pl", """
                :- zara_schema(signal, 2, [atom, atom]).
                signal(alice, red).
                triage_explain(Entity, Result) :- signal(Entity, Result).
            """.trimIndent())),
        )

        assertEquals(listOf("signal/2"), catalog.facts.map { it.indicator })
        assertEquals(listOf("triage_explain/2"), catalog.rules.map { it.indicator })
        assertEquals(listOf("signal/2"), catalog.schemas.map { it.indicator })
        assertEquals(listOf("triage_explain/2"), catalog.experts.map { it.indicator })
    }

    @Test
    fun intentHelpersCompileFromActionWordsWithoutModelAuthority() {
        val request = IntentHelperRequest(
            intent = "investigate",
            actionWords = listOf("investigate", "research", "dig"),
            arguments = listOf(IntentArgument("target", "atom")),
        )

        val draft = DeterministicIntentHelperGenerator.generate(request)

        assertEquals(IntentDraftProviderKind.DETERMINISTIC, draft.provider)
        assertTrue(draft.source.contains("verb_intent(investigate, investigate, 1)."))
        assertTrue(draft.source.contains("verb_intent(research, investigate, 1)."))
        assertTrue(draft.source.contains(":- zara_schema(investigate_explain, 2, [atom, term])."))
        assertTrue(draft.requiresApproval)
        assertTrue(PrologSchemaValidator.validate(PrologSourceAnalyzer.analyze("intent_investigate.pl", draft.source)).isEmpty())
    }

    @Test
    fun remoteIntentGeneratorConfigurationIsExplicitAndBounded() {
        val configuration = IntentGeneratorConfiguration.remote(
            endpoint = "https://llm.starintel.actor/v1/intent-drafts",
            model = "intent-helper-1b",
        )

        assertEquals(IntentDraftProviderKind.REMOTE, configuration.provider)
        assertEquals("intent-helper-1b", configuration.model)
        listOf("http://plain.example/v1", "https://user:secret@example.com/v1").forEach { endpoint ->
            try {
                IntentGeneratorConfiguration.remote(endpoint, "model")
                throw AssertionError("unsafe remote intent endpoint accepted")
            } catch (_: IllegalArgumentException) {
            }
        }
    }
}
