package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class PrologAuthoringTest {
    @Test
    fun typedSchemaBuilderCarriesArgumentNamesTypesAndProvenance() {
        val arguments = PrologFormBuilder.parseArguments("entity:atom, confidence:number")
        val source = PrologFormBuilder.schema("risk", arguments) +
            PrologFormBuilder.fact("risk", listOf("alice", "0.9"))
        val document = PrologSourceAnalyzer.analyze("risk.pl", source)

        assertTrue(PrologSchemaValidator.validate(document).isEmpty())
        val signature = PrologSignatureCatalog.from(listOf(document))[PredicateRef("risk", 2)]!!
        assertEquals(listOf("entity", "confidence"), signature.arguments.map { it.name })
        assertEquals(listOf("atom", "number"), signature.arguments.map { it.type })
        assertEquals("risk.pl", signature.source)
        assertTrue(signature.detail.contains("entity:atom"))
        assertTrue(signature.detail.contains("risk.pl:"))

        val completion = PrologCompletionEngine.complete("ri", 2, listOf(document)).first()
        assertEquals("risk/2", completion.label)
        assertTrue(completion.insertion.contains("Entity"))
        assertTrue(completion.insertion.contains("Confidence"))
        assertTrue(completion.detail.contains("entity:atom"))
        assertTrue(completion.detail.contains("confidence:number"))
        assertTrue(completion.detail.contains("risk.pl:"))
    }

    @Test
    fun factBuilderAcceptsDataTermsButRejectsClauseInjection() {
        assertEquals(
            "signal(alice, [red, amber], 0.9).\n",
            PrologFormBuilder.fact("signal", listOf("alice", "[red, amber]", "0.9")),
        )
        try {
            PrologFormBuilder.fact("signal", listOf("alice).", "shell('id')"))
            throw AssertionError("fact builder accepted clause injection")
        } catch (_: IllegalArgumentException) {
        }
    }

    @Test
    fun ruleBuilderEmitsEditablePurePrologAndRejectsAuthorityEscalation() {
        val arguments = PrologFormBuilder.parseArguments("entity:atom, result:term")
        val source = PrologFormBuilder.rule(
            "triage_explain",
            arguments,
            "signal(Entity, red), Result = review(Entity)",
        )

        assertTrue(source.startsWith("triage_explain(Entity, Result) :-"))
        assertTrue(PrologSourceAnalyzer.analyze("rule.pl", source).diagnostics.isEmpty())

        try {
            PrologFormBuilder.rule("triage_explain", arguments, "shell('id'), Result = ok")
            throw AssertionError("rule builder accepted effectful predicate")
        } catch (_: IllegalArgumentException) {
        }
    }

    @Test
    fun signaturesFallBackToTermArgumentsAndDefinitionLocation() {
        val document = PrologSourceAnalyzer.analyze(
            "plain.pl",
            "parent(alice, bob).\nancestor(X, Y) :- parent(X, Y).\n",
        )
        val signature = PrologSignatureCatalog.from(listOf(document))[PredicateRef("ancestor", 2)]!!

        assertEquals(listOf("arg1", "arg2"), signature.arguments.map { it.name })
        assertEquals(listOf("term", "term"), signature.arguments.map { it.type })
        assertEquals(2, signature.line)
    }

    @Test
    fun malformedSchemaNeverCrashesSignatureDiscovery() {
        val document = PrologSourceAnalyzer.analyze(
            "bad.pl",
            ":- zara_schema(bad, 1, [mystery]).\nbad(value).\n",
        )

        val signatures = PrologSignatureCatalog.from(listOf(document))

        assertEquals(listOf("term"), signatures[PredicateRef("bad", 1)]!!.arguments.map { it.type })
    }
}
