package ai.zara.editor.core

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class SpokenCodeTest {
    @Test
    fun localNavigationCommandDoesNotNeedModel() {
        assertEquals(
            SpokenCodeResolution.Intent(VoiceCodeIntent.GoToLine(42)),
            SpokenCodeRouter.resolve("go to line 42", "python"),
        )
    }

    @Test
    fun spokenPythonTokensBecomeCode() {
        val result = SpokenCodeRouter.resolve(
            "insert def hello open paren close paren colon new line tab print open paren double quote hi double quote close paren",
            "python",
        ) as SpokenCodeResolution.Intent

        assertEquals(
            VoiceCodeIntent.Insert("def hello():\n    print(\"hi\")"),
            result.intent,
        )
    }

    @Test
    fun selectionReplacementIsTypedIntent() {
        val result = SpokenCodeRouter.resolve("replace selection with true", "python")
        assertEquals(
            SpokenCodeResolution.Intent(VoiceCodeIntent.ReplaceSelection("True")),
            result,
        )
    }

    @Test
    fun generativeInstructionIsNeverPretendedToBeLocalDictation() {
        val result = SpokenCodeRouter.resolve("create a parser for org headings", "python")
        assertTrue(result is SpokenCodeResolution.NeedsModel)
    }

    @Test
    fun prologPeriodAndRuleOperatorAreNormalized() {
        val result = SpokenCodeRouter.resolve(
            "insert parent open paren X comma Y close paren if mother open paren X comma Y close paren period",
            "prolog",
        ) as SpokenCodeResolution.Intent

        assertEquals(
            VoiceCodeIntent.Insert("parent(X,Y):- mother(X,Y)."),
            result.intent,
        )
    }
}
