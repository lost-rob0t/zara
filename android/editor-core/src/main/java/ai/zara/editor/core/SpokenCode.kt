package ai.zara.editor.core

sealed interface SpokenCodeResolution {
    data class Intent(val intent: VoiceCodeIntent) : SpokenCodeResolution
    data class NeedsModel(val instruction: String) : SpokenCodeResolution
    data class Rejected(val reason: String) : SpokenCodeResolution
}

/**
 * Fast local speech router. Simple editor commands never need an LLM; generative
 * requests are explicitly handed to the Zara model bridge and stay revision-fenced.
 */
object SpokenCodeRouter {
    private val lineCommand = Regex("(?i)^go to line\\s+(\\d+)\\s*$")
    private val findCommand = Regex("(?i)^find\\s+(.+)$")
    private val replaceSelection = Regex("(?i)^replace selection with\\s+(.+)$")
    private val insertCommand = Regex("(?i)^(?:insert|type|code)\\s+(.+)$")
    private val generative = Regex("(?i)^(?:create|write|refactor|fix|implement|generate|extract|convert)\\b.*")

    fun resolve(transcript: String, languageId: String): SpokenCodeResolution {
        val spoken = transcript.trim()
        if (spoken.isBlank()) return SpokenCodeResolution.Rejected("empty transcript")

        if (spoken.equals("undo", ignoreCase = true)) return SpokenCodeResolution.Intent(VoiceCodeIntent.Undo)
        if (spoken.equals("redo", ignoreCase = true)) return SpokenCodeResolution.Intent(VoiceCodeIntent.Redo)
        if (spoken.equals("run", ignoreCase = true) || spoken.equals("run tests", ignoreCase = true)) {
            return SpokenCodeResolution.Intent(VoiceCodeIntent.Run(if (spoken.contains("tests", true)) "tests" else "default"))
        }
        if (spoken.equals("explain selection", ignoreCase = true)) {
            return SpokenCodeResolution.Intent(VoiceCodeIntent.ExplainSelection())
        }

        lineCommand.matchEntire(spoken)?.let {
            return SpokenCodeResolution.Intent(VoiceCodeIntent.GoToLine(it.groupValues[1].toInt()))
        }
        findCommand.matchEntire(spoken)?.let {
            return SpokenCodeResolution.Intent(VoiceCodeIntent.Find(it.groupValues[1].trim()))
        }
        replaceSelection.matchEntire(spoken)?.let {
            return SpokenCodeResolution.Intent(
                VoiceCodeIntent.ReplaceSelection(CodeSpeechNormalizer.normalize(it.groupValues[1], languageId)),
            )
        }
        insertCommand.matchEntire(spoken)?.let {
            return SpokenCodeResolution.Intent(
                VoiceCodeIntent.Insert(CodeSpeechNormalizer.normalize(it.groupValues[1], languageId)),
            )
        }
        if (generative.matches(spoken)) return SpokenCodeResolution.NeedsModel(spoken)

        return SpokenCodeResolution.Intent(VoiceCodeIntent.Insert(CodeSpeechNormalizer.normalize(spoken, languageId)))
    }
}

object CodeSpeechNormalizer {
    private val indentedNewline = Regex(
        "(?i)\\bnew line\\b((?:[ \\t]+\\btab\\b)+)[ \\t]*",
    )
    private val tabToken = Regex("(?i)\\btab\\b")
    private val common = listOf(
        Regex("(?i)\\bnew line\\b") to "\n",
        tabToken to "    ",
        Regex("(?i)\\bopen paren(?:thesis)?\\b") to "(",
        Regex("(?i)\\bclose paren(?:thesis)?\\b") to ")",
        Regex("(?i)\\bopen bracket\\b") to "[",
        Regex("(?i)\\bclose bracket\\b") to "]",
        Regex("(?i)\\bopen brace\\b") to "{",
        Regex("(?i)\\bclose brace\\b") to "}",
        Regex("(?i)\\bcolon\\b") to ":",
        Regex("(?i)\\bsemicolon\\b") to ";",
        Regex("(?i)\\bcomma\\b") to ",",
        Regex("(?i)\\bdot\\b") to ".",
        Regex("(?i)\\bequals\\b") to "=",
        Regex("(?i)\\bplus\\b") to "+",
        Regex("(?i)\\bminus\\b") to "-",
        Regex("(?i)\\bstar\\b") to "*",
        Regex("(?i)\\bslash\\b") to "/",
        Regex("(?i)\\bdouble quote\\b") to "\"",
        Regex("(?i)\\bsingle quote\\b") to "'",
    )

    fun normalize(spoken: String, languageId: String): String {
        var text = indentedNewline.replace(spoken) { match ->
            val tabs = tabToken.findAll(match.groupValues[1]).count()
            "\n" + "    ".repeat(tabs)
        }
        common.forEach { (pattern, replacement) -> text = pattern.replace(text, replacement) }
        text = when (languageId.lowercase()) {
            "python", "py" -> text
                .replace(Regex("(?i)\\bdef\\s+"), "def ")
                .replace(Regex("(?i)\\btrue\\b"), "True")
                .replace(Regex("(?i)\\bfalse\\b"), "False")
                .replace(Regex("(?i)\\bnone\\b"), "None")
            "prolog", "pl" -> text
                .replace(Regex("(?i)\\bif\\s+"), ":- ")
                .replace(Regex("(?i)\\bperiod\\b"), ".")
                .replace(Regex(",[ \\t]+"), ",")
            else -> text
        }
        return cleanupAroundPunctuation(text)
    }

    private fun cleanupAroundPunctuation(text: String): String = text
        .replace(Regex("[ \\t]+([),;:\\]}.])"), "$1")
        .replace(Regex("([({\\[]) +"), "$1")
        .replace(Regex("([A-Za-z0-9_]) +([({\\[])"), "$1$2")
        .replace(Regex("\"[ \\t]+"), "\"")
        .replace(Regex("[ \\t]+\""), "\"")
        .replace(Regex("'[ \\t]+"), "'")
        .replace(Regex("[ \\t]+'"), "'")
        .replace(Regex("[ \\t]*\\n"), "\n")
}
