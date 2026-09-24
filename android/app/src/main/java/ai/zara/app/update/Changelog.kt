package ai.zara.app.update

import android.content.Context

object Changelog {
    fun notesForVersion(markdown: String, version: String): String? {
        val lines = markdown.lineSequence().toList()
        val heading = "## $version"
        val start = lines.indexOfFirst { it.trim() == heading }
        if (start < 0) return null
        val end = (start + 1 until lines.size)
            .firstOrNull { lines[it].startsWith("## ") }
            ?: lines.size
        return normalize(lines.subList(start + 1, end)).takeIf(String::isNotBlank)
    }

    fun load(context: Context, version: String): String? =
        runCatching {
            context.assets.open("CHANGELOG.md").bufferedReader().use { reader ->
                notesForVersion(reader.readText(), version)
            }
        }.getOrNull()

    fun shouldShow(lastShownVersion: String?, version: String, notes: String?): Boolean =
        !notes.isNullOrBlank() && lastShownVersion != version

    private fun normalize(lines: List<String>): String {
        val rendered = mutableListOf<String>()
        var blank = false
        var previousWasHeading = false
        for (raw in lines) {
            val line = raw.trim()
            if (line.isEmpty()) {
                blank = rendered.isNotEmpty()
                continue
            }

            val isHeading = line.startsWith("### ")
            if (
                blank &&
                rendered.lastOrNull() != "" &&
                (isHeading || !previousWasHeading)
            ) {
                rendered += ""
            }
            blank = false

            rendered += when {
                isHeading -> line.removePrefix("### ").trim()
                line.startsWith("- ") -> "• " + line.removePrefix("- ").trim()
                else -> line
            }
            previousWasHeading = isHeading
        }
        return rendered.joinToString("\n").trim()
    }
}

class ChangelogSeenStore(context: Context) {
    private val preferences =
        context.getSharedPreferences("zara-changelog", Context.MODE_PRIVATE)

    fun shouldShow(version: String, notes: String?): Boolean =
        Changelog.shouldShow(preferences.getString(KEY, null), version, notes)

    fun markShown(version: String) {
        preferences.edit().putString(KEY, version).apply()
    }

    private companion object {
        const val KEY = "last-shown-version"
    }
}
