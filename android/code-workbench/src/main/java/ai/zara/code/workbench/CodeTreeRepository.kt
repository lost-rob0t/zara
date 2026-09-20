package ai.zara.code.workbench

import android.content.Context
import android.content.Intent
import android.net.Uri
import androidx.documentfile.provider.DocumentFile

data class CodeFileRef(
    val name: String,
    val relativePath: String,
    val uri: Uri,
)

object CodeLanguage {
    fun fromFileName(name: String): String = when (name.substringAfterLast('.', "").lowercase()) {
        "py" -> "python"
        "pl", "pro" -> "prolog"
        "kt", "kts" -> "kotlin"
        "java" -> "java"
        "js", "mjs", "cjs" -> "javascript"
        "ts", "tsx" -> "typescript"
        "lisp", "cl", "el" -> "lisp"
        "org" -> "org"
        "sh", "bash" -> "shell"
        "json" -> "json"
        "toml" -> "toml"
        "yaml", "yml" -> "yaml"
        "md" -> "markdown"
        "c", "h", "cc", "cpp", "hpp" -> "c-cpp"
        "rs" -> "rust"
        "go" -> "go"
        "nim" -> "nim"
        "sql" -> "sql"
        "html" -> "html"
        "css" -> "css"
        else -> "text"
    }
}

class CodeTreeRepository(
    private val context: Context,
    private val treeUri: Uri,
) {
    private val root = requireNotNull(DocumentFile.fromTreeUri(context, treeUri)) {
        "Unable to open selected project tree"
    }

    fun listCodeFiles(limit: Int = 2_000): List<CodeFileRef> {
        val result = mutableListOf<CodeFileRef>()
        walk(root, "", result, limit)
        return result.sortedBy { it.relativePath.lowercase() }
    }

    fun read(file: CodeFileRef): String =
        context.contentResolver.openInputStream(file.uri)?.bufferedReader()?.use { it.readText() }
            ?: error("Unable to read ${file.relativePath}")

    fun write(file: CodeFileRef, source: String) {
        context.contentResolver.openOutputStream(file.uri, "wt")?.bufferedWriter()?.use { it.write(source) }
            ?: error("Unable to write ${file.relativePath}")
    }

    private fun walk(
        directory: DocumentFile,
        prefix: String,
        output: MutableList<CodeFileRef>,
        limit: Int,
    ) {
        if (output.size >= limit) return
        directory.listFiles().forEach { child ->
            if (output.size >= limit) return
            val name = child.name ?: return@forEach
            val path = if (prefix.isBlank()) name else "$prefix/$name"
            when {
                child.isDirectory -> walk(child, path, output, limit)
                child.isFile && isEditable(name) -> output += CodeFileRef(name, path, child.uri)
            }
        }
    }

    private fun isEditable(name: String): Boolean {
        val extension = name.substringAfterLast('.', "").lowercase()
        return extension in EDITABLE_EXTENSIONS || extension.isBlank()
    }

    companion object {
        private const val PREFS = "zara-code-workbench"
        private const val KEY_TREE = "project-tree-uri"
        private val EDITABLE_EXTENSIONS = setOf(
            "py", "pl", "pro", "kt", "kts", "java", "js", "mjs", "cjs", "ts", "tsx",
            "lisp", "cl", "el", "org", "sh", "bash", "json", "toml", "yaml", "yml", "md", "txt",
            "c", "h", "cc", "cpp", "hpp", "rs", "go", "nim", "sql", "html", "css",
        )

        fun remembered(context: Context): Uri? = context
            .getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .getString(KEY_TREE, null)
            ?.let(Uri::parse)

        fun remember(context: Context, uri: Uri) {
            context.contentResolver.takePersistableUriPermission(
                uri,
                Intent.FLAG_GRANT_READ_URI_PERMISSION or Intent.FLAG_GRANT_WRITE_URI_PERMISSION,
            )
            context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
                .edit()
                .putString(KEY_TREE, uri.toString())
                .apply()
        }
    }
}
