package ai.zara.org.storage

import ai.zara.org.core.OrgParser
import ai.zara.org.core.OrgTask
import ai.zara.org.core.OrgTangler
import ai.zara.org.core.cycleTodoState
import android.content.ContentResolver
import android.content.Context
import android.net.Uri
import androidx.documentfile.provider.DocumentFile

data class OrgFileRef(
    val name: String,
    val relativePath: String,
    val uri: Uri,
)

class OrgTreeRepository(
    private val context: Context,
    private val treeUri: Uri,
    private val fallbackTodoStates: List<String> = OrgParser.defaultTodoStates,
) : OrgRepository {
    private val resolver: ContentResolver = context.contentResolver
    private val root: DocumentFile = requireNotNull(DocumentFile.fromTreeUri(context, treeUri)) {
        "Unable to open Org tree"
    }

    override fun listOrgFiles(): List<OrgFileRef> {
        val result = mutableListOf<OrgFileRef>()
        walk(root, "", result)
        return result.sortedBy { it.relativePath.lowercase() }
    }

    override fun read(file: OrgFileRef): String =
        resolver.openInputStream(file.uri)?.bufferedReader()?.use { it.readText() }
            ?: error("Unable to read ${file.relativePath}")

    override fun readRelative(relativePath: String): String? {
        val target = findFile(relativePath) ?: return null
        return resolver.openInputStream(target.uri)?.bufferedReader()?.use { it.readText() }
            ?: error("Unable to read $relativePath")
    }

    override fun write(file: OrgFileRef, text: String) {
        resolver.openOutputStream(file.uri, "wt")?.bufferedWriter()?.use { it.write(text) }
            ?: error("Unable to write ${file.relativePath}")
    }

    override fun writeRelative(relativePath: String, text: String): OrgFileRef {
        val target = ensureFile(relativePath, "text/org")
        resolver.openOutputStream(target.uri, "wt")?.bufferedWriter()?.use { it.write(text) }
            ?: error("Unable to write $relativePath")
        return OrgFileRef(
            name = target.name ?: relativePath.substringAfterLast('/'),
            relativePath = relativePath,
            uri = target.uri,
        )
    }

    override fun appendAgendaCapture(text: String, relativePath: String): OrgFileRef {
        val target = ensureFile(relativePath, "text/org")
        resolver.openOutputStream(target.uri, "wa")?.bufferedWriter()?.use { writer ->
            if (target.length() > 0) writer.append('\n')
            writer.append(text.trimEnd()).append('\n')
        } ?: error("Unable to append $relativePath")
        return OrgFileRef(target.name ?: relativePath.substringAfterLast('/'), relativePath, target.uri)
    }

    override fun allTasks(): List<OrgTask> = listOrgFiles().flatMap { file ->
        OrgParser.parse(read(file), file.relativePath, fallbackTodoStates).tasks
    }

    override fun cycleTodo(task: OrgTask): OrgTask {
        val file = listOrgFiles().firstOrNull { it.relativePath == task.path }
            ?: error("Missing task file ${task.path}")
        val source = read(file)
        val mutation = OrgParser.cycleTodoState(source, task, fallbackTodoStates)
        write(file, mutation.source)
        return task.copy(state = mutation.state)
    }

    override fun tangle(file: OrgFileRef): List<OrgFileRef> {
        val result = OrgTangler.tangle(read(file), file.relativePath)
        return result.outputs.map { output ->
            val target = ensureFile(output.path, mimeType(output.language))
            resolver.openOutputStream(target.uri, "wt")?.bufferedWriter()?.use { it.write(output.content) }
                ?: error("Unable to write tangled ${output.path}")
            OrgFileRef(target.name ?: output.path.substringAfterLast('/'), output.path, target.uri)
        }
    }

    private fun walk(directory: DocumentFile, prefix: String, sink: MutableList<OrgFileRef>) {
        directory.listFiles().forEach { child ->
            val name = child.name ?: return@forEach
            val path = if (prefix.isBlank()) name else "$prefix/$name"
            when {
                child.isDirectory -> walk(child, path, sink)
                child.isFile && name.endsWith(".org", ignoreCase = true) ->
                    sink += OrgFileRef(name, path, child.uri)
            }
        }
    }

    private fun ensureFile(relativePath: String, mimeType: String): DocumentFile {
        val normalized = normalize(relativePath)
        val pieces = normalized.split('/').filter { it.isNotBlank() }

        var directory = root
        pieces.dropLast(1).forEach { segment ->
            directory = directory.findFile(segment)?.takeIf { it.isDirectory }
                ?: directory.createDirectory(segment)
                ?: error("Unable to create directory $segment")
        }

        val leaf = pieces.last()
        return directory.findFile(leaf)
            ?: directory.createFile(mimeType, leaf)
            ?: error("Unable to create $normalized")
    }

    private fun findFile(relativePath: String): DocumentFile? {
        val pieces = normalize(relativePath).split('/').filter { it.isNotBlank() }
        var current = root
        pieces.forEach { segment ->
            current = current.findFile(segment) ?: return null
        }
        return current.takeIf { it.isFile }
    }

    private fun normalize(relativePath: String): String {
        val normalized = relativePath.replace('\\', '/').removePrefix("./")
        require(normalized.isNotBlank()) { "Empty relative path" }
        require(!normalized.startsWith('/')) { "Absolute paths are not allowed" }
        require(normalized.split('/').none { it == ".." }) { "Parent traversal is not allowed" }
        return normalized
    }

    private fun mimeType(language: String): String = when (language) {
        "python" -> "text/x-python"
        "prolog" -> "text/x-prolog"
        else -> "text/plain"
    }
}

object OrgTreePermission {
    private const val PREFS = "zara-org-workspace"
    private const val KEY_URI = "tree-uri"

    fun remember(context: Context, uri: Uri) {
        val flags = android.content.Intent.FLAG_GRANT_READ_URI_PERMISSION or
            android.content.Intent.FLAG_GRANT_WRITE_URI_PERMISSION

        context.contentResolver.takePersistableUriPermission(uri, flags)
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_URI, uri.toString())
            .apply()
    }

    fun remembered(context: Context): Uri? =
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .getString(KEY_URI, null)
            ?.let(Uri::parse)
}
