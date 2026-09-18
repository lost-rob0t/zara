package ai.zara.org.sync

import ai.zara.org.storage.SharedOrgHomeContract
import android.content.ContentProvider
import android.content.ContentValues
import android.database.Cursor
import android.database.MatrixCursor
import android.net.Uri
import android.os.Bundle
import android.os.ParcelFileDescriptor
import java.io.File
import java.io.FileNotFoundException

class OrgSyncProvider : ContentProvider() {
    private val root: File
        get() = requireNotNull(context).filesDir.resolve("org-workspaces/main").also { it.mkdirs() }

    override fun onCreate(): Boolean {
        root.mkdirs()
        return true
    }

    override fun query(
        uri: Uri,
        projection: Array<out String>?,
        selection: String?,
        selectionArgs: Array<out String>?,
        sortOrder: String?,
    ): Cursor {
        require(uri.authority == SharedOrgHomeContract.AUTHORITY) { "Unexpected authority" }
        require(uri.pathSegments.firstOrNull() == "files") { "Unsupported Org Sync query" }

        val cursor = MatrixCursor(
            arrayOf(
                SharedOrgHomeContract.COLUMN_PATH,
                SharedOrgHomeContract.COLUMN_NAME,
                SharedOrgHomeContract.COLUMN_URI,
            ),
        )
        root.walkTopDown()
            .filter { it.isFile && it.extension.equals("org", ignoreCase = true) }
            .sortedBy { it.relativeTo(root).invariantSeparatorsPath.lowercase() }
            .forEach { file ->
                val relativePath = file.relativeTo(root).invariantSeparatorsPath
                cursor.addRow(
                    arrayOf(
                        relativePath,
                        file.name,
                        SharedOrgHomeContract.fileUri(relativePath).toString(),
                    ),
                )
            }
        return cursor
    }

    override fun openFile(uri: Uri, mode: String): ParcelFileDescriptor {
        val relativePath = SharedOrgHomeContract.decodeFileUri(uri)
        val file = safeFile(relativePath)
        val flags = when (mode) {
            "r" -> ParcelFileDescriptor.MODE_READ_ONLY
            "w", "wt" -> ParcelFileDescriptor.MODE_WRITE_ONLY or
                ParcelFileDescriptor.MODE_CREATE or
                ParcelFileDescriptor.MODE_TRUNCATE
            "wa" -> ParcelFileDescriptor.MODE_WRITE_ONLY or
                ParcelFileDescriptor.MODE_CREATE or
                ParcelFileDescriptor.MODE_APPEND
            "rw" -> ParcelFileDescriptor.MODE_READ_WRITE or ParcelFileDescriptor.MODE_CREATE
            "rwt" -> ParcelFileDescriptor.MODE_READ_WRITE or
                ParcelFileDescriptor.MODE_CREATE or
                ParcelFileDescriptor.MODE_TRUNCATE
            else -> throw IllegalArgumentException("Unsupported mode: $mode")
        }
        if (mode != "r") file.parentFile?.mkdirs()
        if (mode == "r" && !file.isFile) throw FileNotFoundException(relativePath)
        return ParcelFileDescriptor.open(file, flags)
    }

    override fun call(method: String, arg: String?, extras: Bundle?): Bundle = when (method) {
        SharedOrgHomeContract.METHOD_APPEND_ORG -> {
            val relativePath = requireNotNull(arg) { "Relative path is required" }
            val text = requireNotNull(extras?.getString(SharedOrgHomeContract.EXTRA_TEXT)) {
                "Append text is required"
            }
            val file = safeFile(relativePath)
            file.parentFile?.mkdirs()
            if (file.exists() && file.length() > 0L) file.appendText("\n")
            file.appendText(text.trimEnd() + "\n")
            resultFor(relativePath)
        }

        SharedOrgHomeContract.METHOD_WRITE_TEXT -> {
            val relativePath = requireNotNull(arg) { "Relative path is required" }
            val text = requireNotNull(extras?.getString(SharedOrgHomeContract.EXTRA_TEXT)) {
                "Write text is required"
            }
            val file = safeFile(relativePath)
            file.parentFile?.mkdirs()
            file.writeText(text)
            resultFor(relativePath)
        }

        else -> throw IllegalArgumentException("Unsupported Org Sync method: $method")
    }

    override fun getType(uri: Uri): String =
        if (SharedOrgHomeContract.decodeFileUri(uri).endsWith(".org", ignoreCase = true)) {
            "text/org"
        } else {
            "text/plain"
        }

    override fun insert(uri: Uri, values: ContentValues?): Uri? =
        throw UnsupportedOperationException("Use typed Org Sync calls")

    override fun delete(uri: Uri, selection: String?, selectionArgs: Array<out String>?): Int =
        throw UnsupportedOperationException("Use typed Org Sync calls")

    override fun update(
        uri: Uri,
        values: ContentValues?,
        selection: String?,
        selectionArgs: Array<out String>?,
    ): Int = throw UnsupportedOperationException("Use typed Org Sync calls")

    private fun resultFor(relativePath: String): Bundle = Bundle().apply {
        putString(
            SharedOrgHomeContract.EXTRA_URI,
            SharedOrgHomeContract.fileUri(relativePath).toString(),
        )
    }

    private fun safeFile(relativePath: String): File {
        val normalized = relativePath.replace('\\', '/').removePrefix("./")
        require(normalized.isNotBlank()) { "Empty Org home path" }
        require(!normalized.startsWith('/')) { "Absolute paths are not allowed" }
        require(normalized.split('/').none { it == ".." }) { "Parent traversal is not allowed" }

        val canonicalRoot = root.canonicalFile
        val file = File(canonicalRoot, normalized).canonicalFile
        require(
            file.path == canonicalRoot.path ||
                file.path.startsWith(canonicalRoot.path + File.separator),
        ) { "Path escapes shared Org home" }
        return file
    }
}
