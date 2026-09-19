package ai.zara.org.sync

import ai.zara.org.storage.OrgTreeRepository
import ai.zara.org.storage.SharedOrgHomeContract
import ai.zara.org.sync.core.OrgWorkspaceMapper
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
    override fun onCreate(): Boolean = true

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
        when (val workspace = SharedWorkspaceStore.load(requireNotNull(context))) {
            is SharedWorkspaceSelection.AppPrivate,
            is SharedWorkspaceSelection.Git,
            -> {
                val root = localRoot(workspace)
                root.walkTopDown()
                    .filter { it.isFile && it.extension.equals("org", ignoreCase = true) }
                    .sortedBy { it.relativeTo(root).invariantSeparatorsPath.lowercase() }
                    .forEach { file ->
                        addRow(cursor, file.relativeTo(root).invariantSeparatorsPath, file.name)
                    }
            }

            is SharedWorkspaceSelection.Saf -> {
                safRepository(workspace).listOrgFiles().forEach { file ->
                    addRow(cursor, file.relativePath, file.name)
                }
            }
        }
        return cursor
    }

    override fun openFile(uri: Uri, mode: String): ParcelFileDescriptor {
        val relativePath = SharedOrgHomeContract.decodeFileUri(uri)
        return when (val workspace = SharedWorkspaceStore.load(requireNotNull(context))) {
            is SharedWorkspaceSelection.AppPrivate,
            is SharedWorkspaceSelection.Git,
            -> openLocalFile(workspace, relativePath, mode)

            is SharedWorkspaceSelection.Saf -> {
                val file = safRepository(workspace).listOrgFiles()
                    .firstOrNull { it.relativePath == relativePath }
                    ?: throw FileNotFoundException(relativePath)
                requireNotNull(requireNotNull(context).contentResolver.openFileDescriptor(file.uri, mode)) {
                    "Unable to open SAF Org file $relativePath"
                }
            }
        }
    }

    override fun call(method: String, arg: String?, extras: Bundle?): Bundle {
        val relativePath = requireNotNull(arg) { "Relative path is required" }
        val text = requireNotNull(extras?.getString(SharedOrgHomeContract.EXTRA_TEXT)) {
            "Org text is required"
        }
        when (val workspace = SharedWorkspaceStore.load(requireNotNull(context))) {
            is SharedWorkspaceSelection.AppPrivate,
            is SharedWorkspaceSelection.Git,
            -> when (method) {
                SharedOrgHomeContract.METHOD_APPEND_ORG -> {
                    val file = safeLocalFile(workspace, relativePath)
                    file.parentFile?.mkdirs()
                    if (file.exists() && file.length() > 0L) file.appendText("\n")
                    file.appendText(text.trimEnd() + "\n")
                }

                SharedOrgHomeContract.METHOD_WRITE_TEXT -> {
                    val file = safeLocalFile(workspace, relativePath)
                    file.parentFile?.mkdirs()
                    file.writeText(text)
                }

                else -> throw IllegalArgumentException("Unsupported Org Sync method: $method")
            }

            is SharedWorkspaceSelection.Saf -> when (method) {
                SharedOrgHomeContract.METHOD_APPEND_ORG ->
                    safRepository(workspace).appendAgendaCapture(text, relativePath)
                SharedOrgHomeContract.METHOD_WRITE_TEXT ->
                    safRepository(workspace).writeRelative(relativePath, text)
                else -> throw IllegalArgumentException("Unsupported Org Sync method: $method")
            }
        }
        return resultFor(relativePath)
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

    private fun addRow(cursor: MatrixCursor, relativePath: String, name: String) {
        cursor.addRow(
            arrayOf(
                relativePath,
                name,
                SharedOrgHomeContract.fileUri(relativePath).toString(),
            ),
        )
    }

    private fun safRepository(workspace: SharedWorkspaceSelection.Saf): OrgTreeRepository {
        val appContext = requireNotNull(context)
        val treeUri = Uri.parse(workspace.descriptor.treeUri)
        check(SharedWorkspaceStore.hasSafGrant(appContext, treeUri)) {
            "Shared Org SAF grant was revoked; re-select the workspace"
        }
        return OrgTreeRepository(appContext, treeUri)
    }

    private fun localRoot(workspace: SharedWorkspaceSelection): File {
        val appContext = requireNotNull(context)
        val root = when (workspace) {
            is SharedWorkspaceSelection.AppPrivate ->
                OrgWorkspaceMapper.appPrivateRoot(appContext.filesDir, workspace.descriptor)
            is SharedWorkspaceSelection.Git ->
                OrgWorkspaceMapper.gitRoot(appContext.filesDir, workspace.descriptor)
            is SharedWorkspaceSelection.Saf -> error("SAF workspaces do not have direct filesystem roots")
        }
        return root.also { it.mkdirs() }
    }

    private fun openLocalFile(
        workspace: SharedWorkspaceSelection,
        relativePath: String,
        mode: String,
    ): ParcelFileDescriptor {
        val file = safeLocalFile(workspace, relativePath)
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

    private fun resultFor(relativePath: String): Bundle = Bundle().apply {
        putString(
            SharedOrgHomeContract.EXTRA_URI,
            SharedOrgHomeContract.fileUri(relativePath).toString(),
        )
    }

    private fun safeLocalFile(
        workspace: SharedWorkspaceSelection,
        relativePath: String,
    ): File {
        val normalized = relativePath.replace('\\', '/').removePrefix("./")
        require(normalized.isNotBlank()) { "Empty Org home path" }
        require(!normalized.startsWith('/')) { "Absolute paths are not allowed" }
        require(normalized.split('/').none { it == ".." }) { "Parent traversal is not allowed" }

        val canonicalRoot = localRoot(workspace).canonicalFile
        val file = File(canonicalRoot, normalized).canonicalFile
        require(
            file.path == canonicalRoot.path ||
                file.path.startsWith(canonicalRoot.path + File.separator),
        ) { "Path escapes shared Org home" }
        return file
    }
}
