package ai.zara.org.sync

import ai.zara.org.sync.core.OrgWorkspaceDescriptor
import ai.zara.org.sync.core.WorkspaceId
import android.content.Context
import android.content.Intent
import android.net.Uri

sealed interface SharedWorkspaceSelection {
    data class AppPrivate(val descriptor: OrgWorkspaceDescriptor.AppPrivate) : SharedWorkspaceSelection
    data class Saf(val descriptor: OrgWorkspaceDescriptor.SafTree) : SharedWorkspaceSelection
}

object SharedWorkspaceStore {
    private const val PREFS = "org-sync"
    private const val KEY_MODE = "workspace-mode"
    private const val KEY_ROOT_ID = "workspace-root-id"
    private const val KEY_TREE_URI = "workspace-tree-uri"
    private const val MODE_APP_PRIVATE = "app_private"
    private const val MODE_SAF = "saf_tree"

    fun load(context: Context): SharedWorkspaceSelection {
        val prefs = context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
        if (prefs.getString(KEY_MODE, MODE_APP_PRIVATE) == MODE_SAF) {
            val uri = prefs.getString(KEY_TREE_URI, null)
            if (uri != null) {
                return SharedWorkspaceSelection.Saf(
                    OrgWorkspaceDescriptor.SafTree(
                        id = WorkspaceId("shared"),
                        displayName = "Shared Org",
                        treeUri = uri,
                    ),
                )
            }
        }
        val rootId = prefs.getString(KEY_ROOT_ID, "main") ?: "main"
        return SharedWorkspaceSelection.AppPrivate(
            runCatching {
                OrgWorkspaceDescriptor.AppPrivate(
                    id = WorkspaceId("shared"),
                    displayName = "Shared Org",
                    rootId = rootId,
                )
            }.getOrElse {
                OrgWorkspaceDescriptor.AppPrivate(
                    id = WorkspaceId("shared"),
                    displayName = "Shared Org",
                    rootId = "main",
                )
            },
        )
    }

    fun useAppPrivate(context: Context, rootId: String) {
        val descriptor = OrgWorkspaceDescriptor.AppPrivate(
            id = WorkspaceId("shared"),
            displayName = "Shared Org",
            rootId = rootId,
        )
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_MODE, MODE_APP_PRIVATE)
            .putString(KEY_ROOT_ID, descriptor.rootId)
            .remove(KEY_TREE_URI)
            .apply()
    }

    fun useSaf(context: Context, treeUri: Uri) {
        val descriptor = OrgWorkspaceDescriptor.SafTree(
            id = WorkspaceId("shared"),
            displayName = "Shared Org",
            treeUri = treeUri.toString(),
        )
        val flags = Intent.FLAG_GRANT_READ_URI_PERMISSION or Intent.FLAG_GRANT_WRITE_URI_PERMISSION
        context.contentResolver.takePersistableUriPermission(treeUri, flags)
        require(hasSafGrant(context, treeUri)) { "Persisted SAF grant was not retained" }
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_MODE, MODE_SAF)
            .putString(KEY_TREE_URI, descriptor.treeUri)
            .apply()
    }

    fun hasSafGrant(context: Context, treeUri: Uri): Boolean =
        context.contentResolver.persistedUriPermissions.any { grant ->
            grant.uri == treeUri && grant.isReadPermission && grant.isWritePermission
        }
}
