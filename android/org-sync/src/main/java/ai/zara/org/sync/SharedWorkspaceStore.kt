package ai.zara.org.sync

import ai.zara.org.sync.core.OrgWorkspaceDescriptor
import ai.zara.org.sync.core.WorkspaceId
import android.content.Context
import android.content.Intent
import android.net.Uri

sealed interface SharedWorkspaceSelection {
    data class AppPrivate(val descriptor: OrgWorkspaceDescriptor.AppPrivate) : SharedWorkspaceSelection
    data class Saf(val descriptor: OrgWorkspaceDescriptor.SafTree) : SharedWorkspaceSelection
    data class Git(val descriptor: OrgWorkspaceDescriptor.Git) : SharedWorkspaceSelection
}

internal data class StoredWorkspaceSelection(
    val mode: String?,
    val rootId: String?,
    val treeUri: String?,
    val remote: String?,
    val branch: String?,
)

internal object SharedWorkspaceSelectionCodec {
    private const val MODE_APP_PRIVATE = "app_private"
    private const val MODE_SAF = "saf_tree"
    private const val MODE_GIT = "git_workspace"

    fun decode(stored: StoredWorkspaceSelection): SharedWorkspaceSelection =
        when (stored.mode ?: MODE_APP_PRIVATE) {
            MODE_APP_PRIVATE -> SharedWorkspaceSelection.AppPrivate(
                OrgWorkspaceDescriptor.AppPrivate(
                    id = WorkspaceId("shared"),
                    displayName = "Shared Org",
                    rootId = stored.rootId ?: "main",
                ),
            )

            MODE_SAF -> SharedWorkspaceSelection.Saf(
                OrgWorkspaceDescriptor.SafTree(
                    id = WorkspaceId("shared"),
                    displayName = "Shared Org",
                    treeUri = requireNotNull(stored.treeUri) {
                        "Persisted SAF workspace is missing its tree URI"
                    },
                ),
            )

            MODE_GIT -> SharedWorkspaceSelection.Git(
                OrgWorkspaceDescriptor.Git(
                    id = WorkspaceId("shared"),
                    displayName = "Shared Org",
                    localRootId = requireNotNull(stored.rootId) {
                        "Persisted Git workspace is missing its local root"
                    },
                    remote = requireNotNull(stored.remote) {
                        "Persisted Git workspace is missing its remote"
                    },
                    branch = requireNotNull(stored.branch) {
                        "Persisted Git workspace is missing its branch"
                    },
                ),
            )

            else -> error("Unknown persisted Org workspace mode: ${stored.mode}")
        }
}

object SharedWorkspaceStore {
    private const val PREFS = "org-sync"
    private const val KEY_MODE = "workspace-mode"
    private const val KEY_ROOT_ID = "workspace-root-id"
    private const val KEY_TREE_URI = "workspace-tree-uri"
    private const val KEY_REMOTE = "remote"
    private const val KEY_BRANCH = "branch"
    private const val MODE_APP_PRIVATE = "app_private"
    private const val MODE_SAF = "saf_tree"
    private const val MODE_GIT = "git_workspace"

    fun load(context: Context): SharedWorkspaceSelection {
        val prefs = context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
        return SharedWorkspaceSelectionCodec.decode(
            StoredWorkspaceSelection(
                mode = prefs.getString(KEY_MODE, MODE_APP_PRIVATE),
                rootId = prefs.getString(KEY_ROOT_ID, null),
                treeUri = prefs.getString(KEY_TREE_URI, null),
                remote = prefs.getString(KEY_REMOTE, null),
                branch = prefs.getString(KEY_BRANCH, null),
            ),
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
            .remove(KEY_REMOTE)
            .remove(KEY_BRANCH)
            .apply()
    }

    fun useGit(context: Context, localRootId: String, remote: String, branch: String) {
        val descriptor = OrgWorkspaceDescriptor.Git(
            id = WorkspaceId("shared"),
            displayName = "Shared Org",
            localRootId = localRootId,
            remote = remote,
            branch = branch,
        )
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_MODE, MODE_GIT)
            .putString(KEY_ROOT_ID, descriptor.localRootId)
            .putString(KEY_REMOTE, descriptor.remote)
            .putString(KEY_BRANCH, descriptor.branch)
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
            .remove(KEY_ROOT_ID)
            .remove(KEY_REMOTE)
            .remove(KEY_BRANCH)
            .apply()
    }

    fun hasSafGrant(context: Context, treeUri: Uri): Boolean =
        context.contentResolver.persistedUriPermissions.any { grant ->
            grant.uri == treeUri && grant.isReadPermission && grant.isWritePermission
        }
}
