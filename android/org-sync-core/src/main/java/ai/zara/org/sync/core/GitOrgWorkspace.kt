package ai.zara.org.sync.core

import java.io.File
import java.time.Instant
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.transport.URIish

data class GitWorkspaceStatus(
    val initialized: Boolean,
    val clean: Boolean,
    val branch: String?,
    val added: Set<String> = emptySet(),
    val changed: Set<String> = emptySet(),
    val removed: Set<String> = emptySet(),
    val conflicting: Set<String> = emptySet(),
)

sealed interface GitSyncResult {
    data class Synced(val pushed: Boolean, val message: String) : GitSyncResult
    data class Conflict(val files: Set<String>, val message: String) : GitSyncResult
    data class Failed(val message: String) : GitSyncResult
}

class GitOrgWorkspace(
    private val root: File,
) {
    @Synchronized
    fun initialize(): GitWorkspaceStatus {
        root.mkdirs()
        if (!File(root, ".git").isDirectory) {
            Git.init().setDirectory(root).call().close()
        }
        return status()
    }

    @Synchronized
    fun clone(remote: String, branch: String): GitWorkspaceStatus {
        validateRemote(remote)
        require(branch.isNotBlank()) { "Git branch is required" }
        require(!root.exists() || root.listFiles().orEmpty().isEmpty()) {
            "Shared Org home is not empty; initialize it instead of cloning over existing files"
        }
        root.parentFile?.mkdirs()
        Git.cloneRepository()
            .setURI(remote)
            .setDirectory(root)
            .setBranch(branch)
            .call()
            .close()
        return status()
    }

    @Synchronized
    fun configureRemote(remote: String, branch: String) {
        validateRemote(remote)
        require(branch.isNotBlank()) { "Git branch is required" }
        open().use { git ->
            val config = git.repository.config
            config.setString("remote", "origin", "url", remote)
            config.setString("remote", "origin", "fetch", "+refs/heads/*:refs/remotes/origin/*")
            config.setString("branch", branch, "remote", "origin")
            config.setString("branch", branch, "merge", "refs/heads/$branch")
            config.save()
        }
    }

    fun status(): GitWorkspaceStatus {
        if (!File(root, ".git").isDirectory) {
            return GitWorkspaceStatus(initialized = false, clean = root.listFiles().orEmpty().isEmpty(), branch = null)
        }
        return open().use { git ->
            val raw = git.status().call()
            GitWorkspaceStatus(
                initialized = true,
                clean = raw.isClean,
                branch = git.repository.branch,
                added = raw.added + raw.untracked,
                changed = raw.changed + raw.modified,
                removed = raw.removed + raw.missing,
                conflicting = raw.conflicting,
            )
        }
    }

    @Synchronized
    fun sync(): GitSyncResult = runCatching {
        open().use { git ->
            var raw = git.status().call()
            if (raw.conflicting.isNotEmpty()) {
                return GitSyncResult.Conflict(raw.conflicting, "Resolve existing Git conflicts before syncing")
            }

            if (raw.hasUncommittedChanges()) {
                git.add().addFilepattern(".").call()
                git.add().addFilepattern(".").setUpdate(true).call()
                raw = git.status().call()
                if (raw.hasUncommittedChanges()) {
                    git.commit()
                        .setMessage("Org Sync ${Instant.now()}")
                        .setAll(true)
                        .call()
                }
            }

            val pull = git.pull().call()
            val afterPull = git.status().call()
            if (!pull.isSuccessful || afterPull.conflicting.isNotEmpty()) {
                return GitSyncResult.Conflict(
                    afterPull.conflicting,
                    "Git pull requires conflict resolution; no force reset was attempted",
                )
            }

            val updates = git.push().call().flatMap { it.remoteUpdates }.toList()
            val rejected = updates.filter { update ->
                val status = update.status.name
                status.contains("REJECT", ignoreCase = true) || status.contains("NONFASTFORWARD", ignoreCase = true)
            }
            if (rejected.isNotEmpty()) {
                GitSyncResult.Failed("Push rejected: ${rejected.joinToString { it.status.name }}")
            } else {
                GitSyncResult.Synced(pushed = updates.isNotEmpty(), message = "Git sync completed")
            }
        }
    }.getOrElse { error ->
        GitSyncResult.Failed(error.message ?: error::class.java.simpleName)
    }

    private fun open(): Git = Git.open(root)

    companion object {
        fun validateRemote(remote: String) {
            require(remote.isNotBlank()) { "Git remote is required" }
            val uri = URIish(remote)
            if (uri.scheme == "http" || uri.scheme == "https") {
                require(uri.pass == null) { "Do not embed Git passwords or tokens in the remote URL" }
            }
        }
    }
}
