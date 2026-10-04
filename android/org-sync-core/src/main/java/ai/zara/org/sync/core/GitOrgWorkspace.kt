package ai.zara.org.sync.core

import java.io.File
import java.time.Instant
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.errors.CanceledException
import org.eclipse.jgit.lib.Constants
import org.eclipse.jgit.lib.ProgressMonitor
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.revwalk.filter.RevFilter
import org.eclipse.jgit.transport.RemoteRefUpdate
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

data class GitRevisionEvidence(
    val baseRevision: String?,
    val localRevision: String?,
    val remoteRevision: String?,
)

sealed interface GitSyncResult {
    data class Synced(val pushed: Boolean, val message: String) : GitSyncResult
    data class Conflict(
        val files: Set<String>,
        val message: String,
        val revisions: GitRevisionEvidence,
    ) : GitSyncResult
    data class Cancelled(val generation: Long?, val message: String) : GitSyncResult
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
    fun clone(remote: String, branch: String, lease: SyncLease? = null): GitWorkspaceStatus {
        validateRemote(remote)
        require(branch.isNotBlank()) { "Git branch is required" }
        require(lease == null || lease.isCurrent()) { "Git clone was cancelled before start" }
        require(!root.exists() || root.listFiles().orEmpty().isEmpty()) {
            "Shared Org home is not empty; initialize it instead of cloning over existing files"
        }
        root.parentFile?.mkdirs()
        val command = Git.cloneRepository()
            .setURI(remote)
            .setDirectory(root)
            .setBranch(branch)
        lease?.let { command.setProgressMonitor(LeaseProgressMonitor(it)) }
        command.call().use { git ->
            persistConfiguredBranch(git.repository, branch)
        }
        require(lease == null || lease.isCurrent()) { "Git clone was cancelled" }
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
            config.setString(CONFIG_SECTION, null, CONFIG_BRANCH_KEY, branch)
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
    fun sync(lease: SyncLease? = null): GitSyncResult = runCatching {
        if (lease != null && !lease.isCurrent()) {
            return GitSyncResult.Cancelled(lease.generation, "Git sync cancelled before start")
        }

        open().use { git ->
            val repository = git.repository
            val configuredBranch = configuredBranch(repository)
            val currentBranch = repository.branch
            if (configuredBranch != null && currentBranch != configuredBranch) {
                return GitSyncResult.Failed(
                    "Configured Org sync branch '$configuredBranch' is not checked out; current branch is '$currentBranch'",
                )
            }

            var raw = git.status().call()
            if (raw.conflicting.isNotEmpty()) {
                return GitSyncResult.Conflict(
                    raw.conflicting,
                    "Resolve existing Git conflicts before syncing",
                    revisionEvidence(repository),
                )
            }

            if (raw.hasUncommittedChanges()) {
                if (lease != null && !lease.isCurrent()) {
                    return GitSyncResult.Cancelled(lease.generation, "Git sync cancelled before local commit")
                }
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

            if (lease != null && !lease.isCurrent()) {
                return GitSyncResult.Cancelled(lease.generation, "Git sync cancelled before pull")
            }

            val localBeforePull = repository.resolve(Constants.HEAD)?.name
            val pullCommand = git.pull()
            configuredBranch?.let { branch ->
                pullCommand.setRemote("origin")
                pullCommand.setRemoteBranchName(branch)
            }
            lease?.let { pullCommand.setProgressMonitor(LeaseProgressMonitor(it)) }
            val pull = pullCommand.call()

            if (lease != null && !lease.isCurrent()) {
                return GitSyncResult.Cancelled(lease.generation, "Git sync cancelled during pull")
            }

            val afterPull = git.status().call()
            if (!pull.isSuccessful || afterPull.conflicting.isNotEmpty()) {
                return GitSyncResult.Conflict(
                    afterPull.conflicting,
                    "Git pull requires conflict resolution; no force reset was attempted",
                    revisionEvidence(repository, localOverride = localBeforePull),
                )
            }

            if (lease != null && !lease.isCurrent()) {
                return GitSyncResult.Cancelled(lease.generation, "Git sync cancelled before push")
            }

            val pushCommand = git.push()
            lease?.let { pushCommand.setProgressMonitor(LeaseProgressMonitor(it)) }
            val updates = pushCommand.call().flatMap { it.remoteUpdates }.toList()

            if (lease != null && !lease.isCurrent()) {
                return GitSyncResult.Cancelled(lease.generation, "Git sync cancelled during push")
            }

            val unsuccessful = updates.filterNot { update -> isSuccessfulPushStatus(update.status) }
            if (unsuccessful.isNotEmpty()) {
                GitSyncResult.Failed(
                    "Push did not complete: ${unsuccessful.joinToString { it.status.name }}",
                )
            } else {
                GitSyncResult.Synced(
                    pushed = updates.any { update -> update.status == RemoteRefUpdate.Status.OK },
                    message = "Git sync completed",
                )
            }
        }
    }.getOrElse { error ->
        if (error is CanceledException) {
            GitSyncResult.Cancelled(lease?.generation, "Git sync cancelled")
        } else {
            GitSyncResult.Failed(error.message ?: error::class.java.simpleName)
        }
    }

    private fun configuredBranch(repository: Repository): String? =
        repository.config.getString(CONFIG_SECTION, null, CONFIG_BRANCH_KEY)

    private fun persistConfiguredBranch(repository: Repository, branch: String) {
        val config = repository.config
        config.setString(CONFIG_SECTION, null, CONFIG_BRANCH_KEY, branch)
        config.save()
    }

    private fun revisionEvidence(
        repository: Repository,
        localOverride: String? = null,
    ): GitRevisionEvidence {
        val local = localOverride ?: repository.resolve(Constants.HEAD)?.name
        val branch = runCatching { repository.branch }.getOrNull()
        val remote = branch
            ?.takeUnless { it == Constants.HEAD }
            ?.let { repository.resolve("refs/remotes/origin/$it")?.name }
        return GitRevisionEvidence(
            baseRevision = mergeBase(repository, local, remote),
            localRevision = local,
            remoteRevision = remote,
        )
    }

    private fun mergeBase(repository: Repository, local: String?, remote: String?): String? {
        if (local == null || remote == null) return null
        val localObject = repository.resolve(local) ?: return null
        val remoteObject = repository.resolve(remote) ?: return null
        return RevWalk(repository).use { walk ->
            walk.revFilter = RevFilter.MERGE_BASE
            walk.markStart(walk.parseCommit(localObject))
            walk.markStart(walk.parseCommit(remoteObject))
            walk.next()?.name
        }
    }

    private fun open(): Git = Git.open(root)

    companion object {
        private const val CONFIG_SECTION = "zara-org-sync"
        private const val CONFIG_BRANCH_KEY = "branch"

        internal fun isSuccessfulPushStatus(status: RemoteRefUpdate.Status): Boolean =
            status == RemoteRefUpdate.Status.OK || status == RemoteRefUpdate.Status.UP_TO_DATE

        fun validateRemote(remote: String) {
            require(remote.isNotBlank()) { "Git remote is required" }
            val uri = URIish(remote)
            require(uri.pass == null) { "Do not embed Git passwords or tokens in the remote URL" }
        }
    }
}

private class LeaseProgressMonitor(
    private val lease: SyncLease,
) : ProgressMonitor {
    override fun start(totalTasks: Int) = Unit

    override fun beginTask(title: String, totalWork: Int) = Unit

    override fun update(completed: Int) = Unit

    override fun endTask() = Unit

    override fun isCancelled(): Boolean = !lease.isCurrent()

    override fun showDuration(enabled: Boolean) = Unit
}
