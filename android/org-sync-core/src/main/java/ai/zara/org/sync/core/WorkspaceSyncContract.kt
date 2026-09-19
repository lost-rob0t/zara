package ai.zara.org.sync.core

import java.io.File
import java.net.URI
import java.util.concurrent.atomic.AtomicLong

data class WorkspaceId(val value: String) {
    init {
        require(value.matches(Regex("[A-Za-z0-9][A-Za-z0-9._-]{0,63}"))) {
            "Workspace id must be 1-64 safe identifier characters"
        }
    }
}

sealed interface OrgWorkspaceDescriptor {
    val id: WorkspaceId
    val displayName: String

    data class AppPrivate(
        override val id: WorkspaceId,
        override val displayName: String,
        val rootId: String,
    ) : OrgWorkspaceDescriptor {
        init {
            require(displayName.isNotBlank()) { "Workspace name is required" }
            validateRelativeRoot(rootId)
        }
    }

    data class SafTree(
        override val id: WorkspaceId,
        override val displayName: String,
        val treeUri: String,
    ) : OrgWorkspaceDescriptor {
        init {
            require(displayName.isNotBlank()) { "Workspace name is required" }
            val parsed = URI(treeUri)
            require(parsed.scheme == "content") { "SAF workspace must use a content URI" }
            require(treeUri.isNotBlank()) { "SAF tree URI is required" }
        }
    }

    data class Git(
        override val id: WorkspaceId,
        override val displayName: String,
        val localRootId: String,
        val remote: String,
        val branch: String,
    ) : OrgWorkspaceDescriptor {
        init {
            require(displayName.isNotBlank()) { "Workspace name is required" }
            validateRelativeRoot(localRootId)
            GitOrgWorkspace.validateRemote(remote)
            require(branch.isNotBlank()) { "Git branch is required" }
        }
    }

    companion object {
        internal fun validateRelativeRoot(value: String) {
            val normalized = value.replace('\\', '/').trim('/')
            require(normalized.isNotBlank()) { "Workspace root id is required" }
            require(!value.startsWith('/')) { "Workspace root must be relative" }
            require(normalized.split('/').none { it.isBlank() || it == "." || it == ".." }) {
                "Workspace root contains an unsafe path segment"
            }
        }
    }
}

object OrgWorkspaceMapper {
    fun appPrivateRoot(filesDir: File, descriptor: OrgWorkspaceDescriptor.AppPrivate): File =
        safeChild(filesDir.resolve("org-workspaces"), descriptor.rootId)

    fun gitRoot(filesDir: File, descriptor: OrgWorkspaceDescriptor.Git): File =
        safeChild(filesDir.resolve("org-workspaces"), descriptor.localRootId)

    private fun safeChild(base: File, relativeRoot: String): File {
        OrgWorkspaceDescriptor.validateRelativeRoot(relativeRoot)
        val canonicalBase = base.canonicalFile
        val candidate = File(canonicalBase, relativeRoot).canonicalFile
        require(
            candidate.path == canonicalBase.path ||
                candidate.path.startsWith(canonicalBase.path + File.separator),
        ) { "Workspace root escapes Zara-owned storage" }
        return candidate
    }
}

class SyncGenerationFence {
    private val currentGeneration = AtomicLong(0L)

    fun begin(): SyncLease = SyncLease(this, currentGeneration.incrementAndGet())

    fun cancel(lease: SyncLease): Boolean {
        if (lease.owner !== this) return false
        return currentGeneration.compareAndSet(lease.generation, lease.generation + 1L)
    }

    internal fun isCurrent(generation: Long): Boolean =
        currentGeneration.get() == generation
}

class SyncLease internal constructor(
    internal val owner: SyncGenerationFence,
    val generation: Long,
) {
    fun isCurrent(): Boolean = owner.isCurrent(generation)
}
