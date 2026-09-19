package ai.zara.org.sync.core

import java.nio.file.Files
import org.eclipse.jgit.transport.RemoteRefUpdate
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class GitOrgWorkspaceTest {
    @Test
    fun initializeCreatesRepositoryAndReportsClean() {
        val root = Files.createTempDirectory("zara-org-sync").toFile()
        val workspace = GitOrgWorkspace(root)

        val status = workspace.initialize()

        assertTrue(status.initialized)
        assertTrue(status.clean)
    }

    @Test
    fun cancelledLeaseNeverTouchesRepository() {
        val root = Files.createTempDirectory("zara-org-sync-cancel").toFile()
        val fence = SyncGenerationFence()
        val lease = fence.begin()
        assertTrue(fence.cancel(lease))

        val result = GitOrgWorkspace(root).sync(lease)

        assertTrue(result is GitSyncResult.Cancelled)
        assertFalse(root.resolve(".git").exists())
    }

    @Test
    fun syncFailsClosedWhenConfiguredBranchIsNotCheckedOut() {
        val root = Files.createTempDirectory("zara-org-sync-branch").toFile()
        val workspace = GitOrgWorkspace(root)
        workspace.initialize()
        workspace.configureRemote("file:///definitely-missing/zara-org-sync.git", "notes")

        val result = workspace.sync()

        assertTrue(result is GitSyncResult.Failed)
        assertTrue((result as GitSyncResult.Failed).message.contains("Configured Org sync branch 'notes' is not checked out"))
    }

    @Test
    fun pushStatusClassificationFailsClosed() {
        assertTrue(GitOrgWorkspace.isSuccessfulPushStatus(RemoteRefUpdate.Status.OK))
        assertTrue(GitOrgWorkspace.isSuccessfulPushStatus(RemoteRefUpdate.Status.UP_TO_DATE))
        assertFalse(GitOrgWorkspace.isSuccessfulPushStatus(RemoteRefUpdate.Status.NOT_ATTEMPTED))
        assertFalse(GitOrgWorkspace.isSuccessfulPushStatus(RemoteRefUpdate.Status.REJECTED_OTHER_REASON))
    }

    @Test
    fun httpsRemoteRejectsEmbeddedPassword() {
        val rejected = runCatching {
            GitOrgWorkspace.validateRemote("https://user:secret@example.invalid/org.git")
        }.isFailure

        assertTrue(rejected)
    }

    @Test
    fun sshRemoteRejectsEmbeddedPassword() {
        val rejected = runCatching {
            GitOrgWorkspace.validateRemote("ssh://git:secret@example.invalid/org.git")
        }.isFailure

        assertTrue(rejected)
    }

    @Test
    fun sshRemoteMayCarryUsernameButNotSecretInDescriptor() {
        val accepted = runCatching {
            GitOrgWorkspace.validateRemote("ssh://git@example.invalid/org.git")
        }.isSuccess

        assertTrue(accepted)
        assertFalse("ssh://git@example.invalid/org.git".contains("secret"))
    }
}
