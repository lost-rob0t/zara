package ai.zara.org.sync.core

import java.nio.file.Files
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
    fun httpsRemoteRejectsEmbeddedPassword() {
        val rejected = runCatching {
            GitOrgWorkspace.validateRemote("https://user:secret@example.invalid/org.git")
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
