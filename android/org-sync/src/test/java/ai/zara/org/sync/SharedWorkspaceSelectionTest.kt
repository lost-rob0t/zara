package ai.zara.org.sync

import ai.zara.org.sync.core.OrgWorkspaceDescriptor
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class SharedWorkspaceSelectionTest {
    @Test
    fun gitSelectionPreservesArbitraryConfiguredRootAndRemote() {
        val selection = SharedWorkspaceSelectionCodec.decode(
            StoredWorkspaceSelection(
                mode = "git_workspace",
                rootId = "repos/personal/org-notes",
                treeUri = null,
                remote = "ssh://git@example.invalid/notes.git",
                branch = "work/org",
            ),
        )

        assertTrue(selection is SharedWorkspaceSelection.Git)
        val descriptor = (selection as SharedWorkspaceSelection.Git).descriptor
        assertEquals("repos/personal/org-notes", descriptor.localRootId)
        assertEquals("ssh://git@example.invalid/notes.git", descriptor.remote)
        assertEquals("work/org", descriptor.branch)
    }

    @Test
    fun malformedPersistedGitSelectionFailsClosed() {
        val result = runCatching {
            SharedWorkspaceSelectionCodec.decode(
                StoredWorkspaceSelection(
                    mode = "git_workspace",
                    rootId = "repos/org",
                    treeUri = null,
                    remote = null,
                    branch = "main",
                ),
            )
        }

        assertTrue(result.isFailure)
    }

    @Test
    fun appPrivateSelectionStillUsesConfiguredLogicalRoot() {
        val selection = SharedWorkspaceSelectionCodec.decode(
            StoredWorkspaceSelection(
                mode = "app_private",
                rootId = "custom/nested/root",
                treeUri = null,
                remote = null,
                branch = null,
            ),
        )

        assertTrue(selection is SharedWorkspaceSelection.AppPrivate)
        val descriptor = (selection as SharedWorkspaceSelection.AppPrivate).descriptor
        assertEquals("custom/nested/root", descriptor.rootId)
    }
}
