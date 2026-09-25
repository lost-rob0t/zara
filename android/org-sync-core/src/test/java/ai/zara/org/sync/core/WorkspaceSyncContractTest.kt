package ai.zara.org.sync.core

import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class WorkspaceSyncContractTest {
    @Test
    fun appPrivateWorkspaceMapsUnderOwnedRoot() {
        val filesDir = Files.createTempDirectory("zara-org-files").toFile()
        val descriptor = OrgWorkspaceDescriptor.AppPrivate(
            id = WorkspaceId("notes-main"),
            displayName = "Notes",
            rootId = "notes/main",
        )

        val mapped = OrgWorkspaceMapper.appPrivateRoot(filesDir, descriptor)

        assertTrue(mapped.canonicalPath.startsWith(filesDir.canonicalPath))
        assertEquals(
            filesDir.resolve("org-workspaces/notes/main").canonicalPath,
            mapped.canonicalPath,
        )
    }

    @Test
    fun workspaceMappingRejectsTraversal() {
        val rejected = runCatching {
            OrgWorkspaceDescriptor.AppPrivate(
                id = WorkspaceId("escape"),
                displayName = "Escape",
                rootId = "../outside",
            )
        }.isFailure

        assertTrue(rejected)
    }

    @Test
    fun safWorkspaceKeepsContentUriAsAuthority() {
        val descriptor = OrgWorkspaceDescriptor.SafTree(
            id = WorkspaceId("phone-notes"),
            displayName = "Phone notes",
            treeUri = "content://com.example.documents/tree/primary%3ANotes",
        )

        assertEquals(
            "content://com.example.documents/tree/primary%3ANotes",
            descriptor.treeUri,
        )
        assertFalse(descriptor.treeUri.startsWith("file:"))
    }

    @Test
    fun gitWorkspaceRejectsCredentialBearingRemote() {
        val rejected = runCatching {
            OrgWorkspaceDescriptor.Git(
                id = WorkspaceId("git-main"),
                displayName = "Git notes",
                localRootId = "git/main",
                remote = "https://user:secret@example.invalid/notes.git",
                branch = "main",
            )
        }.isFailure

        assertTrue(rejected)
    }

    @Test
    fun newerGenerationFencesOlderLease() {
        val fence = SyncGenerationFence()
        val first = fence.begin()
        val second = fence.begin()

        assertFalse(first.isCurrent())
        assertTrue(second.isCurrent())
        assertTrue(second.generation > first.generation)
    }

    @Test
    fun explicitCancelFencesCurrentLease() {
        val fence = SyncGenerationFence()
        val lease = fence.begin()

        assertTrue(fence.cancel(lease))
        assertFalse(lease.isCurrent())
        assertFalse(fence.cancel(lease))
    }
}
