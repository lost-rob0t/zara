package ai.zara.app.projects

import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Assert.assertThrows
import org.junit.Test

class ProjectContextStoreTest {
    @Test fun `create select and restart preserve app private project context`() {
        val root = Files.createTempDirectory("zara-project-context").toFile()
        val file = File(root, "projects.bin")
        val ids = ArrayDeque(listOf("project-a"))
        val store = ProjectContextStore(file) { ids.removeFirst() }

        val created = store.create("  Star Intel  ")
        assertEquals("project-a", created.id)
        assertEquals("Star Intel", created.name)
        assertEquals(ProjectSourceScope.AppPrivate, created.sourceScope)
        assertNull(created.conversationId)

        store.select(created.id)
        val restored = ProjectContextStore(file).state()
        assertEquals("project-a", restored.selectedProjectId)
        assertEquals(created.copy(conversationId = null), restored.selectedProject)
        assertNull(restored.loadFailure)
    }

    @Test fun `conversation binding remains isolated between projects and survives restart`() {
        val root = Files.createTempDirectory("zara-project-conversations").toFile()
        val file = File(root, "projects.bin")
        val ids = ArrayDeque(listOf("alpha", "beta"))
        val store = ProjectContextStore(file) { ids.removeFirst() }
        val alpha = store.create("Alpha")
        val beta = store.create("Beta")

        store.bindConversation(alpha.id, "conversation-alpha")
        store.bindConversation(beta.id, "conversation-beta")

        val restored = ProjectContextStore(file).state()
        assertEquals("conversation-alpha", restored.project(alpha.id)?.conversationId)
        assertEquals("conversation-beta", restored.project(beta.id)?.conversationId)
    }

    @Test fun `switching project changes selection without rewriting conversation identity`() {
        val root = Files.createTempDirectory("zara-project-switch").toFile()
        val file = File(root, "projects.bin")
        val ids = ArrayDeque(listOf("alpha", "beta"))
        val store = ProjectContextStore(file) { ids.removeFirst() }
        val alpha = store.create("Alpha")
        val beta = store.create("Beta")
        store.bindConversation(alpha.id, "conversation-alpha")
        store.bindConversation(beta.id, "conversation-beta")

        store.select(alpha.id)
        assertEquals("conversation-alpha", store.state().selectedProject?.conversationId)
        store.select(beta.id)
        assertEquals("conversation-beta", store.state().selectedProject?.conversationId)
        store.select(alpha.id)
        assertEquals("conversation-alpha", store.state().selectedProject?.conversationId)
    }

    @Test fun `unknown project selection and binding fail without mutating current selection`() {
        val root = Files.createTempDirectory("zara-project-missing").toFile()
        val file = File(root, "projects.bin")
        val store = ProjectContextStore(file) { "known" }
        store.create("Known")
        store.select("known")

        assertThrows(IllegalArgumentException::class.java) { store.select("missing") }
        assertThrows(IllegalArgumentException::class.java) {
            store.bindConversation("missing", "conversation")
        }
        assertEquals("known", store.state().selectedProjectId)
    }

    @Test fun `project names are bounded and do not encode filesystem authority`() {
        val root = Files.createTempDirectory("zara-project-names").toFile()
        val store = ProjectContextStore(File(root, "projects.bin")) { "id" }

        assertThrows(IllegalArgumentException::class.java) { store.create("   ") }
        assertThrows(IllegalArgumentException::class.java) { store.create("a".repeat(81)) }
        assertThrows(IllegalArgumentException::class.java) { store.create("bad\nname") }

        val created = store.create("~/Documents/Projects/zara")
        assertEquals("~/Documents/Projects/zara", created.name)
        assertEquals(ProjectSourceScope.AppPrivate, created.sourceScope)
    }

    @Test fun `duplicate project names are rejected case insensitively without changing registry`() {
        val root = Files.createTempDirectory("zara-project-duplicate-name").toFile()
        val ids = ArrayDeque(listOf("alpha", "beta"))
        val store = ProjectContextStore(File(root, "projects.bin")) { ids.removeFirst() }
        store.create("Alpha")

        assertThrows(IllegalArgumentException::class.java) { store.create(" alpha ") }
        assertEquals(listOf("Alpha"), store.state().projects.map { it.name })
    }

    @Test fun `duplicate generated project id is rejected without overwriting existing context`() {
        val root = Files.createTempDirectory("zara-project-duplicate-id").toFile()
        val store = ProjectContextStore(File(root, "projects.bin")) { "same-id" }
        store.create("Alpha")

        assertThrows(IllegalArgumentException::class.java) { store.create("Beta") }
        assertEquals(listOf("Alpha"), store.state().projects.map { it.name })
    }

    @Test fun `conversation ids are trimmed bounded and reject control characters`() {
        val root = Files.createTempDirectory("zara-project-conversation-id").toFile()
        val store = ProjectContextStore(File(root, "projects.bin")) { "alpha" }
        store.create("Alpha")

        store.bindConversation("alpha", "  conversation-alpha  ")
        assertEquals("conversation-alpha", store.state().project("alpha")?.conversationId)
        assertThrows(IllegalArgumentException::class.java) {
            store.bindConversation("alpha", "conversation\nalpha")
        }
        assertThrows(IllegalArgumentException::class.java) {
            store.bindConversation("alpha", "c".repeat(257))
        }
        assertEquals("conversation-alpha", store.state().project("alpha")?.conversationId)
    }

    @Test fun `corrupt state degrades explicitly instead of inventing projects`() {
        val root = Files.createTempDirectory("zara-project-corrupt").toFile()
        val file = File(root, "projects.bin")
        file.writeText("not a Zara project registry")

        val state = ProjectContextStore(file).state()
        assertTrue(state.projects.isEmpty())
        assertNull(state.selectedProjectId)
        assertNotNull(state.loadFailure)
    }

    @Test fun `degraded state blocks mutation and preserves corrupt bytes for recovery`() {
        val root = Files.createTempDirectory("zara-project-degraded-write").toFile()
        val file = File(root, "projects.bin")
        val original = "not a Zara project registry"
        file.writeText(original)
        val store = ProjectContextStore(file) { "alpha" }

        assertThrows(IllegalStateException::class.java) { store.create("Alpha") }
        assertThrows(IllegalStateException::class.java) { store.select(null) }
        assertEquals(original, file.readText())
    }

    @Test fun `clearing selection preserves registered contexts`() {
        val root = Files.createTempDirectory("zara-project-clear").toFile()
        val file = File(root, "projects.bin")
        val store = ProjectContextStore(file) { "alpha" }
        store.create("Alpha")
        store.select("alpha")
        store.select(null)

        val state = ProjectContextStore(file).state()
        assertNull(state.selectedProjectId)
        assertEquals(listOf("alpha"), state.projects.map { it.id })
    }
}
