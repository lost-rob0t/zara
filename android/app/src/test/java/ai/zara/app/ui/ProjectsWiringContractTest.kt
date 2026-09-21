package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ProjectsWiringContractTest {
    @Test fun `projects route renders the real workspace instead of the implementation gate`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("AppSurface.Projects -> ProjectsSurface("))
        assertTrue(source.contains("operationError = operationError"))
        assertFalse(source.contains("AppSurface.Projects -> GatedSurface"))
    }

    @Test fun `conversation project is projected into chat breadcrumb and send scope`() {
        val app = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val projects = File("src/main/java/ai/zara/app/ui/ProjectsSurface.kt").readText()

        assertTrue(app.contains("conversation?.projectId?.let(projectState::project)"))
        assertTrue(app.contains("project?.let { ProjectBreadcrumb(it) }"))
        assertTrue(app.contains("onSendText(message, conversation, project)"))
        assertTrue(projects.contains("Chat / "))
        assertTrue(projects.contains("project.name"))
    }

    @Test fun `projects surface renders host operation failures`() {
        val source = File("src/main/java/ai/zara/app/ui/ProjectsSurface.kt").readText()

        assertTrue(source.contains("operationError: String?"))
        assertTrue(source.contains("operationError?.takeIf"))
        assertTrue(source.contains("ErrorBanner"))
    }

    @Test fun `host persists full conversations and turn status instead of a last turn projection`() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(source.contains("ConversationStore(File(filesDir, \"conversations.bin\"))"))
        assertTrue(source.contains("conversationState by mutableStateOf"))
        assertTrue(source.contains("conversationStore.beginTurn("))
        assertTrue(source.contains("conversationStore.completeTurn("))
        assertTrue(source.contains("conversationStore.failTurn("))
    }

    @Test fun `project selection and chat project assignment share one durable association`() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(source.contains("ProjectContextStore(File(filesDir, \"projects.bin\"))"))
        assertTrue(source.contains("projectStore.select(created.id)"))
        assertTrue(source.contains("projectStore.select(projectId)"))
        assertTrue(source.contains("conversationStore.moveToProject("))
        assertTrue(source.contains("remoteConversationId"))
    }

    @Test fun `project model exposes no filesystem source authority`() {
        val source = File("src/main/java/ai/zara/app/projects/ProjectContextStore.kt").readText()
        val model = source.substringAfter("data class ProjectContext(")
            .substringBefore("data class ProjectContextState(")

        assertTrue(model.contains("sourceScope: ProjectSourceScope = ProjectSourceScope.AppPrivate"))
        assertFalse(model.contains("path:"))
        assertFalse(model.contains("uri:"))
        assertFalse(model.contains("directory:"))
    }
}
