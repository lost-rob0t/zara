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

    @Test fun `active project is projected into chat breadcrumb and send scope`() {
        val app = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val projects = File("src/main/java/ai/zara/app/ui/ProjectsSurface.kt").readText()

        assertTrue(app.contains("projectState.selectedProject"))
        assertTrue(app.contains("project?.let { ProjectBreadcrumb(it) }"))
        assertTrue(app.contains("onSendText(message, project)"))
        assertTrue(projects.contains("\"Chat / ${'$'}{project.name}\""))
    }

    @Test fun `projects surface renders host operation failures`() {
        val source = File("src/main/java/ai/zara/app/ui/ProjectsSurface.kt").readText()

        assertTrue(source.contains("operationError: String?"))
        assertTrue(source.contains("operationError?.takeIf"))
        assertTrue(source.contains("ErrorBanner"))
    }

    @Test fun `host isolates last turn projection by selected project`() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(source.contains("projectTurns by mutableStateOf<Map<String, RenderedTextTurn>>"))
        assertTrue(source.contains("projectState.selectedProjectId?.let { projectTurns[it] }"))
        assertTrue(source.contains("projectTurns = projectTurns + (project.id to rendered)"))
    }

    @Test fun `host persists project selection and binds only remote conversation identities`() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(source.contains("ProjectContextStore(File(filesDir, \"projects.bin\"))"))
        assertTrue(source.contains("projectStore.select(created.id)"))
        assertTrue(source.contains("projectStore.select(projectId)"))
        assertTrue(source.contains("takeUnless { it.startsWith(\"local-project:\") }"))
        assertTrue(source.contains("projectStore.bindConversation("))
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
