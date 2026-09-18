package ai.zara.app.projects

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.util.UUID

private const val PROJECT_STORE_MAGIC = "ZARA-PROJECTS/1"
private const val MAX_PROJECT_STORE_BYTES = 64 * 1024
private const val MAX_PROJECTS = 64
private const val MAX_PROJECT_NAME_CHARS = 80
private const val MAX_PROJECT_ID_CHARS = 128
private const val MAX_CONVERSATION_ID_CHARS = 256

enum class ProjectSourceScope {
    AppPrivate,
}

data class ProjectContext(
    val id: String,
    val name: String,
    val sourceScope: ProjectSourceScope = ProjectSourceScope.AppPrivate,
    val conversationId: String? = null,
)

data class ProjectContextState(
    val projects: List<ProjectContext> = emptyList(),
    val selectedProjectId: String? = null,
    val loadFailure: String? = null,
) {
    val selectedProject: ProjectContext?
        get() = selectedProjectId?.let(::project)

    fun project(id: String): ProjectContext? = projects.firstOrNull { it.id == id }
}

class ProjectContextStore(
    private val file: File,
    private val idFactory: () -> String = { UUID.randomUUID().toString() },
) {
    @Volatile
    private var current: ProjectContextState = load()

    @Synchronized
    fun state(): ProjectContextState = current

    @Synchronized
    fun create(rawName: String): ProjectContext {
        ensureHealthy()
        check(current.projects.size < MAX_PROJECTS) { "Project limit reached" }
        val name = normalizeName(rawName)
        require(current.projects.none { it.name.equals(name, ignoreCase = true) }) {
            "A project with that name already exists"
        }
        val id = validateId(idFactory())
        require(current.projects.none { it.id == id }) { "Project id already exists" }
        val project = ProjectContext(id = id, name = name)
        commit(current.copy(projects = current.projects + project))
        return project
    }

    @Synchronized
    fun select(projectId: String?): ProjectContextState {
        ensureHealthy()
        projectId?.let { id ->
            require(current.project(id) != null) { "Unknown project: $id" }
        }
        return commit(current.copy(selectedProjectId = projectId))
    }

    @Synchronized
    fun bindConversation(projectId: String, conversationId: String?): ProjectContextState {
        ensureHealthy()
        require(current.project(projectId) != null) { "Unknown project: $projectId" }
        val normalizedConversationId = conversationId?.trim()?.also { value ->
            require(value.isNotEmpty()) { "Conversation id must not be blank" }
            require(value.length <= MAX_CONVERSATION_ID_CHARS) { "Conversation id is too long" }
            require(value.none(Char::isISOControl)) { "Conversation id contains control characters" }
        }
        val projects = current.projects.map { project ->
            if (project.id == projectId) {
                project.copy(conversationId = normalizedConversationId)
            } else {
                project
            }
        }
        return commit(current.copy(projects = projects))
    }

    private fun ensureHealthy() {
        check(current.loadFailure == null) {
            "Project metadata is degraded; preserve the file for recovery before changing projects"
        }
    }

    private fun commit(next: ProjectContextState): ProjectContextState {
        persist(next.copy(loadFailure = null))
        current = next.copy(loadFailure = null)
        return current
    }

    private fun load(): ProjectContextState {
        if (!file.exists()) return ProjectContextState()
        if (!file.isFile || file.length() !in 1..MAX_PROJECT_STORE_BYTES.toLong()) {
            return degradedState()
        }
        return try {
            DataInputStream(FileInputStream(file).buffered()).use { input ->
                require(input.readUTF() == PROJECT_STORE_MAGIC)
                val selectedProjectId = input.readUTF().ifEmpty { null }
                val count = input.readInt()
                require(count in 0..MAX_PROJECTS)
                val projects = buildList(count) {
                    repeat(count) {
                        val id = validateId(input.readUTF())
                        val name = normalizeName(input.readUTF())
                        val scope = ProjectSourceScope.valueOf(input.readUTF())
                        val conversationId = input.readUTF().ifEmpty { null }?.also { value ->
                            require(value.length <= MAX_CONVERSATION_ID_CHARS)
                            require(value.none(Char::isISOControl))
                        }
                        add(ProjectContext(id, name, scope, conversationId))
                    }
                }
                require(projects.map { it.id }.toSet().size == projects.size)
                require(projects.map { it.name.lowercase() }.toSet().size == projects.size)
                require(selectedProjectId == null || projects.any { it.id == selectedProjectId })
                require(input.read() == -1)
                ProjectContextState(projects, selectedProjectId)
            }
        } catch (_: Exception) {
            degradedState()
        }
    }

    private fun persist(state: ProjectContextState) {
        val directory = file.absoluteFile.parentFile
            ?: throw IllegalStateException("Project store path has no parent directory")
        check(directory.exists() || directory.mkdirs()) {
            "Project store directory could not be created"
        }
        val temp = Files.createTempFile(directory.toPath(), ".${file.name}.", ".tmp").toFile()
        try {
            DataOutputStream(FileOutputStream(temp).buffered()).use { output ->
                output.writeUTF(PROJECT_STORE_MAGIC)
                output.writeUTF(state.selectedProjectId.orEmpty())
                output.writeInt(state.projects.size)
                state.projects.forEach { project ->
                    output.writeUTF(project.id)
                    output.writeUTF(project.name)
                    output.writeUTF(project.sourceScope.name)
                    output.writeUTF(project.conversationId.orEmpty())
                }
            }
            check(temp.length() in 1..MAX_PROJECT_STORE_BYTES.toLong()) {
                "Project metadata exceeds the bounded store size"
            }
            replace(temp, file)
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    private fun replace(source: File, destination: File) {
        try {
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING,
            )
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING)
        }
    }

    private fun normalizeName(rawName: String): String {
        val name = rawName.trim()
        require(name.isNotEmpty()) { "Project name is required" }
        require(name.length <= MAX_PROJECT_NAME_CHARS) { "Project name is too long" }
        require(name.none(Char::isISOControl)) { "Project name contains control characters" }
        return name
    }

    private fun validateId(rawId: String): String {
        val id = rawId.trim()
        require(id.isNotEmpty()) { "Project id is required" }
        require(id.length <= MAX_PROJECT_ID_CHARS) { "Project id is too long" }
        require(id.none(Char::isISOControl)) { "Project id contains control characters" }
        return id
    }

    private fun degradedState(): ProjectContextState = ProjectContextState(
        loadFailure = "Project metadata is corrupt or unsupported",
    )
}
