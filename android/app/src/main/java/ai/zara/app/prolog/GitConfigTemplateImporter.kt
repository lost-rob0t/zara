package ai.zara.app.prolog

import org.eclipse.jgit.api.Git
import java.io.File
import java.net.URI
import java.nio.file.Files
import java.util.Properties

data class GitConfigTemplate(
    val name: String,
    val repository: String,
    val revision: String,
    val sources: List<PrologSource>,
)

/**
 * Imports public HTTPS Git repositories as data-only Zara configuration templates.
 * JGit does not execute repository hooks. Only manifest-declared Prolog/Org content
 * enters the private workspace, and every resulting source is analyzed before use.
 */
class GitConfigTemplateImporter(
    private val cacheRoot: File,
) {
    fun import(repository: String, ref: String? = null): GitConfigTemplate {
        val uri = validateRepository(repository)
        val root = cacheRoot.canonicalFile
        check(root.mkdirs() || root.isDirectory) { "Git template cache is unavailable" }
        val checkout = Files.createTempDirectory(root.toPath(), "zara-config-").toFile().canonicalFile
        require(checkout.path.startsWith(root.path + File.separator)) { "Git checkout escaped cache root" }
        try {
            val clone = Git.cloneRepository()
                .setURI(uri.toString())
                .setDirectory(checkout)
                .setDepth(1)
                .setCloneSubmodules(false)
            if (!ref.isNullOrBlank()) {
                require(ref.matches(REF)) { "Git template ref is invalid" }
                clone.setBranch(ref)
            }
            clone.call().use { git ->
                val manifest = manifestFile(checkout)
                val properties = Properties().apply {
                    manifest.inputStream().use { input -> load(input) }
                }
                require(properties.getProperty("version") == "1") { "Unsupported Zara config template version" }
                val name = properties.getProperty("name")?.trim().orEmpty()
                require(name.isNotEmpty() && name.length <= 128) { "Template name is invalid" }

                val imported = mutableListOf<PrologSource>()
                val directory = properties.getProperty("directory")?.trim()?.takeIf(String::isNotEmpty)
                val org = properties.getProperty("org")?.trim()?.takeIf(String::isNotEmpty)

                directory?.let { imported += importDirectory(checkout, it) }
                org?.let { imported += importOrg(checkout, it) }

                if (directory == null && org == null) {
                    val conventional = resolveInside(checkout, DEFAULT_ANDROID_DOTFILES_DIRECTORY)
                    if (conventional.isDirectory && !Files.isSymbolicLink(conventional.toPath())) {
                        imported += importDirectory(checkout, DEFAULT_ANDROID_DOTFILES_DIRECTORY)
                    }
                }

                require(imported.isNotEmpty()) {
                    "Template declares no Prolog sources and has no canonical Android dotfiles directory"
                }
                require(imported.size <= MAX_SOURCES) { "Template exceeds source limit" }
                val duplicate = imported.groupingBy(PrologSource::name).eachCount().entries.firstOrNull { it.value > 1 }
                require(duplicate == null) { "Template emits duplicate source names" }
                val totalBytes = imported.sumOf { it.text.encodeToByteArray().size.toLong() }
                require(totalBytes <= MAX_TOTAL_BYTES) { "Template exceeds total byte limit" }
                imported.forEach(::validateSource)

                val revision = git.repository.resolve("HEAD")?.name
                    ?: throw IllegalArgumentException("Template repository has no HEAD")
                require(revision.matches(Regex("[0-9a-f]{40}"))) { "Template revision is not immutable" }
                return GitConfigTemplate(name, uri.toString(), revision, imported.sortedBy(PrologSource::name))
            }
        } finally {
            checkout.deleteRecursively()
        }
    }

    private fun importDirectory(checkout: File, relative: String): List<PrologSource> {
        val directory = resolveInside(checkout, relative)
        require(directory.isDirectory && !Files.isSymbolicLink(directory.toPath())) {
            "Template directory is unavailable"
        }
        return directory.walkTopDown()
            .maxDepth(8)
            .filter { file -> file.isFile && file.extension == "pl" && !Files.isSymbolicLink(file.toPath()) }
            .take(MAX_SOURCES + 1)
            .map { file ->
                require(file.length() <= MAX_SOURCE_BYTES) { "Template source exceeds byte limit" }
                val relativeFile = file.canonicalFile.relativeTo(directory.canonicalFile)
                require(relativeFile.path == relativeFile.name) {
                    "Template directory may contain only top-level Prolog source files"
                }
                val name = file.name
                require(name.matches(SOURCE_NAME)) { "Template source name is invalid" }
                PrologSource(name, file.readText(Charsets.UTF_8))
            }
            .toList()
    }

    private fun importOrg(checkout: File, relative: String): List<PrologSource> {
        val org = resolveInside(checkout, relative)
        require(org.isFile && !Files.isSymbolicLink(org.toPath())) { "Template Org file is unavailable" }
        require(org.length() <= MAX_ORG_BYTES) { "Template Org file exceeds byte limit" }
        val outputs = linkedMapOf<String, StringBuilder>()
        var target: String? = null
        org.readLines(Charsets.UTF_8).forEach { line ->
            val begin = ORG_BEGIN.matchEntire(line.trim())
            if (begin != null) {
                require(target == null) { "Nested Prolog Org source block" }
                val declared = begin.groupValues[1]
                val name = File(declared).name
                require(declared == name && name.matches(SOURCE_NAME)) {
                    "Org :tangle target must be a .pl basename"
                }
                target = name
                return@forEach
            }
            if (ORG_END.matches(line.trim())) {
                require(target != null) { "Unmatched Org source block end" }
                target = null
                return@forEach
            }
            target?.let { name ->
                val output = outputs.getOrPut(name) { StringBuilder() }
                output.append(line).append('\n')
                require(output.toString().encodeToByteArray().size <= MAX_SOURCE_BYTES) {
                    "Tangled Prolog source exceeds byte limit"
                }
            }
        }
        require(target == null) { "Unterminated Prolog Org source block" }
        return outputs.map { (name, text) -> PrologSource(name, text.toString()) }
    }

    private fun manifestFile(checkout: File): File {
        val candidates = listOf(
            resolveInside(checkout, ".zara/config-template.properties"),
            resolveInside(checkout, "zara-template.properties"),
        )
        return candidates.firstOrNull { it.isFile && !Files.isSymbolicLink(it.toPath()) }
            ?: throw IllegalArgumentException("Repository is not a Zara config template")
    }

    private fun resolveInside(checkout: File, relative: String): File {
        require(relative.isNotBlank() && !relative.startsWith('/')) { "Template path must be relative" }
        val root = checkout.canonicalFile
        val file = File(root, relative).canonicalFile
        require(file.path.startsWith(root.path + File.separator)) { "Template path escaped checkout" }
        return file
    }

    private fun validateSource(source: PrologSource) {
        require(source.text.encodeToByteArray().size <= MAX_SOURCE_BYTES) { "Template source exceeds byte limit" }
        val document = PrologSourceAnalyzer.analyze(source.name, source.text)
        require(document.diagnostics.isEmpty()) {
            "Template source ${source.name} failed Prolog analysis: " +
                document.diagnostics.joinToString("; ") { it.message }
        }
        document.clauses.filter { it.kind == PrologClauseKind.DIRECTIVE }.forEach { clause ->
            val directive = clause.text.trim().removePrefix(":-").removeSuffix(".").trim()
            require(ALLOWED_DIRECTIVE.matches(directive)) {
                "Template source ${source.name} contains an unsupported directive"
            }
        }
        require(!DANGEROUS_PREDICATE.containsMatchIn(source.text)) {
            "Template source ${source.name} contains a predicate outside the data-only template boundary"
        }
    }

    private fun validateRepository(raw: String): URI {
        require(raw.length <= 2_048) { "Git repository URL is too long" }
        val uri = URI(raw.trim())
        require(uri.scheme == "https" && !uri.host.isNullOrBlank()) { "Only public HTTPS Git repositories are accepted" }
        require(uri.userInfo == null) { "Credentials must not be embedded in Git template URLs" }
        require(uri.fragment == null) { "Git template URL must not contain a fragment" }
        return uri
    }

    private companion object {
        val SOURCE_NAME = Regex("[a-zA-Z][a-zA-Z0-9_-]{0,63}\\.pl")
        val REF = Regex("[A-Za-z0-9._/-]{1,160}")
        val ORG_BEGIN = Regex("(?i)^#\\+begin_src\\s+prolog(?:\\s+.*)?\\s+:tangle\\s+([^\\s]+)\\s*$")
        val ORG_END = Regex("(?i)^#\\+end_src\\s*$")
        val ALLOWED_DIRECTIVE = Regex("(?s)^(zara_schema\\s*\\(.*\\)|op\\s*\\(.*\\))$")
        val DANGEROUS_PREDICATE = Regex(
            "(?i)(^|[^a-zA-Z0-9_])(" +
                "consult|ensure_loaded|load_files|initialization|call|once|catch|throw|halt|shell|" +
                "open|close|read|write|process_create|assert|asserta|assertz|retract|retractall|abolish|" +
                "clause|current_predicate|set_prolog_flag|working_directory|directory_files|delete_file|rename_file" +
                ")\\s*(\\(|$)",
        )
        const val DEFAULT_ANDROID_DOTFILES_DIRECTORY = ".config/zarathushtra/android"
        const val MAX_SOURCES = 64
        const val MAX_SOURCE_BYTES = 512 * 1024L
        const val MAX_TOTAL_BYTES = 4 * 1024 * 1024L
        const val MAX_ORG_BYTES = 4 * 1024 * 1024L
    }
}
