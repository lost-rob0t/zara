package ai.zara.app.localai

import java.io.File
import java.io.InputStream
import java.security.MessageDigest
import java.util.Properties

class LocalModelStore(
    private val root: File,
) {
    private val manifest = File(root, ACTIVE_MANIFEST)
    private val metadataRoot = File(root, METADATA_DIRECTORY)

    fun install(
        source: InputStream,
        metadata: LocalModelMetadata,
    ): LocalModelSpec {
        ensureRoot()
        val safeName = "${metadata.id}-${metadata.version}${metadata.format.extension}"
        val destination = File(root, safeName).canonicalFile
        require(destination.parentFile == root.canonicalFile) { "Model path escaped app-private storage" }
        val temporary = File(root, ".$safeName.part")
        val digest = MessageDigest.getInstance("SHA-256")

        try {
            temporary.outputStream().buffered().use { output ->
                val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
                var total = 0L
                while (true) {
                    val read = source.read(buffer)
                    if (read < 0) break
                    if (read == 0) continue
                    total += read
                    require(total <= MAX_MODEL_BYTES) { "Local model exceeds the supported size limit" }
                    digest.update(buffer, 0, read)
                    output.write(buffer, 0, read)
                }
                require(total > 0L) { "Local model is empty" }
            }
            val actual = digest.digest().toHex()
            require(actual == metadata.sha256) { "Local model SHA-256 mismatch" }
            if (destination.exists()) require(destination.delete()) { "Existing local model could not be replaced" }
            require(temporary.renameTo(destination)) { "Local model could not be finalized" }
            writeCatalogManifest(destination.name, metadata)
            val spec = metadata.toSpec(destination.absolutePath)
            activate(spec)
            return spec
        } catch (error: Throwable) {
            temporary.delete()
            throw error
        }
    }

    fun installedModels(): List<LocalModelSpec> {
        if (!root.isDirectory) return emptyList()
        val models = linkedMapOf<Pair<String, String>, LocalModelSpec>()
        if (metadataRoot.isDirectory) {
            metadataRoot.listFiles { file -> file.isFile && file.extension == "properties" }
                ?.sortedBy(File::getName)
                ?.forEach { file ->
                    val spec = readManifest(file)
                    models[spec.id to spec.version] = spec
                }
        }
        // Compatibility with installs created before the provider catalog existed.
        activeModel()?.let { active -> models.putIfAbsent(active.id to active.version, active) }
        return models.values.toList()
    }

    fun model(
        id: String,
        version: String,
    ): LocalModelSpec? {
        validateIdentity(id, version)
        val metadataFile = catalogManifest(id, version)
        if (metadataFile.isFile) return readManifest(metadataFile)
        return activeModel()?.takeIf { it.id == id && it.version == version }
    }

    fun activate(spec: LocalModelSpec) {
        ensureRoot()
        val file = File(spec.path).canonicalFile
        require(file.parentFile == root.canonicalFile) { "Model path escaped app-private storage" }
        check(file.isFile) { "Local model file is missing" }
        check(sha256(file) == spec.sha256) { "Local model failed SHA-256 verification" }
        writeManifest(manifest, file.name, spec.metadata())
    }

    fun activeModel(): LocalModelSpec? {
        if (!manifest.isFile) return null
        return readManifest(manifest)
    }

    fun clear() {
        if (manifest.exists()) check(manifest.delete()) { "Active model manifest could not be removed" }
    }

    private fun readManifest(file: File): LocalModelSpec {
        val properties = Properties().apply {
            file.inputStream().buffered().use(::load)
        }
        val filename = requireProperty(properties, "filename")
        val modelFile = File(root, filename).canonicalFile
        check(modelFile.parentFile == root.canonicalFile) { "Model path escaped app-private storage" }
        check(modelFile.isFile) { "Local model file is missing" }
        val format = properties.getProperty("format")
            ?.trim()
            ?.takeIf(String::isNotEmpty)
            ?.let(LocalModelFormat::requireKnown)
            ?: LocalModelFormat.LITERT_LM
        val metadata = LocalModelMetadata(
            id = requireProperty(properties, "id"),
            version = requireProperty(properties, "version"),
            quantization = LocalModelQuantization.requireKnown(requireProperty(properties, "quantization")),
            sha256 = requireProperty(properties, "sha256"),
            maxContextTokens = requireProperty(properties, "max_context_tokens").toIntOrNull()
                ?: error("Local model context limit is invalid"),
            backend = runCatching {
                LocalModelBackend.valueOf(requireProperty(properties, "backend"))
            }.getOrElse { throw IllegalStateException("Local model backend is invalid") },
            format = format,
        )
        check(modelFile.name.endsWith(format.extension, ignoreCase = true)) {
            "Local model filename does not match declared format"
        }
        check(sha256(modelFile) == metadata.sha256) { "Local model failed SHA-256 verification" }
        return metadata.toSpec(modelFile.absolutePath)
    }

    private fun writeCatalogManifest(
        filename: String,
        metadata: LocalModelMetadata,
    ) {
        check(metadataRoot.mkdirs() || metadataRoot.isDirectory) { "Local model metadata directory is unavailable" }
        writeManifest(catalogManifest(metadata.id, metadata.version), filename, metadata)
    }

    private fun catalogManifest(
        id: String,
        version: String,
    ): File = File(metadataRoot, "$id-$version.properties")

    private fun writeManifest(
        destination: File,
        filename: String,
        metadata: LocalModelMetadata,
    ) {
        val properties = Properties().apply {
            setProperty("schema", "2")
            setProperty("filename", filename)
            setProperty("id", metadata.id)
            setProperty("version", metadata.version)
            setProperty("format", metadata.format.wireName)
            setProperty("quantization", metadata.quantization.wireName)
            setProperty("sha256", metadata.sha256)
            setProperty("max_context_tokens", metadata.maxContextTokens.toString())
            setProperty("backend", metadata.backend.name)
        }
        val parent = checkNotNull(destination.parentFile) { "Model metadata directory is unavailable" }
        check(parent.mkdirs() || parent.isDirectory) { "Model metadata directory is unavailable" }
        val temporary = File(parent, ".${destination.name}.part")
        temporary.outputStream().buffered().use { properties.store(it, "Zara local model metadata") }
        if (destination.exists()) check(destination.delete()) { "Old model manifest could not be replaced" }
        check(temporary.renameTo(destination)) { "Model manifest could not be finalized" }
    }

    private fun ensureRoot() {
        check(root.mkdirs() || root.isDirectory) { "Local model directory is unavailable" }
    }

    private fun validateIdentity(
        id: String,
        version: String,
    ) {
        require(id.matches(Regex("[A-Za-z0-9._-]{1,96}"))) { "Model id is invalid" }
        require(version.matches(Regex("[A-Za-z0-9._+-]{1,64}"))) { "Model version is invalid" }
    }

    private fun requireProperty(
        properties: Properties,
        name: String,
    ): String = properties.getProperty(name)?.trim()?.takeIf(String::isNotEmpty)
        ?: throw IllegalStateException("Local model metadata is missing $name")

    private fun LocalModelMetadata.toSpec(path: String): LocalModelSpec = LocalModelSpec(
        id = id,
        version = version,
        quantization = quantization,
        sha256 = sha256,
        path = path,
        maxContextTokens = maxContextTokens,
        backend = backend,
        format = format,
    )

    private fun sha256(file: File): String {
        val digest = MessageDigest.getInstance("SHA-256")
        file.inputStream().buffered().use { input ->
            val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
            while (true) {
                val read = input.read(buffer)
                if (read < 0) break
                if (read > 0) digest.update(buffer, 0, read)
            }
        }
        return digest.digest().toHex()
    }

    private fun ByteArray.toHex(): String = joinToString("") { "%02x".format(it) }

    companion object {
        private const val ACTIVE_MANIFEST = "active.properties"
        private const val METADATA_DIRECTORY = "metadata"
        private const val MAX_MODEL_BYTES = 8L * 1024 * 1024 * 1024
    }
}
