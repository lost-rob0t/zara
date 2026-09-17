package ai.zara.app.localai

import java.io.File
import java.io.InputStream
import java.security.MessageDigest
import java.util.Properties

class LocalModelStore(
    private val root: File,
) {
    private val manifest = File(root, ACTIVE_MANIFEST)

    fun install(
        source: InputStream,
        metadata: LocalModelMetadata,
    ): LocalModelSpec {
        ensureRoot()
        val safeName = "${metadata.id}-${metadata.version}.litertlm"
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
            writeManifest(destination.name, metadata)
            return metadata.toSpec(destination.absolutePath)
        } catch (error: Throwable) {
            temporary.delete()
            throw error
        }
    }

    fun activeModel(): LocalModelSpec? {
        if (!manifest.isFile) return null
        val properties = Properties().apply {
            manifest.inputStream().buffered().use(::load)
        }
        val filename = requireProperty(properties, "filename")
        val file = File(root, filename).canonicalFile
        check(file.parentFile == root.canonicalFile) { "Active model path escaped app-private storage" }
        check(file.isFile) { "Active local model file is missing" }
        val metadata = LocalModelMetadata(
            id = requireProperty(properties, "id"),
            version = requireProperty(properties, "version"),
            quantization = LocalModelQuantization.requireKnown(requireProperty(properties, "quantization")),
            sha256 = requireProperty(properties, "sha256"),
            maxContextTokens = requireProperty(properties, "max_context_tokens").toIntOrNull()
                ?: error("Active model context limit is invalid"),
            backend = runCatching {
                LocalModelBackend.valueOf(requireProperty(properties, "backend"))
            }.getOrElse { throw IllegalStateException("Active model backend is invalid") },
        )
        check(sha256(file) == metadata.sha256) { "Active local model failed SHA-256 verification" }
        return metadata.toSpec(file.absolutePath)
    }

    fun clear() {
        if (manifest.exists()) check(manifest.delete()) { "Active model manifest could not be removed" }
    }

    private fun writeManifest(
        filename: String,
        metadata: LocalModelMetadata,
    ) {
        val properties = Properties().apply {
            setProperty("schema", "1")
            setProperty("filename", filename)
            setProperty("id", metadata.id)
            setProperty("version", metadata.version)
            setProperty("quantization", metadata.quantization.wireName)
            setProperty("sha256", metadata.sha256)
            setProperty("max_context_tokens", metadata.maxContextTokens.toString())
            setProperty("backend", metadata.backend.name)
        }
        val temporary = File(root, ".$ACTIVE_MANIFEST.part")
        temporary.outputStream().buffered().use { properties.store(it, "Zara local model metadata") }
        if (manifest.exists()) check(manifest.delete()) { "Old model manifest could not be replaced" }
        check(temporary.renameTo(manifest)) { "Model manifest could not be finalized" }
    }

    private fun ensureRoot() {
        check(root.mkdirs() || root.isDirectory) { "Local model directory is unavailable" }
    }

    private fun requireProperty(
        properties: Properties,
        name: String,
    ): String = properties.getProperty(name)?.trim()?.takeIf(String::isNotEmpty)
        ?: throw IllegalStateException("Active model metadata is missing $name")

    private fun LocalModelMetadata.toSpec(path: String): LocalModelSpec = LocalModelSpec(
        id = id,
        version = version,
        quantization = quantization,
        sha256 = sha256,
        path = path,
        maxContextTokens = maxContextTokens,
        backend = backend,
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
        private const val MAX_MODEL_BYTES = 8L * 1024 * 1024 * 1024
    }
}
