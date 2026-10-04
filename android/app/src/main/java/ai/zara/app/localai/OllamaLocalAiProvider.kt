package ai.zara.app.localai

import java.io.InputStream
import java.net.HttpURLConnection
import java.net.Proxy
import java.net.URI
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import org.json.JSONObject

class OllamaLocalAiProvider(
    endpoint: String = "http://127.0.0.1:11434",
) : LocalAiProvider {
    override val capabilities = LocalAiProviderCapabilities(
        id = "ollama", displayName = "Ollama on this device", offlineOnly = true,
        streaming = true, modelFormats = setOf(LocalModelFormat.GGUF), accelerators = emptySet(),
    )
    private val endpoint = URI(endpoint).also {
        require(it.scheme == "http" && it.host == "127.0.0.1" && it.userInfo == null &&
            it.port in 1..65535 && it.path.isNullOrEmpty() && it.query == null && it.fragment == null) {
            "Local Ollama requires an HTTP endpoint on 127.0.0.1"
        }
    }.toString()
    private val actor = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-ollama").apply { isDaemon = true }
    }
    private val deadlines = Executors.newSingleThreadScheduledExecutor { runnable ->
        Thread(runnable, "zara-local-ollama-deadline").apply { isDaemon = true }
    }
    private val lock = Any()
    @Volatile private var current = LocalAiState(providerId = "ollama")
    private var closed = false
    private var epoch = 0L
    private var connection: HttpURLConnection? = null
    private var active: CompletableFuture<*>? = null
    private var selected: SelectedModel? = null

    private data class SelectedModel(
        val name: String, val digest: String, val quantization: LocalModelQuantization,
    )
    private data class CatalogModel(val name: String, val digest: String, val quantization: String?)

    override fun state(): CompletableFuture<LocalAiState> = CompletableFuture.completedFuture(current)

    fun modelNames(): CompletableFuture<List<String>> = operation { token ->
        tags(token).map { it.name }
    }

    override fun models(): CompletableFuture<List<LocalModelSpec>> = CompletableFuture.completedFuture(emptyList())

    override fun activeModel(): CompletableFuture<LocalModelSpec?> = CompletableFuture.completedFuture(null)

    override fun installModel(source: InputStream, metadata: LocalModelMetadata): CompletableFuture<LocalModelSpec> {
        source.close()
        return CompletableFuture.failedFuture(IllegalStateException("Install Ollama models in the app hosting Ollama"))
    }

    override fun selectModel(id: String, version: String): CompletableFuture<LocalAiState> {
        require(validOllamaModelName(id)) { "Ollama model name is invalid" }
        return operation { token ->
            synchronized(lock) { requireCurrent(token); selected = null }
            update(token, current.copy(phase = LocalAiPhase.LOADING, failure = null, modelName = id))
            val entry = tags(token).firstOrNull { it.name == id }
                ?: error("Ollama model is not installed on this device: $id")
            val digest = entry.digest
            require(version.isEmpty() || version == digest) { "Ollama model digest changed" }
            val quantization = entry.quantization ?: json("/api/show", JSONObject().put("model", id), token)
                .getJSONObject("details").getString("quantization_level")
            val model = SelectedModel(id, digest, LocalModelQuantization.requireKnown(quantization))
            synchronized(lock) {
                requireCurrent(token)
                selected = model
                current = current.copy(phase = LocalAiPhase.READY, generation = token, failure = null, modelName = id)
                current
            }
        }
    }

    override fun generate(request: LocalGenerationRequest, onChunk: (String) -> Unit): CompletableFuture<LocalGenerationResult> =
        operation { token ->
            val model = synchronized(lock) { selected } ?: error("Choose an installed local Ollama model in Settings > Runtime")
            update(token, current.copy(phase = LocalAiPhase.GENERATING, failure = null))
            val body = JSONObject().put("model", model.name).put("prompt", request.prompt).put("stream", true)
                .put("options", JSONObject().put("num_predict", request.maxOutputTokens))
            val output = StringBuilder()
            var done = false
            request("/api/generate", body, token) { input ->
                val reader = input.bufferedReader()
                while (!done) {
                    val line = boundedLine(reader) ?: error("Local Ollama stream ended before completion")
                    val frame = JSONObject(line)
                    check(!frame.has("error")) { "Local Ollama could not generate a response" }
                    val chunk = frame.optString("response", "")
                    check(output.length + chunk.length <= MAX_TEXT) { "Local Ollama output exceeded the limit" }
                    synchronized(lock) {
                        requireCurrent(token)
                        output.append(chunk)
                        if (chunk.isNotEmpty()) onChunk(chunk)
                    }
                    done = frame.getBoolean("done")
                }
            }
            update(token, current.copy(phase = LocalAiPhase.READY, generation = token, failure = null))
            LocalGenerationResult(output.toString(), model.name, model.digest, model.quantization, token)
        }

    override fun cancelGeneration(): CompletableFuture<LocalAiState> {
        synchronized(lock) {
            epoch++
            connection?.disconnect()
            active?.completeExceptionally(CancellationException("Local Ollama operation cancelled"))
            current = current.copy(phase = if (selected == null) LocalAiPhase.STOPPED else LocalAiPhase.READY,
                generation = epoch, failure = null)
            return CompletableFuture.completedFuture(current)
        }
    }

    override fun unloadModel(): CompletableFuture<LocalAiState> {
        cancelGeneration()
        synchronized(lock) {
            selected = null
            current = LocalAiState(generation = epoch, providerId = "ollama")
            return CompletableFuture.completedFuture(current)
        }
    }

    override fun close() {
        synchronized(lock) {
            if (closed) return
            closed = true
            cancelGeneration()
        }
        actor.shutdownNow()
        deadlines.shutdownNow()
    }

    private fun tags(token: Long): List<CatalogModel> {
        val models = json("/api/tags", null, token).getJSONArray("models")
        require(models.length() <= 256) { "Local Ollama model catalog exceeds the limit" }
        return (0 until models.length()).map { index ->
            val item = models.getJSONObject(index)
            val name = item.getString("name")
            val digest = item.getString("digest").removePrefix("sha256:")
            require(validOllamaModelName(name) && digest.matches(Regex("[0-9a-f]{64}"))) {
                "Local Ollama model metadata is invalid"
            }
            CatalogModel(name, digest, item.optJSONObject("details")?.optString("quantization_level")?.takeIf { it.isNotBlank() })
        }
    }

    private fun json(path: String, body: JSONObject?, token: Long): JSONObject = request(path, body, token) { input ->
        val bytes = input.readNBytes(MAX_JSON + 1)
        check(bytes.size <= MAX_JSON) { "Local Ollama response exceeded the limit" }
        JSONObject(bytes.toString(Charsets.UTF_8))
    }

    private fun <T> request(path: String, body: JSONObject?, token: Long, read: (InputStream) -> T): T {
        val http = URI(endpoint + path).toURL().openConnection(Proxy.NO_PROXY) as HttpURLConnection
        synchronized(lock) { requireCurrent(token); connection = http }
        try {
            http.instanceFollowRedirects = false
            http.connectTimeout = 3_000
            http.readTimeout = 5_000
            if (body != null) {
                http.requestMethod = "POST"
                http.doOutput = true
                http.setRequestProperty("Content-Type", "application/json")
                http.outputStream.use { it.write(body.toString().toByteArray(Charsets.UTF_8)) }
            }
            check(http.responseCode == 200) { "Local Ollama returned HTTP ${http.responseCode}" }
            return http.inputStream.use(read)
        } finally {
            synchronized(lock) { if (connection === http) connection = null }
            http.disconnect()
        }
    }

    private fun <T> operation(block: (Long) -> T): CompletableFuture<T> {
        synchronized(lock) {
            if (closed) return CompletableFuture.failedFuture(IllegalStateException("Local Ollama provider is closed"))
            if (active?.isDone == false) return CompletableFuture.failedFuture(IllegalStateException("Local Ollama is busy"))
            val future = CompletableFuture<T>()
            val token = ++epoch
            active = future
            future.whenComplete { _, _ ->
                if (future.isCancelled) synchronized(lock) {
                    if (epoch == token) {
                        epoch++
                        connection?.disconnect()
                        current = current.copy(
                            phase = if (selected == null) LocalAiPhase.STOPPED else LocalAiPhase.READY,
                            generation = epoch, failure = null,
                        )
                    }
                }
            }
            val deadline = deadlines.schedule({
                synchronized(lock) {
                    if (epoch == token && !future.isDone) {
                        epoch++
                        current = current.copy(phase = LocalAiPhase.FAILED, failure = "Local Ollama timed out")
                        connection?.disconnect()
                        future.completeExceptionally(java.util.concurrent.TimeoutException("Local Ollama timed out"))
                    }
                }
            }, 60, TimeUnit.SECONDS)
            actor.execute {
                try { future.complete(block(token)) }
                catch (error: Throwable) {
                    synchronized(lock) {
                        if (epoch == token && !closed) current = current.copy(phase = LocalAiPhase.FAILED,
                            failure = "Local Ollama unavailable. Start Ollama on this device and select an installed model.")
                    }
                    future.completeExceptionally(error)
                } finally { deadline.cancel(false) }
            }
            return future
        }
    }

    private fun update(token: Long, state: LocalAiState) {
        synchronized(lock) { requireCurrent(token); current = state }
    }

    private fun requireCurrent(token: Long) {
        if (closed || epoch != token) throw CancellationException("Stale local Ollama operation")
    }

    private fun boundedLine(reader: java.io.Reader): String? {
        val result = StringBuilder()
        while (true) {
            val next = reader.read()
            if (next < 0) return if (result.isEmpty()) null else result.toString()
            if (next == '\n'.code) return result.toString()
            check(result.length < MAX_JSON) { "Local Ollama stream frame exceeded the limit" }
            result.append(next.toChar())
        }
    }

    private companion object {
        const val MAX_JSON = 65_536
        const val MAX_TEXT = 32_768
    }
}
