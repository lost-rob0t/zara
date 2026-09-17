package ai.zara.app.runtime

import ai.zara.app.model.AndroidCloudModelStorage
import ai.zara.app.model.CloudModelConfig
import ai.zara.app.model.CloudModelCoordinator
import ai.zara.app.model.CloudModelProvider
import ai.zara.app.model.CloudModelPurpose
import ai.zara.app.model.CloudModelState
import ai.zara.app.model.LocalModelConfig
import ai.zara.app.model.LocalModelConfigStore
import ai.zara.app.model.LocalModelCoordinator
import ai.zara.app.model.LocalModelPhase
import ai.zara.app.model.LocalModelState
import ai.zara.app.prolog.PrologProjectIdentityResolver
import ai.zara.app.prolog.PrologQueryPolicy
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.prolog.TreallaBridge
import java.io.File
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

enum class LocalServerPhase { STOPPED, STARTING, READY, RELOADING, FAILED }

data class LocalServerState(
    val phase: LocalServerPhase,
    val generation: Long,
    val loadedSources: List<String>,
    val failure: String? = null,
)

data class LocalQueryResult(
    val query: String,
    val terms: List<String>,
    val generation: Long,
)

class LocalZaraServer(
    private val bridge: TreallaBridge,
    private val corePath: String,
    private val workspace: PrologWorkspace,
    private val localModel: LocalModelCoordinator = defaultLocalModelCoordinator(corePath),
    private val cloudModel: CloudModelCoordinator = defaultCloudModelCoordinator(corePath),
) : AutoCloseable {
    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-server").apply { isDaemon = true }
    }
    @Volatile
    private var current = LocalServerState(LocalServerPhase.STOPPED, 0, emptyList())
    @Volatile
    private var closed = false
    @Volatile
    private var stateObserver: ((LocalServerState) -> Unit)? = null

    fun state(): LocalServerState = current

    fun localModelState(): LocalModelState = localModel.state()

    fun cloudModelState(): CloudModelState = cloudModel.reloadConfig()

    fun setStateObserver(observer: ((LocalServerState) -> Unit)?) {
        stateObserver = observer
        observer?.invoke(current)
    }

    fun start(): CompletableFuture<LocalServerState> = submit {
        check(current.phase == LocalServerPhase.STOPPED) { "Local Zara server is already started" }
        boot(LocalServerPhase.STARTING)
    }

    fun reload(): CompletableFuture<LocalServerState> = submit {
        check(current.phase == LocalServerPhase.READY || current.phase == LocalServerPhase.FAILED) {
            "Local Zara server is not reloadable"
        }
        val wasReady = current.phase == LocalServerPhase.READY
        updateState(current.copy(phase = LocalServerPhase.RELOADING, failure = null))
        if (wasReady) {
            try {
                bridge.shutdown()
            } catch (error: Throwable) {
                return@submit LocalServerState(
                    phase = LocalServerPhase.FAILED,
                    generation = current.generation,
                    loadedSources = emptyList(),
                    failure = error.message ?: "Local runtime shutdown failed",
                ).also(::updateState)
            }
        }
        boot(LocalServerPhase.RELOADING)
    }

    fun query(rawQuery: String): CompletableFuture<LocalQueryResult> {
        val query = try {
            PrologQueryPolicy.requireSafe(rawQuery)
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        return submit {
            check(current.phase == LocalServerPhase.READY) { "Local Zara server is not ready" }
            LocalQueryResult(query, bridge.evaluate(query), current.generation)
        }
    }

    fun resolve(utterance: String): CompletableFuture<LocalQueryResult> {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= 65_536) { "Utterance is too large" }
        providerCommand(text)?.let { return it }
        codingCommand(text)?.let { return it }
        modelCommand(text)?.let { return it }
        val escaped = text
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
        val query = "resolve_frames(\"$escaped\", passive, [], Frames), member(Result, Frames)"
        val symbolic = submit {
            check(current.phase == LocalServerPhase.READY) { "Local Zara server is not ready" }
            LocalQueryResult(query, bridge.evaluate(query), current.generation)
        }
        return symbolic.thenCompose { result ->
            if (result.terms.isNotEmpty()) return@thenCompose CompletableFuture.completedFuture(result)
            val identity = PrologProjectIdentityResolver.resolve(workspace.listSources())
            if (localModel.state().config.enabled) {
                return@thenCompose localModel.generate(
                    text,
                    systemPrompt = localSystemPrompt(identity.effectiveLlmAppName()),
                ).handle { modelResult, error ->
                    when {
                        modelResult != null -> result.copy(terms = listOf(modelResult.text))
                        error != null -> result.copy(
                            terms = listOf(
                                "Local model unavailable: ${rootMessage(error)}. " +
                                    "The symbolic runtime is still ready.",
                            ),
                        )
                        else -> result
                    }
                }
            }
            val cloud = cloudModel.reloadConfig()
            if (!cloud.config.enabled || cloud.config.provider.codingOnly) {
                return@thenCompose CompletableFuture.completedFuture(result)
            }
            val appName = identity.effectiveLlmAppName(cloud.config.appName)
            cloudModel.generate(
                text,
                purpose = CloudModelPurpose.GENERAL,
                effectiveAppName = appName,
            ).handle { modelResult, error ->
                when {
                    modelResult != null -> result.copy(terms = listOf(modelResult.text))
                    error != null -> result.copy(
                        terms = listOf(
                            "Cloud model unavailable: ${rootMessage(error)}. " +
                                "The symbolic runtime is still ready.",
                        ),
                    )
                    else -> result
                }
            }
        }
    }

    private fun codingCommand(text: String): CompletableFuture<LocalQueryResult>? {
        if (text != "/code" && !text.startsWith("/code ")) return null
        return try {
            val prompt = text.removePrefix("/code").trim()
            require(prompt.isNotEmpty()) { "Usage: /code <coding task>" }
            val cloud = cloudModel.reloadConfig()
            require(cloud.config.enabled) { "Cloud model is disabled" }
            val identity = PrologProjectIdentityResolver.resolve(workspace.listSources())
            val appName = identity.effectiveLlmAppName(cloud.config.appName)
            cloudModel.generate(
                prompt,
                purpose = CloudModelPurpose.CODING,
                effectiveAppName = appName,
            ).thenApply { modelResult ->
                LocalQueryResult(
                    query = text,
                    terms = listOf(modelResult.text),
                    generation = current.generation,
                )
            }
        } catch (error: Throwable) {
            CompletableFuture.failedFuture(error)
        }
    }

    private fun providerCommand(text: String): CompletableFuture<LocalQueryResult>? {
        if (text != "/provider" && !text.startsWith("/provider ")) return null
        return try {
            val arguments = text.split(Regex("\\s+"))
            val message = when (arguments.getOrNull(1)?.lowercase() ?: "status") {
                "status" -> describeCloudModelState(cloudModel.reloadConfig())
                "on" -> {
                    val current = cloudModel.reloadConfig().config
                    val state = cloudModel.configure(current.copy(enabled = true))
                    "Cloud model enabled: ${describeCloudModelState(state)}"
                }
                "off" -> {
                    val current = cloudModel.reloadConfig().config
                    val state = cloudModel.configure(current.copy(enabled = false))
                    "Cloud model disabled: ${describeCloudModelState(state)}"
                }
                "cancel" -> {
                    cloudModel.cancelActive()
                    "Cloud model generation cancelled"
                }
                "use" -> configureProvider(arguments)
                "app" -> {
                    val appName = text.substringAfter("/provider app", "").trim()
                    require(appName.isNotEmpty()) { "Usage: /provider app <LLM app name>" }
                    val current = cloudModel.reloadConfig().config
                    val state = cloudModel.configure(current.copy(appName = appName))
                    "Cloud LLM app name updated: ${state.config.appName}"
                }
                else -> error(
                    "Unknown provider command. Use /provider status, /provider on, /provider off, " +
                        "/provider cancel, /provider use <preset>, or /provider app <name>. " +
                        "Store API keys in the Model Providers settings screen, never in chat."
                )
            }
            CompletableFuture.completedFuture(
                LocalQueryResult(
                    query = text,
                    terms = listOf(message),
                    generation = current.generation,
                )
            )
        } catch (error: Throwable) {
            CompletableFuture.failedFuture(error)
        }
    }

    private fun configureProvider(arguments: List<String>): String {
        val preset = arguments.getOrNull(2)?.lowercase()
            ?: throw IllegalArgumentException(
                "Usage: /provider use openrouter <model> | zai <model> | starintel <model> | openai <base-url> <model>"
            )
        val previous = cloudModel.reloadConfig().config
        val configured = when (preset) {
            "openrouter" -> {
                val model = arguments.getOrNull(3) ?: error("Usage: /provider use openrouter <model>")
                previous.copy(
                    enabled = true,
                    provider = CloudModelProvider.OPENROUTER,
                    endpoint = CloudModelConfig.OPENROUTER_ENDPOINT,
                    model = model,
                )
            }
            "zai", "z-ai", "z-ai-code", "zai-coding" -> {
                val model = arguments.getOrNull(3) ?: error("Usage: /provider use zai <model>")
                previous.copy(
                    enabled = true,
                    provider = CloudModelProvider.ZAI_CODING_PLAN,
                    endpoint = CloudModelConfig.ZAI_CODING_ENDPOINT,
                    model = model,
                )
            }
            "starintel" -> {
                val model = arguments.getOrNull(3) ?: error("Usage: /provider use starintel <model>")
                previous.copy(
                    enabled = true,
                    provider = CloudModelProvider.OPENAI_COMPATIBLE,
                    endpoint = CloudModelConfig.DEFAULT_STARINTEL_ENDPOINT,
                    model = model,
                )
            }
            "openai", "generic" -> {
                val endpoint = arguments.getOrNull(3)
                    ?: error("Usage: /provider use openai <base-url> <model>")
                val model = arguments.getOrNull(4)
                    ?: error("Usage: /provider use openai <base-url> <model>")
                previous.copy(
                    enabled = true,
                    provider = CloudModelProvider.OPENAI_COMPATIBLE,
                    endpoint = endpoint,
                    model = model,
                )
            }
            else -> error("Unknown provider preset: $preset")
        }
        val state = cloudModel.configure(configured)
        return "Cloud model configured: ${describeCloudModelState(state)}"
    }

    private fun modelCommand(text: String): CompletableFuture<LocalQueryResult>? {
        if (text != "/model" && !text.startsWith("/model ")) return null
        return try {
            val arguments = text.split(Regex("\\s+"), limit = 6)
            val message = when (arguments.getOrNull(1)?.lowercase() ?: "status") {
                "status" -> describeModelState(localModel.state())
                "on" -> {
                    val state = localModel.configure(localModel.state().config.copy(enabled = true))
                    "Local model enabled: ${describeModelState(state)}"
                }
                "off" -> {
                    val state = localModel.configure(localModel.state().config.copy(enabled = false))
                    "Local model disabled: ${describeModelState(state)}"
                }
                "cancel" -> {
                    localModel.cancelActive()
                    "Local model generation cancelled"
                }
                "use" -> {
                    require(arguments.size >= 4) {
                        "Usage: /model use <loopback-endpoint> <model> [quantization]"
                    }
                    val previous = localModel.state().config
                    val state = localModel.configure(
                        previous.copy(
                            enabled = true,
                            endpoint = arguments[2],
                            model = arguments[3],
                            quantization = arguments.getOrNull(4),
                        )
                    )
                    "Local model configured: ${describeModelState(state)}"
                }
                else -> error(
                    "Unknown local model command. Use /model status, /model on, /model off, " +
                        "/model cancel, or /model use <loopback-endpoint> <model> [quantization]"
                )
            }
            CompletableFuture.completedFuture(
                LocalQueryResult(
                    query = text,
                    terms = listOf(message),
                    generation = current.generation,
                )
            )
        } catch (error: Throwable) {
            CompletableFuture.failedFuture(error)
        }
    }

    private fun describeModelState(state: LocalModelState): String {
        val config = state.config
        val quantization = config.quantization?.let { " · $it" }.orEmpty()
        val failure = state.message?.let { " · $it" }.orEmpty()
        return "${state.phase.name.lowercase()} · ${config.model}$quantization · ${config.endpoint}$failure"
    }

    private fun describeCloudModelState(state: CloudModelState): String {
        val config = state.config
        val key = if (state.apiKeyConfigured) "key=stored" else "key=missing"
        val scope = if (config.provider.codingOnly) "coding-only" else "general"
        val failure = state.message?.let { " · $it" }.orEmpty()
        return "${state.phase.name.lowercase()} · ${config.provider.wireName} · $scope · ${config.model.ifBlank { "no-model" }} · ${config.endpoint} · $key$failure"
    }

    private fun localSystemPrompt(appName: String): String =
        "You are $appName's local conversational model. Answer the user directly. " +
            "Do not claim to execute tools, device actions, timers, apps, URLs, or other side effects. " +
            "Those actions are owned by the symbolic runtime."

    private fun boot(phase: LocalServerPhase): LocalServerState {
        updateState(current.copy(phase = phase, failure = null))
        return try {
            bridge.initialize(corePath)
            val sources = workspace.sourceFiles()
            sources.forEach { bridge.consult(it.absolutePath) }
            LocalServerState(
                phase = LocalServerPhase.READY,
                generation = current.generation + 1,
                loadedSources = sources.map { it.name },
            ).also(::updateState)
        } catch (error: Throwable) {
            runCatching { bridge.shutdown() }
            LocalServerState(
                phase = LocalServerPhase.FAILED,
                generation = current.generation,
                loadedSources = emptyList(),
                failure = error.message ?: error::class.java.simpleName,
            ).also(::updateState)
        }
    }

    private fun updateState(state: LocalServerState) {
        current = state
        stateObserver?.invoke(state)
    }

    private fun <T> submit(block: () -> T): CompletableFuture<T> {
        if (closed) return CompletableFuture.failedFuture(IllegalStateException("Local Zara server is closed"))
        val future = CompletableFuture<T>()
        actor.execute {
            try {
                future.complete(block())
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        return future
    }

    override fun close() {
        if (closed) return
        closed = true
        localModel.close()
        cloudModel.close()
        val future = CompletableFuture<Unit>()
        actor.execute {
            try {
                if (current.phase != LocalServerPhase.STOPPED) bridge.shutdown()
                updateState(current.copy(phase = LocalServerPhase.STOPPED))
                future.complete(Unit)
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        runCatching { future.get() }
        actor.shutdownNow()
        stateObserver = null
    }

    companion object {
        private fun defaultLocalModelCoordinator(corePath: String): LocalModelCoordinator {
            val root = File(corePath).parentFile ?: error("Local runtime directory is unavailable")
            return LocalModelCoordinator(
                LocalModelConfigStore(File(root, "local-model.properties"))
            )
        }

        private fun defaultCloudModelCoordinator(corePath: String): CloudModelCoordinator {
            val root = File(corePath).parentFile ?: error("Local runtime directory is unavailable")
            return AndroidCloudModelStorage.coordinator(root)
        }

        private fun rootMessage(error: Throwable): String {
            var current: Throwable = error
            while (current.cause != null && current.cause !== current) {
                current = current.cause!!
            }
            return current.message ?: current::class.java.simpleName
        }
    }
}
