package ai.zara.app.ui.extensions

import ai.zara.app.runtime.LocalQueryResult
import java.io.File
import java.util.concurrent.CompletableFuture

/**
 * Trusted host projection for one Android plugin.
 *
 * The ZARA-ANDROID-PLUGIN/1 host owns discovery, signer/protocol validation,
 * enablement and generation fencing. The UI layer only accepts this inert
 * projection after those checks have been made.
 */
data class AndroidPluginUiProjection(
    val pluginId: String,
    val trusted: Boolean,
    val enabled: Boolean,
    val generation: Long,
    val contributions: List<UiContribution>,
) {
    init {
        require(pluginId.matches(Regex("[a-zA-Z0-9][a-zA-Z0-9._-]{0,63}"))) {
            "Android plugin UI id is invalid"
        }
        require(generation >= 0) { "Android plugin UI generation must be non-negative" }
        require(contributions.size <= 256) { "Android plugin UI contribution set is too large" }
    }
}

class AndroidUiExtensionRepository(
    private val root: File,
    private val prologQuery: (String) -> CompletableFuture<LocalQueryResult>,
    private val pluginProjectionProvider: () -> List<AndroidPluginUiProjection> = { emptyList() },
) {
    init {
        check(root.mkdirs() || root.isDirectory) { "Android UI config directory is unavailable" }
    }

    fun load(): CompletableFuture<List<UiContribution>> {
        val registry = UiExtensionRegistry()
        runCatching { loadPortablePython(registry) }
        runCatching { loadPluginProjection(registry) }

        return prologQuery("zara_ui(Result)").handle { result, error ->
            if (error == null && result != null) {
                runCatching {
                    val contributions = result.terms.map(PrologUiTermParser::parse)
                    registry.replaceOwner("user:init.pl", contributions)
                }
            }
            registry.forPlatform(UiPlatform.ANDROID)
        }
    }

    private fun loadPortablePython(registry: UiExtensionRegistry) {
        val init = File(root, "init.py")
        if (!init.isFile) return
        val source = init.readText()
        require(source.encodeToByteArray().size <= MAX_INIT_BYTES) { "init.py is too large" }
        val contributions = PortablePythonUiInitParser.parse(source)
        registry.replaceOwner("user:init.py", contributions)
    }

    private fun loadPluginProjection(registry: UiExtensionRegistry) {
        val staged = UiExtensionRegistry()
        val seen = mutableSetOf<String>()
        pluginProjectionProvider()
            .sortedWith(compareBy<AndroidPluginUiProjection> { it.pluginId }.thenBy { it.generation })
            .forEach { projection ->
                require(seen.add(projection.pluginId)) {
                    "duplicate Android plugin UI projection: ${projection.pluginId}"
                }
                if (!projection.trusted || !projection.enabled) return@forEach
                staged.replaceOwner("plugin:${projection.pluginId}", projection.contributions)
            }
        staged.snapshot()
            .groupBy { it.owner }
            .forEach { (owner, contributions) ->
                registry.replaceOwner(owner, contributions)
            }
    }

    companion object {
        private const val MAX_INIT_BYTES = 256 * 1024
    }
}
