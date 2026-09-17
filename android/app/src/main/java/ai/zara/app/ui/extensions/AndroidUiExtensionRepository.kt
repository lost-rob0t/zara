package ai.zara.app.ui.extensions

import ai.zara.app.runtime.LocalQueryResult
import android.util.JsonReader
import java.io.File
import java.io.FileReader
import java.util.concurrent.CompletableFuture

class AndroidUiExtensionRepository(
    private val root: File,
    private val prologQuery: (String) -> CompletableFuture<LocalQueryResult>,
) {
    init {
        check(root.mkdirs() || root.isDirectory) { "Android UI config directory is unavailable" }
    }

    fun load(): CompletableFuture<List<UiContribution>> {
        val registry = UiExtensionRegistry()
        loadPortablePython(registry)
        loadPluginManifests(registry)

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

    private fun loadPluginManifests(registry: UiExtensionRegistry) {
        val pluginRoot = File(root, "plugins")
        if (!pluginRoot.isDirectory) return
        val owners = mutableSetOf<String>()
        pluginRoot.walkTopDown()
            .filter { file ->
                file.isFile && (file.name == "ui.json" || file.name.endsWith(".ui.json"))
            }
            .sortedBy(File::getAbsolutePath)
            .forEach { file ->
                val manifest = runCatching { readManifest(file) }.getOrNull() ?: return@forEach
                if (!manifest.enabled) return@forEach
                val owner = "plugin:${manifest.plugin}"
                if (!owners.add(owner)) return@forEach
                runCatching { registry.replaceOwner(owner, manifest.contributions) }
            }
    }

    private fun readManifest(file: File): PluginUiManifest {
        require(file.length() <= MAX_MANIFEST_BYTES) { "UI manifest is too large" }
        FileReader(file).use { reader ->
            JsonReader(reader).use { json ->
                var apiVersion: String? = null
                var plugin: String? = null
                var enabled = true
                var contributions = emptyList<UiContribution>()
                json.beginObject()
                while (json.hasNext()) {
                    when (json.nextName()) {
                        "api_version" -> apiVersion = json.nextString()
                        "plugin" -> plugin = json.nextString()
                        "enabled" -> enabled = json.nextBoolean()
                        "contributions" -> contributions = readContributions(json)
                        else -> json.skipValue()
                    }
                }
                json.endObject()
                require(apiVersion == UI_MANIFEST_API_VERSION) { "unsupported UI manifest api_version" }
                val name = requireNotNull(plugin) { "UI manifest plugin is required" }
                require(name.matches(Regex("[a-zA-Z0-9][a-zA-Z0-9._-]{0,63}"))) {
                    "UI manifest plugin name is invalid"
                }
                return PluginUiManifest(name, enabled, contributions)
            }
        }
    }

    private fun readContributions(json: JsonReader): List<UiContribution> {
        val output = mutableListOf<UiContribution>()
        json.beginArray()
        while (json.hasNext()) {
            var id: String? = null
            var slot: String? = null
            var kind: String? = null
            var label: String? = null
            var action = ""
            var priority = 100
            var platforms = listOf("desktop", "android")
            json.beginObject()
            while (json.hasNext()) {
                when (json.nextName()) {
                    "id" -> id = json.nextString()
                    "slot" -> slot = json.nextString()
                    "kind" -> kind = json.nextString()
                    "label" -> label = json.nextString()
                    "action" -> action = json.nextString()
                    "priority" -> priority = json.nextInt()
                    "platforms" -> platforms = readStringList(json)
                    else -> json.skipValue()
                }
            }
            json.endObject()
            output += UiContribution(
                id = requireNotNull(id) { "UI contribution id is required" },
                slot = UiSlot.fromWire(requireNotNull(slot) { "UI contribution slot is required" }),
                kind = UiContributionKind.fromWire(requireNotNull(kind) { "UI contribution kind is required" }),
                label = requireNotNull(label) { "UI contribution label is required" },
                action = action,
                priority = priority,
                platforms = platforms.map(UiPlatform::fromWire).toSet(),
            )
        }
        json.endArray()
        return output
    }

    private fun readStringList(json: JsonReader): List<String> {
        val values = mutableListOf<String>()
        json.beginArray()
        while (json.hasNext()) values += json.nextString()
        json.endArray()
        return values
    }

    private data class PluginUiManifest(
        val plugin: String,
        val enabled: Boolean,
        val contributions: List<UiContribution>,
    )

    companion object {
        private const val UI_MANIFEST_API_VERSION = "1"
        private const val MAX_INIT_BYTES = 256 * 1024
        private const val MAX_MANIFEST_BYTES = 256 * 1024
    }
}
