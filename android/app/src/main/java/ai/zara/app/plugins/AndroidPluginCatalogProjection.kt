package ai.zara.app.plugins

private val PLUGIN_ID = Regex("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")
private val PLUGIN_TOKEN = Regex("[A-Za-z0-9][A-Za-z0-9._:-]{0,95}")
private val DIAGNOSTIC_CODE = Regex("[a-z0-9][a-z0-9._-]{0,79}")

internal enum class AndroidPluginCatalogSource(val label: String) {
    LOCAL("local"),
    REMOTE("remote"),
}

internal enum class AndroidPluginCatalogHealth(val label: String) {
    INSTALLED("installed"),
    DISABLED("disabled"),
    PERMISSION_REQUIRED("permission required"),
    READY("ready"),
    DEGRADED("degraded"),
    INCOMPATIBLE("incompatible"),
    DISCONNECTED("disconnected"),
}

internal data class AndroidPluginCatalogItem(
    val pluginId: String,
    val displayName: String,
    val version: String,
    val source: AndroidPluginCatalogSource,
    val trusted: Boolean,
    val enabled: Boolean,
    val health: AndroidPluginCatalogHealth,
    val capabilities: List<String> = emptyList(),
    val permissions: List<String> = emptyList(),
    val diagnosticCode: String? = null,
) {
    init {
        require(PLUGIN_ID.matches(pluginId)) { "Plugin id is invalid." }
        require(displayName.isNotBlank() && displayName.length <= 80 && displayName.isPlainUiText()) {
            "Plugin display name is invalid."
        }
        require(version.isNotBlank() && version.length <= 80 && version.isPlainUiText()) {
            "Plugin version is invalid."
        }
        require(capabilities.size <= 64 && capabilities.all(PLUGIN_TOKEN::matches)) {
            "Plugin capability summary is invalid."
        }
        require(permissions.size <= 32 && permissions.all(PLUGIN_TOKEN::matches)) {
            "Plugin permission summary is invalid."
        }
        require(diagnosticCode == null || DIAGNOSTIC_CODE.matches(diagnosticCode)) {
            "Plugin diagnostic code is invalid."
        }
        require(health != AndroidPluginCatalogHealth.READY || (trusted && enabled)) {
            "A ready plugin must be both trusted and enabled."
        }
        require(health != AndroidPluginCatalogHealth.DISABLED || !enabled) {
            "A disabled plugin cannot be marked enabled."
        }
    }

    private fun String.isPlainUiText(): Boolean =
        all { character -> character.code >= 0x20 && character.code != 0x7f }
}

internal data class AndroidPluginCatalogSnapshot(
    val generation: Long,
    val hostAvailable: Boolean,
    val plugins: List<AndroidPluginCatalogItem>,
    val reasonCode: String? = null,
) {
    init {
        require(generation >= 0) { "Plugin catalog generation must be non-negative." }
        require(plugins.size <= 256) { "Plugin catalog is too large." }
        require(plugins.map { it.pluginId }.toSet().size == plugins.size) {
            "Plugin catalog contains duplicate identities."
        }
        require(hostAvailable || plugins.isEmpty()) {
            "Unavailable plugin host cannot publish plugin state."
        }
        require(reasonCode == null || DIAGNOSTIC_CODE.matches(reasonCode)) {
            "Plugin catalog reason code is invalid."
        }
    }

    companion object {
        fun unavailable(reasonCode: String = "android_plugin_host_not_wired") =
            AndroidPluginCatalogSnapshot(
                generation = 0,
                hostAvailable = false,
                plugins = emptyList(),
                reasonCode = reasonCode,
            )
    }
}

/**
 * Presentation-only projection of canonical ZARA-ANDROID-PLUGIN/1 host state.
 *
 * Implementations may observe the host, but this interface cannot discover packages,
 * grant trust, enable a plugin, authorize capabilities, or execute plugin actions.
 */
internal interface AndroidPluginCatalogProjectionSource {
    fun snapshot(): AndroidPluginCatalogSnapshot
    fun observe(observer: (AndroidPluginCatalogSnapshot) -> Unit): AutoCloseable
}

/**
 * Honest default until the canonical Android plugin host (#924) supplies observations.
 */
internal class UnavailableAndroidPluginCatalogProjectionSource :
    AndroidPluginCatalogProjectionSource {
    private val unavailable = AndroidPluginCatalogSnapshot.unavailable()

    override fun snapshot(): AndroidPluginCatalogSnapshot = unavailable

    override fun observe(observer: (AndroidPluginCatalogSnapshot) -> Unit): AutoCloseable {
        observer(unavailable)
        return AutoCloseable { }
    }
}
