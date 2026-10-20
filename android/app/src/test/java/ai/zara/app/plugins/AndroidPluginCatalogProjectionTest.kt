package ai.zara.app.plugins

import org.junit.Test

class AndroidPluginCatalogProjectionTest {
    @Test
    fun readyRequiresTrustedAndEnabledAuthority() {
        expectFailure<IllegalArgumentException> {
            item(trusted = false, enabled = true, health = AndroidPluginCatalogHealth.READY)
        }
        expectFailure<IllegalArgumentException> {
            item(trusted = true, enabled = false, health = AndroidPluginCatalogHealth.READY)
        }

        val ready = item(trusted = true, enabled = true, health = AndroidPluginCatalogHealth.READY)
        check(ready.health == AndroidPluginCatalogHealth.READY)
    }

    @Test
    fun unavailableHostCannotPublishInventedPluginRows() {
        val unavailable = AndroidPluginCatalogSnapshot.unavailable()
        check(!unavailable.hostAvailable)
        check(unavailable.plugins.isEmpty())

        expectFailure<IllegalArgumentException> {
            AndroidPluginCatalogSnapshot(
                generation = 1,
                hostAvailable = false,
                plugins = listOf(item()),
                reasonCode = "host_missing",
            )
        }
    }

    @Test
    fun catalogRejectsDuplicateAndUnboundedOrSecretShapedMetadata() {
        val plugin = item()
        expectFailure<IllegalArgumentException> {
            AndroidPluginCatalogSnapshot(2, true, listOf(plugin, plugin))
        }
        expectFailure<IllegalArgumentException> {
            item(capabilities = List(65) { "capability" })
        }
        expectFailure<IllegalArgumentException> {
            item(permissions = listOf("permission with spaces"))
        }
        expectFailure<IllegalArgumentException> {
            item(diagnosticCode = "raw diagnostic body\nsecret=value")
        }
    }

    @Test
    fun unavailableProjectionIsObservationOnlyAndStable() {
        val source = UnavailableAndroidPluginCatalogProjectionSource()
        val first = source.snapshot()
        var observed: AndroidPluginCatalogSnapshot? = null
        source.observe { observed = it }.close()
        check(first == observed)
        check(first.reasonCode == "android_plugin_host_not_wired")
    }

    private fun item(
        trusted: Boolean = false,
        enabled: Boolean = false,
        health: AndroidPluginCatalogHealth = AndroidPluginCatalogHealth.INSTALLED,
        capabilities: List<String> = listOf("notes.read"),
        permissions: List<String> = listOf("android.notification"),
        diagnosticCode: String? = null,
    ) = AndroidPluginCatalogItem(
        pluginId = "example.plugin",
        displayName = "Example plugin",
        version = "1.0.0",
        source = AndroidPluginCatalogSource.LOCAL,
        trusted = trusted,
        enabled = enabled,
        health = health,
        capabilities = capabilities,
        permissions = permissions,
        diagnosticCode = diagnosticCode,
    )

    private inline fun <reified T : Throwable> expectFailure(block: () -> Unit) {
        try {
            block()
        } catch (failure: Throwable) {
            check(failure is T) { "Expected ${T::class.java.name}, got $failure" }
            return
        }
        error("Expected ${T::class.java.name}")
    }
}
