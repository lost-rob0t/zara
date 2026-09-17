package ai.zara.store.catalog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraCatalogContractTest {
    private val identity = FdroidPackageIdentity(
        packageName = "ai.zara.notes",
        versionCode = 7,
        apkSha256 = sha('a'),
        signerSha256 = sha('b'),
    )

    @Test
    fun ordinaryFdroidPackageDoesNotRequireZaraMetadata() {
        val result = ZaraCatalogOverlay.merge(identity, null)
        assertTrue(result is CatalogOverlayResult.Ordinary)
    }

    @Test
    fun matchingZaraMetadataAugmentsExactFdroidIdentity() {
        val result = ZaraCatalogOverlay.merge(identity, pluginMetadata())
        assertTrue(result is CatalogOverlayResult.Enriched)
    }

    @Test
    fun mismatchedHashCannotElevatePackageIntoZaraPlugin() {
        val result = ZaraCatalogOverlay.merge(
            identity,
            pluginMetadata().copy(apkSha256 = sha('c')),
        )
        assertEquals(
            "apk_hash_mismatch",
            (result as CatalogOverlayResult.Rejected).reason,
        )
    }

    @Test
    fun mismatchedSignerCannotElevatePackageIntoZaraPlugin() {
        val result = ZaraCatalogOverlay.merge(
            identity,
            pluginMetadata().copy(acceptedSignerSha256 = setOf(sha('c'))),
        )
        assertEquals(
            "signer_mismatch",
            (result as CatalogOverlayResult.Rejected).reason,
        )
    }

    @Test
    fun installRequiresVerifiedArtifactAndObservedPackageIdentity() {
        val downloaded = StorePackageState().markDownloaded()
        val observed = ObservedInstallation(
            packageName = identity.packageName,
            versionCode = identity.versionCode,
            signerSha256 = identity.signerSha256,
        )

        val beforeVerification = StoreInstallVerifier.observe(
            expected = identity,
            current = downloaded,
            observed = observed,
            kind = ZaraPackageKind.ANDROID_PLUGIN,
        )
        assertEquals(
            "artifact_not_verified",
            (beforeVerification as InstallObservationResult.Rejected).reason,
        )

        val afterVerification = StoreInstallVerifier.observe(
            expected = identity,
            current = downloaded.markVerified(),
            observed = observed,
            kind = ZaraPackageKind.ANDROID_PLUGIN,
        )
        assertTrue(afterVerification is InstallObservationResult.Accepted)
    }

    @Test
    fun installedPluginIsNotAutomaticallyEnabledTrustedOrReady() {
        val state = StorePackageState()
            .markDownloaded()
            .markVerified()
        val observed = ObservedInstallation(
            packageName = identity.packageName,
            versionCode = identity.versionCode,
            signerSha256 = identity.signerSha256,
        )
        val accepted = StoreInstallVerifier.observe(
            expected = identity,
            current = state,
            observed = observed,
            kind = ZaraPackageKind.ANDROID_PLUGIN,
        ) as InstallObservationResult.Accepted

        assertTrue(accepted.state.installed)
        assertFalse(accepted.state.plugin!!.enabled)
        assertFalse(accepted.state.plugin.trusted)
        assertFalse(accepted.state.plugin.permissionReady)
        assertFalse(accepted.state.plugin.runtimeReady)
    }

    @Test
    fun runtimeReadyCannotSkipPluginAuthorityStates() {
        assertThrows(IllegalArgumentException::class.java) {
            PluginRuntimeState(runtimeReady = true)
        }
    }

    private fun pluginMetadata() = ZaraCatalogMetadata(
        packageName = identity.packageName,
        versionCode = identity.versionCode,
        apkSha256 = identity.apkSha256,
        acceptedSignerSha256 = setOf(identity.signerSha256),
        kind = ZaraPackageKind.ANDROID_PLUGIN,
        sourceRepo = "https://github.com/lost-rob0t/zara",
        sourceSha = "d".repeat(40),
        protocols = setOf("ZARA-ANDROID-PLUGIN/1"),
        capabilities = setOf("notes.read", "notes.write"),
    )

    private fun sha(char: Char): String = char.toString().repeat(64)
}
