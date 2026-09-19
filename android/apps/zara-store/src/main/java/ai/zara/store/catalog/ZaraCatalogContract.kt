package ai.zara.store.catalog

private val SHA256 = Regex("[0-9a-f]{64}")

private fun requireSha256(value: String, label: String) {
    require(value.matches(SHA256)) { "$label must be a lowercase SHA-256 digest" }
}

enum class ZaraPackageKind {
    APP,
    ANDROID_PLUGIN,
    WEAR_APP,
    SUPPORT,
}

data class FdroidPackageIdentity(
    val packageName: String,
    val versionCode: Long,
    val apkSha256: String,
    val signerSha256: String,
) {
    init {
        require(packageName.isNotBlank() && packageName.contains('.') && packageName.none(Char::isWhitespace)) {
            "F-Droid package name is invalid"
        }
        require(versionCode > 0) { "F-Droid version code must be positive" }
        requireSha256(apkSha256, "APK hash")
        requireSha256(signerSha256, "APK signer")
    }
}

data class ZaraCatalogMetadata(
    val packageName: String,
    val versionCode: Long,
    val apkSha256: String,
    val acceptedSignerSha256: Set<String>,
    val kind: ZaraPackageKind,
    val sourceRepo: String,
    val sourceSha: String,
    val protocols: Set<String> = emptySet(),
    val capabilities: Set<String> = emptySet(),
) {
    init {
        require(packageName.isNotBlank()) { "Zara package name is required" }
        require(versionCode > 0) { "Zara version code must be positive" }
        requireSha256(apkSha256, "Zara APK hash")
        require(acceptedSignerSha256.isNotEmpty()) { "At least one accepted signer is required" }
        acceptedSignerSha256.forEach { requireSha256(it, "Zara accepted signer") }
        require(sourceRepo.isNotBlank()) { "Source repository is required" }
        require(sourceSha.matches(Regex("[0-9a-f]{40}"))) { "Source SHA must be an immutable git SHA" }
        require(protocols.size <= 32) { "Too many package protocols" }
        require(capabilities.size <= 256) { "Too many package capabilities" }
        if (kind == ZaraPackageKind.ANDROID_PLUGIN) {
            require(protocols.any { it.startsWith("ZARA-ANDROID-PLUGIN/") }) {
                "Android plugins must declare a ZARA-ANDROID-PLUGIN protocol"
            }
        }
    }
}

sealed interface CatalogOverlayResult {
    data class Ordinary(
        val identity: FdroidPackageIdentity,
    ) : CatalogOverlayResult

    data class Enriched(
        val identity: FdroidPackageIdentity,
        val metadata: ZaraCatalogMetadata,
    ) : CatalogOverlayResult

    data class Rejected(
        val identity: FdroidPackageIdentity,
        val reason: String,
    ) : CatalogOverlayResult
}

object ZaraCatalogOverlay {
    fun merge(
        identity: FdroidPackageIdentity,
        metadata: ZaraCatalogMetadata?,
    ): CatalogOverlayResult {
        if (metadata == null) return CatalogOverlayResult.Ordinary(identity)
        if (identity.packageName != metadata.packageName) {
            return CatalogOverlayResult.Rejected(identity, "package_name_mismatch")
        }
        if (identity.versionCode != metadata.versionCode) {
            return CatalogOverlayResult.Rejected(identity, "version_code_mismatch")
        }
        if (identity.apkSha256 != metadata.apkSha256) {
            return CatalogOverlayResult.Rejected(identity, "apk_hash_mismatch")
        }
        if (identity.signerSha256 !in metadata.acceptedSignerSha256) {
            return CatalogOverlayResult.Rejected(identity, "signer_mismatch")
        }
        return CatalogOverlayResult.Enriched(identity, metadata)
    }
}

/**
 * Store-owned artifact lifecycle only.
 *
 * Plugin enabled/trusted/permission/runtime readiness is canonical host state and
 * must be projected from that authority when rendered; Zara Store must not keep
 * or mutate a parallel plugin registry.
 */
data class StorePackageState(
    val discovered: Boolean = true,
    val downloaded: Boolean = false,
    val verified: Boolean = false,
    val installed: Boolean = false,
) {
    init {
        if (downloaded) require(discovered) { "Downloaded package must be discovered" }
        if (verified) require(downloaded) { "Verified package must be downloaded" }
        if (installed) require(verified) { "Installed package must come from a verified artifact" }
    }

    fun markDownloaded(): StorePackageState = copy(downloaded = true)

    fun markVerified(): StorePackageState {
        require(downloaded) { "Package must be downloaded before verification" }
        return copy(verified = true)
    }
}

data class ObservedInstallation(
    val packageName: String,
    val versionCode: Long,
    val signerSha256: String,
) {
    init {
        requireSha256(signerSha256, "Observed package signer")
    }
}

sealed interface InstallObservationResult {
    data class Accepted(val state: StorePackageState) : InstallObservationResult
    data class Rejected(val reason: String) : InstallObservationResult
}

object StoreInstallVerifier {
    fun observe(
        expected: FdroidPackageIdentity,
        current: StorePackageState,
        observed: ObservedInstallation,
        kind: ZaraPackageKind,
    ): InstallObservationResult {
        if (!current.verified) return InstallObservationResult.Rejected("artifact_not_verified")
        if (expected.packageName != observed.packageName) {
            return InstallObservationResult.Rejected("installed_package_mismatch")
        }
        if (expected.versionCode != observed.versionCode) {
            return InstallObservationResult.Rejected("installed_version_mismatch")
        }
        if (expected.signerSha256 != observed.signerSha256) {
            return InstallObservationResult.Rejected("installed_signer_mismatch")
        }

        // Package kind affects catalog semantics, never Store-owned runtime authority.
        kind.name
        return InstallObservationResult.Accepted(current.copy(installed = true))
    }
}
