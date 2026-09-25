package ai.zara.app.update

class VersionedUpdateManifest private constructor(
    override val release: UpdateRelease,
    override val versionName: String,
    override val versionCode: Long,
    val apkName: String,
) : UpdateApkProvenance {
    companion object {
        const val MAX_MANIFEST_BYTES = 8192
        private const val MAX_VERSION_CODE = 2100000000L
        private const val MAX_FIELDS = 16
        private val keyPattern = Regex("[a-z][a-z0-9_]{0,63}")
        private val versionCodePattern = Regex("[1-9][0-9]{0,9}")
        private val requiredFields = setOf(
            "schema",
            "source_sha",
            "version_name",
            "version_code",
            "apk",
            "apk_sha256",
        )

        fun apkName(version: String): String = "zara-android-$version.apk"

        fun checksumName(version: String): String = "${apkName(version)}.sha256"

        fun manifestName(version: String): String = "zara-android-$version.manifest.txt"

        fun parse(
            text: String,
            expectedVersion: String,
            apkUrl: String,
        ): Result<VersionedUpdateManifest> = runCatching {
            require(text.length <= MAX_MANIFEST_BYTES) {
                "Versioned update manifest is too large"
            }
            require(text.all { it == '\n' || it == '\r' || it in ' '..'~' }) {
                "Versioned update manifest contains invalid characters"
            }
            val fields = linkedMapOf<String, String>()
            for (line in text.lineSequence().filter(String::isNotBlank)) {
                require(fields.size < MAX_FIELDS) {
                    "Versioned update manifest has too many fields"
                }
                val parts = line.split('=', limit = 2)
                require(parts.size == 2 && parts[0].matches(keyPattern) && parts[1].isNotEmpty()) {
                    "Versioned update manifest field is malformed"
                }
                require(fields.put(parts[0], parts[1]) == null) {
                    "Versioned update manifest repeats a field"
                }
            }
            require(fields.keys == requiredFields) {
                "Versioned update manifest fields are incomplete or unknown"
            }
            require(fields["schema"] == "1") {
                "Versioned update manifest has the wrong schema"
            }
            val versionName = checkNotNull(fields["version_name"]) {
                "Versioned update version is missing"
            }
            require(versionName == expectedVersion) {
                "Versioned update version does not match the release tag"
            }
            val expectedApk = apkName(expectedVersion)
            require(fields["apk"] == expectedApk) {
                "Versioned update must use the canonical Zara phone APK"
            }
            val codeText = checkNotNull(fields["version_code"]) {
                "Versioned update version code is missing"
            }
            require(codeText.matches(versionCodePattern)) {
                "Versioned update version code is invalid"
            }
            val versionCode = codeText.toLongOrNull()
            require(versionCode != null && versionCode in 1..MAX_VERSION_CODE) {
                "Versioned update version code is invalid"
            }
            val release = UpdateRelease(
                version = expectedVersion,
                sourceSha = checkNotNull(fields["source_sha"]) {
                    "Versioned update source SHA is missing"
                },
                apkUrl = apkUrl,
                sha256 = checkNotNull(fields["apk_sha256"]) {
                    "Versioned update checksum is missing"
                },
                channel = UpdateChannel.Versioned,
            )
            VersionedUpdateManifest(
                release = UpdateSecurity.validate(release).getOrThrow(),
                versionName = versionName,
                versionCode = versionCode,
                apkName = expectedApk,
            )
        }
    }
}
