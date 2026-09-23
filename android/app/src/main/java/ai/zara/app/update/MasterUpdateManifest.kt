package ai.zara.app.update

class MasterUpdateManifest private constructor(
    val release: UpdateRelease,
    val versionName: String,
    val versionCode: Long,
) {
    fun isUpdateFor(
        currentSourceSha: String,
        currentVersion: String,
        currentVersionCode: Long,
    ): Boolean {
        if (currentVersionCode !in 1..MAX_VERSION_CODE) return false
        if (!currentSourceSha.matches(sourceShaPattern)) return false
        if (!currentVersion.matches(versionNamePattern)) return false
        if (release.sourceSha == currentSourceSha || versionCode < currentVersionCode) return false
        return !UpdateSecurity.isNewer(currentVersion, versionName)
    }

    companion object {
        const val CHANNEL = "android-latest"
        const val APK_NAME = "zara-latest.apk"
        const val MANIFEST_NAME = "zara-latest.manifest.txt"
        const val DOWNLOAD_PREFIX =
            "https://github.com/lost-rob0t/zara/releases/download/android-latest/"
        const val MAX_MANIFEST_BYTES = 8192
        private const val MAX_VERSION_CODE = 2100000000L
        private const val MAX_FIELDS = 32
        private val keyPattern = Regex("[a-z][a-z0-9_]{0,63}")
        private val sourceShaPattern = Regex("[0-9a-f]{40}")
        private val versionCodePattern = Regex("[1-9][0-9]{0,9}")
        private val versionNamePattern = Regex(
            "^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)" +
                "(?:-([0-9A-Za-z.-]+))?(?:\\+([0-9A-Za-z.-]+))?$",
        )

        fun parse(text: String, apkUrl: String): Result<MasterUpdateManifest> = runCatching {
            require(text.length <= MAX_MANIFEST_BYTES) {
                "Master update manifest is too large"
            }
            require(text.all { it == '\n' || it == '\r' || it in ' '..'~' }) {
                "Master update manifest contains invalid characters"
            }
            val fields = linkedMapOf<String, String>()
            for (line in text.lineSequence().filter(String::isNotBlank)) {
                require(fields.size < MAX_FIELDS) {
                    "Master update manifest has too many fields"
                }
                val parts = line.split('=', limit = 2)
                require(parts.size == 2 && parts[0].matches(keyPattern) && parts[1].isNotEmpty()) {
                    "Master update manifest field is malformed"
                }
                require(fields.put(parts[0], parts[1]) == null) {
                    "Master update manifest repeats a field"
                }
            }
            require(fields["schema"] == "1" && fields["channel"] == CHANNEL) {
                "Master update manifest has the wrong schema or channel"
            }
            require(fields["mutable"] == "true") {
                "Master update manifest must declare a rolling channel"
            }
            require(fields["phone_apk"] == APK_NAME && apkUrl == "$DOWNLOAD_PREFIX$APK_NAME") {
                "Master update must use the official phone APK"
            }
            val versionName = checkNotNull(fields["version_name"]) {
                "Master update version is missing"
            }
            require(versionName.matches(versionNamePattern)) {
                "Master update version is invalid"
            }
            val codeText = checkNotNull(fields["version_code"]) {
                "Master update version code is missing"
            }
            require(codeText.matches(versionCodePattern)) {
                "Master update version code is invalid"
            }
            val versionCode = codeText.toLongOrNull()
            require(versionCode != null && versionCode in 1..MAX_VERSION_CODE) {
                "Master update version code is invalid"
            }
            val release = UpdateRelease(
                version = "master",
                sourceSha = checkNotNull(fields["source_sha"]) {
                    "Master update source SHA is missing"
                },
                apkUrl = apkUrl,
                sha256 = checkNotNull(fields["phone_sha256"]) {
                    "Master update checksum is missing"
                },
                channel = UpdateChannel.Master,
            )
            MasterUpdateManifest(
                release = UpdateSecurity.validate(release).getOrThrow(),
                versionName = versionName,
                versionCode = versionCode,
            )
        }
    }
}
