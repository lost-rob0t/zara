package ai.zara.app.update

import org.json.JSONObject

object UpdateCatalog {
    fun select(choices: List<UpdateRelease>, channel: UpdateChannel, selectedId: String?): UpdateRelease? {
        val matching = choices.filter { it.channel == channel }
        return matching.firstOrNull { it.selectionId == selectedId } ?: matching.firstOrNull()
    }

    fun versioned(json: JSONObject, readManifest: (String) -> String): UpdateRelease? {
        if (json.optBoolean("draft", false)) return null
        val tag = json.optString("tag_name")
        if (!tag.matches(Regex("v\\d+\\.\\d+\\.\\d+(?:-[0-9A-Za-z.-]+)?"))) return null
        val version = tag.removePrefix("v")
        val apkName = "zara-android-$version.apk"
        val manifestName = "zara-android-$version.manifest.txt"
        val assets = json.optJSONArray("assets") ?: return null
        val entries = (0 until assets.length()).map { assets.getJSONObject(it) }
        val apk = entries.firstOrNull { it.optString("name") == apkName } ?: return null
        val manifestAsset = entries.firstOrNull { it.optString("name") == manifestName } ?: return null
        val manifestUrl = UpdateSecurity.requireTrustedTransport(manifestAsset.getString("browser_download_url"))
        val fields = parseManifest(readManifest(manifestUrl))
        require(fields["apk"] == apkName) { "Release manifest names a different APK" }
        require(fields["version_name"] == version) { "Release tag and manifest version differ" }
        return UpdateSecurity.validate(UpdateRelease(
            version = version,
            sourceSha = fields.getValue("source_sha"),
            apkUrl = apk.getString("browser_download_url"),
            sha256 = fields.getValue("apk_sha256"),
            versionCode = versionCode(fields),
        )).getOrThrow()
    }

    fun master(text: String, manifestUrl: String): UpdateRelease {
        val fields = parseManifest(text)
        require(fields["channel"] == "android-latest") { "Rolling master channel is invalid" }
        require(fields["mutable"] == "true") { "Rolling master manifest must declare mutability" }
        require(fields["phone_apk"] == "zara-latest.apk") { "Rolling master APK name is unexpected" }
        val productVersion = fields.getValue("version_name")
        require(productVersion.matches(Regex("\\d+\\.\\d+\\.\\d+(?:-[0-9A-Za-z.-]+)?"))) {
            "Rolling master product version is invalid"
        }
        return UpdateSecurity.validate(UpdateRelease(
            version = "master",
            sourceSha = fields.getValue("source_sha"),
            apkUrl = manifestUrl.substringBeforeLast('/') + "/zara-latest.apk",
            sha256 = fields.getValue("phone_sha256"),
            channel = UpdateChannel.Master,
            versionCode = versionCode(fields),
            productVersion = productVersion,
        )).getOrThrow()
    }

    private fun parseManifest(text: String): Map<String, String> {
        val fields = linkedMapOf<String, String>()
        for (line in text.lineSequence().map(String::trim).filter { it.isNotEmpty() && !it.startsWith("#") }) {
            val separator = line.indexOf('=')
            require(separator > 0) { "Update manifest is malformed" }
            val key = line.substring(0, separator)
            require(fields.put(key, line.substring(separator + 1)) == null) { "Duplicate manifest field: $key" }
        }
        require(fields["schema"] == "1") { "Update manifest schema is unsupported" }
        return fields
    }

    private fun versionCode(fields: Map<String, String>): Int {
        val code = fields["version_code"]?.toIntOrNull()
        require(code != null && code > 0) { "Release version code is invalid" }
        return code
    }
}
