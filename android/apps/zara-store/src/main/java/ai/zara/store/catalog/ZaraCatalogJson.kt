package ai.zara.store.catalog

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import com.google.gson.JsonParser
import java.net.URI

private const val CATALOG_SCHEMA_V1 = "ZARA-CATALOG/1"
private const val MAX_CATALOG_BYTES = 4 * 1024 * 1024
private const val MAX_PACKAGES = 10_000
private const val MAX_PROTOCOLS = 32
private const val MAX_CAPABILITIES = 256

private val ROOT_FIELDS = setOf("schema", "generated_at", "repo", "packages")
private val REPO_FIELDS = setOf("name", "fdroid_repo", "revision")
private val PACKAGE_FIELDS = setOf(
    "package_name",
    "version_code",
    "apk_sha256",
    "accepted_signer_sha256",
    "kind",
    "source_repo",
    "source_sha",
    "protocols",
    "capabilities",
)

data class ZaraCatalogRepository(
    val name: String,
    val fdroidRepo: String,
    val revision: String,
)

data class ZaraCatalogDocument(
    val schema: String,
    val generatedAtEpochMs: Long,
    val repository: ZaraCatalogRepository,
    val packages: List<ZaraCatalogMetadata>,
)

/**
 * Decodes the Zara companion catalog without assigning trust to it.
 *
 * Production refresh must verify the catalog bytes/signature and the matching F-Droid package
 * identity before using any metadata returned here for first-party or plugin trust decisions.
 */
object ZaraCatalogJson {
    fun parseUntrusted(json: String): ZaraCatalogDocument {
        require(json.encodeToByteArray().size <= MAX_CATALOG_BYTES) {
            "Zara catalog exceeds $MAX_CATALOG_BYTES bytes"
        }

        val root = JsonParser.parseString(json)
        require(root.isJsonObject) { "Zara catalog root must be an object" }
        val obj = root.asJsonObject
        obj.requireOnlyKeys("catalog", ROOT_FIELDS)

        val schema = obj.requiredString("schema")
        require(schema == CATALOG_SCHEMA_V1) { "Unsupported Zara catalog schema: $schema" }

        val generatedAt = obj.requiredLong("generated_at")
        require(generatedAt >= 0) { "generated_at must be non-negative" }

        val repoObj = obj.requiredObject("repo")
        repoObj.requireOnlyKeys("repo", REPO_FIELDS)
        val repository = ZaraCatalogRepository(
            name = repoObj.requiredString("name").bounded("repo.name", 160),
            fdroidRepo = repoObj.requiredString("fdroid_repo").requireHttpsUrl("repo.fdroid_repo"),
            revision = repoObj.requiredString("revision").bounded("repo.revision", 256),
        )

        val packageArray = obj.requiredArray("packages")
        require(packageArray.size() <= MAX_PACKAGES) { "Too many Zara catalog packages" }

        val identities = mutableSetOf<Pair<String, Long>>()
        val packages = packageArray.mapIndexed { index, element ->
            require(element.isJsonObject) { "packages[$index] must be an object" }
            val packageObj = element.asJsonObject
            packageObj.requireOnlyKeys("packages[$index]", PACKAGE_FIELDS)
            val packageName = packageObj.requiredString("package_name")
            val versionCode = packageObj.requiredLong("version_code")
            require(identities.add(packageName to versionCode)) {
                "Duplicate Zara catalog package identity: $packageName@$versionCode"
            }

            ZaraCatalogMetadata(
                packageName = packageName,
                versionCode = versionCode,
                apkSha256 = packageObj.requiredString("apk_sha256"),
                acceptedSignerSha256 = packageObj.requiredStringSet(
                    name = "accepted_signer_sha256",
                    maxSize = 16,
                ),
                kind = parseKind(packageObj.requiredString("kind")),
                sourceRepo = packageObj.requiredString("source_repo").requireHttpsUrl("source_repo"),
                sourceSha = packageObj.requiredString("source_sha"),
                protocols = packageObj.optionalStringSet("protocols", MAX_PROTOCOLS),
                capabilities = packageObj.optionalStringSet("capabilities", MAX_CAPABILITIES),
            )
        }

        return ZaraCatalogDocument(
            schema = schema,
            generatedAtEpochMs = generatedAt,
            repository = repository,
            packages = packages,
        )
    }

    private fun parseKind(value: String): ZaraPackageKind = when (value) {
        "app" -> ZaraPackageKind.APP
        "android_plugin" -> ZaraPackageKind.ANDROID_PLUGIN
        "wear_app" -> ZaraPackageKind.WEAR_APP
        "support" -> ZaraPackageKind.SUPPORT
        else -> error("Unsupported Zara package kind: $value")
    }
}

private fun JsonObject.requireOnlyKeys(context: String, allowed: Set<String>) {
    val unknown = keySet().filterNot(allowed::contains).sorted()
    require(unknown.isEmpty()) { "$context contains unsupported fields: ${unknown.joinToString()}" }
}

private fun JsonObject.requiredString(name: String): String {
    val element = get(name) ?: error("Missing required Zara catalog field: $name")
    require(element.isJsonPrimitive && element.asJsonPrimitive.isString) {
        "$name must be a string"
    }
    return element.asString
}

private fun JsonObject.requiredLong(name: String): Long {
    val element = get(name) ?: error("Missing required Zara catalog field: $name")
    require(element.isJsonPrimitive && element.asJsonPrimitive.isNumber) {
        "$name must be an integer"
    }
    val literal = element.asJsonPrimitive.toString()
    require(literal.matches(Regex("-?(0|[1-9][0-9]*)"))) { "$name must be an integer" }
    return literal.toLongOrNull() ?: throw IllegalArgumentException("$name is outside the Long range")
}

private fun JsonObject.requiredObject(name: String): JsonObject {
    val element = get(name) ?: error("Missing required Zara catalog field: $name")
    require(element.isJsonObject) { "$name must be an object" }
    return element.asJsonObject
}

private fun JsonObject.requiredArray(name: String): JsonArray {
    val element = get(name) ?: error("Missing required Zara catalog field: $name")
    require(element.isJsonArray) { "$name must be an array" }
    return element.asJsonArray
}

private fun JsonObject.requiredStringSet(name: String, maxSize: Int): Set<String> {
    val array = requiredArray(name)
    require(array.size() in 1..maxSize) { "$name must contain between 1 and $maxSize entries" }
    return array.toStringSet(name)
}

private fun JsonObject.optionalStringSet(name: String, maxSize: Int): Set<String> {
    val element = get(name) ?: return emptySet()
    require(element.isJsonArray) { "$name must be an array" }
    val array = element.asJsonArray
    require(array.size() <= maxSize) { "$name exceeds $maxSize entries" }
    return array.toStringSet(name)
}

private fun JsonArray.toStringSet(name: String): Set<String> {
    val values = mapIndexed { index, element ->
        require(element.isJsonPrimitive && element.asJsonPrimitive.isString) {
            "$name[$index] must be a string"
        }
        element.asString.bounded("$name[$index]", 256)
    }
    require(values.toSet().size == values.size) { "$name contains duplicate entries" }
    return values.toSet()
}

private fun String.bounded(name: String, maxLength: Int): String {
    require(isNotBlank()) { "$name must not be blank" }
    require(length <= maxLength) { "$name exceeds $maxLength characters" }
    return this
}

private fun String.requireHttpsUrl(name: String): String {
    bounded(name, 2048)
    val uri = try {
        URI(this)
    } catch (error: Exception) {
        throw IllegalArgumentException("$name must be a valid HTTPS URL", error)
    }
    require(uri.scheme == "https" && !uri.host.isNullOrBlank() && uri.userInfo == null) {
        "$name must be an HTTPS URL without user info"
    }
    return this
}
