package ai.zara.app.device

import java.net.URI
import java.nio.charset.StandardCharsets

object OpenUriPolicy {
    const val MAX_URI_BYTES = 2_048
    private val allowedSchemes = setOf("http", "https")

    fun normalize(value: String): String {
        require(value.isNotBlank()) { "URI is required" }
        require(value.toByteArray(StandardCharsets.UTF_8).size <= MAX_URI_BYTES) {
            "URI exceeds byte limit"
        }
        require(value.none { it.code < 0x20 || it.code == 0x7f }) {
            "URI contains control characters"
        }
        require('\\' !in value) { "URI contains backslash" }

        val parsed = try {
            URI(value)
        } catch (error: Exception) {
            throw IllegalArgumentException("URI is malformed", error)
        }
        require(!parsed.isOpaque) { "opaque URI is not allowed" }
        val scheme = parsed.scheme?.lowercase()
            ?: throw IllegalArgumentException("URI scheme is required")
        require(scheme in allowedSchemes) { "URI scheme is not allowed" }
        require(parsed.rawUserInfo == null) { "URI userinfo is not allowed" }
        require(parsed.rawFragment == null) { "URI fragment is not allowed" }
        require(!parsed.host.isNullOrBlank()) { "URI host is required" }
        val authority = parsed.rawAuthority
            ?: throw IllegalArgumentException("URI authority is required")

        val rebuilt = buildString {
            append(scheme)
            append("://")
            append(authority)
            append(parsed.rawPath?.takeIf(String::isNotEmpty) ?: "/")
            parsed.rawQuery?.let {
                append('?')
                append(it)
            }
        }
        val normalized = URI(rebuilt).normalize().toASCIIString()
        require(normalized.toByteArray(StandardCharsets.UTF_8).size <= MAX_URI_BYTES) {
            "normalized URI exceeds byte limit"
        }
        return normalized
    }
}
