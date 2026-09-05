package ai.zara.app.runtime

import java.net.URI

@JvmInline
value class ServerProfile private constructor(val endpoint: String) {
    companion object {
        fun create(endpoint: String): ServerProfile {
            require(endpoint.isNotBlank()) { "server endpoint is required" }
            val uri = try {
                URI(endpoint)
            } catch (error: Exception) {
                throw IllegalArgumentException("server endpoint is invalid", error)
            }
            require(uri.scheme == "tcp") { "server endpoint must use tcp" }
            require(uri.userInfo == null) { "server endpoint must not contain credentials" }
            require(uri.rawQuery == null && uri.rawFragment == null) {
                "server endpoint must not contain query or fragment"
            }
            require(uri.rawPath.isNullOrEmpty()) { "server endpoint must not contain a path" }
            require(!uri.host.isNullOrBlank()) { "server endpoint requires a host" }
            require(uri.port in 1..65535) { "server endpoint requires a valid port" }
            return ServerProfile(uri.toASCIIString())
        }
    }
}
