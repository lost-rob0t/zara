package ai.zara.app.finance

import java.net.HttpURLConnection
import java.net.URI
import java.net.URL
import java.nio.charset.StandardCharsets

data class StarFundsHttpResponse(
    val status: Int,
    val body: String,
)

fun interface StarFundsTokenProvider {
    fun token(): String?
}

interface StarFundsHttpTransport {
    fun request(
        method: String,
        url: String,
        bearerToken: String?,
        body: String?,
    ): StarFundsHttpResponse
}

class UrlConnectionStarFundsTransport(
    private val connectTimeoutMillis: Int = 10_000,
    private val readTimeoutMillis: Int = 30_000,
    private val maxResponseBytes: Int = 2 * 1024 * 1024,
) : StarFundsHttpTransport {
    override fun request(
        method: String,
        url: String,
        bearerToken: String?,
        body: String?,
    ): StarFundsHttpResponse {
        val connection = URL(url).openConnection() as HttpURLConnection
        try {
            connection.requestMethod = method
            connection.connectTimeout = connectTimeoutMillis
            connection.readTimeout = readTimeoutMillis
            connection.instanceFollowRedirects = false
            connection.setRequestProperty("Accept", "application/json")
            bearerToken
                ?.takeIf { it.isNotBlank() }
                ?.let { connection.setRequestProperty("Authorization", "Bearer $it") }

            if (body != null) {
                val bytes = body.toByteArray(StandardCharsets.UTF_8)
                require(bytes.size <= 64 * 1024) { "Star Funds request body is too large" }
                connection.doOutput = true
                connection.setRequestProperty("Content-Type", "application/json")
                connection.outputStream.use { it.write(bytes) }
            }

            val status = connection.responseCode
            val stream =
                if (status in 200..299) {
                    connection.inputStream
                } else {
                    connection.errorStream
                }
            val responseBytes =
                stream?.use { input ->
                    val output = java.io.ByteArrayOutputStream()
                    val buffer = ByteArray(8192)
                    while (true) {
                        val count = input.read(buffer)
                        if (count < 0) break
                        if (output.size() + count > maxResponseBytes) {
                            error("Star Funds response exceeded configured limit")
                        }
                        output.write(buffer, 0, count)
                    }
                    output.toByteArray()
                } ?: ByteArray(0)

            return StarFundsHttpResponse(
                status = status,
                body = responseBytes.toString(StandardCharsets.UTF_8),
            )
        } finally {
            connection.disconnect()
        }
    }
}

enum class StarFundsTask(
    val wireName: String,
) {
    RESEARCH_OPPORTUNITIES("research-opportunities"),
    KALSHI_PAPER_ATTEMPT("kalshi-paper-attempt"),
}

class StarFundsEndpoint private constructor(
    val baseUrl: String,
) {
    companion object {
        fun create(raw: String): StarFundsEndpoint {
            val value = raw.trim().trimEnd('/')
            require(value.isNotBlank()) { "Star Funds URL is required" }
            val uri = URI(value)
            require(uri.userInfo == null) { "Star Funds URL must not contain credentials" }
            require(uri.rawQuery == null && uri.rawFragment == null) {
                "Star Funds URL must not contain query or fragment"
            }
            require(!uri.host.isNullOrBlank()) { "Star Funds URL must have a host" }
            val loopback = uri.host in setOf("127.0.0.1", "localhost", "::1")
            require(uri.scheme == "https" || (loopback && uri.scheme == "http")) {
                "Remote Star Funds endpoints require HTTPS"
            }
            return StarFundsEndpoint(value)
        }
    }

    fun path(path: String): String {
        require(path.startsWith("/") && !path.startsWith("//"))
        require(!path.contains('\'))
        return "$baseUrl$path"
    }
}

class StarFundsClient(
    private val endpoint: StarFundsEndpoint,
    private val tokenProvider: StarFundsTokenProvider,
    private val transport: StarFundsHttpTransport = UrlConnectionStarFundsTransport(),
) {
    fun status(): StarFundsHttpResponse =
        request("GET", "/api/v1/star-funds/status", null)

    fun researchOpportunities(top: Int = 25): StarFundsHttpResponse {
        val bounded = top.coerceIn(1, 100)
        return callTask(
            StarFundsTask.RESEARCH_OPPORTUNITIES,
            """{"top":$bounded}""",
        )
    }

    fun kalshiPaperAttempt(
        ticker: String,
        word: String,
        minimumMentions: Int,
        side: String,
        limitPrice: Double,
        maxSize: Int,
        minimumVolume: Double,
        maximumSpread: Double,
    ): StarFundsHttpResponse {
        require(ticker.isNotBlank())
        require(word.isNotBlank())
        require(minimumMentions >= 0)
        require(maxSize > 0)
        require(limitPrice in 0.0..1.0)
        require(minimumVolume >= 0.0)
        require(maximumSpread in 0.0..1.0)

        val normalizedSide = side.trim().lowercase()
        require(normalizedSide == "yes" || normalizedSide == "no") {
            "side must be yes or no"
        }

        val body =
            buildString {
                append('{')
                append("\"ticker\":")
                append(jsonString(ticker.trim()))
                append(",\"word\":")
                append(jsonString(word.trim()))
                append(",\"minimum_mentions\":")
                append(minimumMentions)
                append(",\"side\":")
                append(jsonString(normalizedSide))
                append(",\"limit_price\":")
                append(limitPrice)
                append(",\"max_size\":")
                append(maxSize)
                append(",\"minimum_volume\":")
                append(minimumVolume)
                append(",\"maximum_spread\":")
                append(maximumSpread)
                append('}')
            }

        return callTask(StarFundsTask.KALSHI_PAPER_ATTEMPT, body)
    }

    private fun callTask(
        task: StarFundsTask,
        body: String,
    ): StarFundsHttpResponse =
        request(
            "POST",
            "/api/v1/star-funds/tasks/${task.wireName}",
            body,
        )

    private fun request(
        method: String,
        path: String,
        body: String?,
    ): StarFundsHttpResponse {
        require(method == "GET" || method == "POST")
        require(
            path == "/api/v1/star-funds/status" ||
                path.startsWith("/api/v1/star-funds/tasks/"),
        )
        require(!path.contains("live", ignoreCase = true)) {
            "Android Star Funds client has no live execution surface"
        }
        return transport.request(
            method = method,
            url = endpoint.path(path),
            bearerToken = tokenProvider.token(),
            body = body,
        )
    }

    private fun jsonString(value: String): String =
        buildString {
            append('"')
            value.forEach { character ->
                when (character) {
                    '"' -> append("\\\"")
                    '\\' -> append("\\\\")
                    '\b' -> append("\\b")
                    '\u000C' -> append("\\f")
                    '\n' -> append("\\n")
                    '\r' -> append("\\r")
                    '\t' -> append("\\t")
                    else -> {
                        if (character.code < 0x20) {
                            append("\\u")
                            append(character.code.toString(16).padStart(4, '0'))
                        } else {
                            append(character)
                        }
                    }
                }
            }
            append('"')
        }
}
