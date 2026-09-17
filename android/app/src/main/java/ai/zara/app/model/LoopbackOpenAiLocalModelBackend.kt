package ai.zara.app.model

import java.io.BufferedReader
import java.io.IOException
import java.net.ConnectException
import java.net.HttpURLConnection
import java.net.SocketTimeoutException
import java.net.URL
import org.json.JSONArray
import org.json.JSONObject

class LoopbackOpenAiLocalModelBackend : LocalModelBackend {
    @Volatile
    private var activeConnection: HttpURLConnection? = null

    override fun generate(
        config: LocalModelConfig,
        request: LocalModelRequest,
        cancelled: () -> Boolean,
        onText: (String) -> Unit,
    ): LocalModelResult {
        val safe = config.validated()
        if (!safe.enabled) {
            throw LocalModelException(LocalModelFailureReason.DISABLED, "Local model is disabled")
        }
        val started = System.nanoTime()
        val deadlineNanos = started + request.deadlineMs * 1_000_000L
        val endpoint = "${safe.endpoint}/v1/chat/completions"
        val connection = (URL(endpoint).openConnection() as HttpURLConnection).apply {
            requestMethod = "POST"
            doOutput = true
            instanceFollowRedirects = false
            connectTimeout = minOf(request.deadlineMs, 10_000L).toInt()
            readTimeout = request.deadlineMs.toInt()
            setRequestProperty("Content-Type", "application/json")
            setRequestProperty("Accept", "text/event-stream, application/json")
            setRequestProperty("User-Agent", "zara-android-local-model/1")
        }
        activeConnection = connection
        return try {
            checkActive(cancelled, deadlineNanos)
            val payload = JSONObject()
                .put("model", safe.model)
                .put("stream", true)
                .put("max_tokens", request.maxOutputTokens)
                .put("temperature", 0.2)
                .put(
                    "messages",
                    JSONArray()
                        .put(JSONObject().put("role", "system").put("content", request.systemPrompt))
                        .put(JSONObject().put("role", "user").put("content", request.prompt)),
                )
            connection.outputStream.use { output ->
                output.write(payload.toString().encodeToByteArray())
            }
            val status = connection.responseCode
            if (status !in 200..299) {
                throw classifyHttpFailure(status, readError(connection))
            }
            val text = if (connection.contentType.orEmpty().contains("text/event-stream")) {
                readEventStream(connection.inputStream.bufferedReader(), cancelled, deadlineNanos, onText)
            } else {
                readJsonResponse(connection.inputStream.bufferedReader(), cancelled, deadlineNanos, onText)
            }
            if (text.isBlank()) {
                throw LocalModelException(
                    LocalModelFailureReason.INVALID_RESPONSE,
                    "Local model returned no text",
                )
            }
            LocalModelResult(
                identity = safe.identity(),
                text = text,
                elapsedMs = (System.nanoTime() - started) / 1_000_000L,
            )
        } catch (error: LocalModelException) {
            throw error
        } catch (error: SocketTimeoutException) {
            throw LocalModelException(LocalModelFailureReason.TIMEOUT, "Local model timed out", error)
        } catch (error: ConnectException) {
            throw LocalModelException(
                LocalModelFailureReason.UNAVAILABLE,
                "Local model server is not running on ${safe.endpoint}",
                error,
            )
        } catch (error: IOException) {
            if (cancelled()) {
                throw LocalModelException(LocalModelFailureReason.CANCELLED, "Local model request was cancelled", error)
            }
            throw LocalModelException(LocalModelFailureReason.UNAVAILABLE, "Local model connection failed", error)
        } finally {
            if (activeConnection === connection) activeConnection = null
            connection.disconnect()
        }
    }

    override fun cancel() {
        activeConnection?.disconnect()
    }

    override fun close() {
        cancel()
    }

    private fun readEventStream(
        reader: BufferedReader,
        cancelled: () -> Boolean,
        deadlineNanos: Long,
        onText: (String) -> Unit,
    ): String {
        val output = StringBuilder()
        reader.useLines { lines ->
            for (line in lines) {
                checkActive(cancelled, deadlineNanos)
                if (!line.startsWith("data:")) continue
                val data = line.removePrefix("data:").trim()
                if (data == "[DONE]") break
                if (data.isEmpty()) continue
                val chunk = runCatching { JSONObject(data) }.getOrElse {
                    throw LocalModelException(
                        LocalModelFailureReason.INVALID_RESPONSE,
                        "Local model stream returned malformed JSON",
                        it,
                    )
                }
                val text = extractText(chunk)
                if (text.isNotEmpty()) {
                    appendBounded(output, text)
                    onText(text)
                }
            }
        }
        return output.toString()
    }

    private fun readJsonResponse(
        reader: BufferedReader,
        cancelled: () -> Boolean,
        deadlineNanos: Long,
        onText: (String) -> Unit,
    ): String {
        val raw = StringBuilder()
        reader.use { input ->
            val buffer = CharArray(DEFAULT_BUFFER_SIZE)
            while (true) {
                checkActive(cancelled, deadlineNanos)
                val read = input.read(buffer)
                if (read < 0) break
                check(raw.length + read <= MAX_RESPONSE_CHARS) {
                    "Local model response exceeded the safe limit"
                }
                raw.append(buffer, 0, read)
            }
        }
        val text = runCatching { extractText(JSONObject(raw.toString())) }.getOrElse {
            throw LocalModelException(
                LocalModelFailureReason.INVALID_RESPONSE,
                "Local model returned malformed JSON",
                it,
            )
        }
        if (text.isNotEmpty()) onText(text)
        return text
    }

    private fun extractText(json: JSONObject): String {
        val choices = json.optJSONArray("choices") ?: return ""
        if (choices.length() == 0) return ""
        val choice = choices.optJSONObject(0) ?: return ""
        val delta = choice.optJSONObject("delta")
        val message = choice.optJSONObject("message")
        return when {
            delta != null -> delta.optString("content", "")
            message != null -> message.optString("content", "")
            else -> choice.optString("text", "")
        }
    }

    private fun appendBounded(output: StringBuilder, text: String) {
        if (output.length + text.length > MAX_RESPONSE_CHARS) {
            throw LocalModelException(
                LocalModelFailureReason.INVALID_RESPONSE,
                "Local model response exceeded the safe limit",
            )
        }
        output.append(text)
    }

    private fun readError(connection: HttpURLConnection): String {
        val stream = connection.errorStream ?: return ""
        return stream.bufferedReader().use { reader ->
            buildString {
                val buffer = CharArray(DEFAULT_BUFFER_SIZE)
                while (length < MAX_ERROR_CHARS) {
                    val read = reader.read(buffer, 0, minOf(buffer.size, MAX_ERROR_CHARS - length))
                    if (read < 0) break
                    append(buffer, 0, read)
                }
            }
        }
    }

    private fun classifyHttpFailure(status: Int, body: String): LocalModelException {
        val normalized = body.lowercase()
        val reason = when {
            status == HttpURLConnection.HTTP_NOT_FOUND -> LocalModelFailureReason.UNSUPPORTED
            "out of memory" in normalized || "oom" in normalized -> LocalModelFailureReason.OUT_OF_MEMORY
            status in 400..499 -> LocalModelFailureReason.UNSUPPORTED
            else -> LocalModelFailureReason.UNAVAILABLE
        }
        val detail = body.trim().take(512).ifBlank { "HTTP $status" }
        return LocalModelException(reason, "Local model server rejected the request: $detail")
    }

    private fun checkActive(cancelled: () -> Boolean, deadlineNanos: Long) {
        if (cancelled()) {
            throw LocalModelException(LocalModelFailureReason.CANCELLED, "Local model request was cancelled")
        }
        if (System.nanoTime() >= deadlineNanos) {
            throw LocalModelException(LocalModelFailureReason.TIMEOUT, "Local model request exceeded its deadline")
        }
    }

    companion object {
        private const val MAX_RESPONSE_CHARS = 1_048_576
        private const val MAX_ERROR_CHARS = 32_768
    }
}
