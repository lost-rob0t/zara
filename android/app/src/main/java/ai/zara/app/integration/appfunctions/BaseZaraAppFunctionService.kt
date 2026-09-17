package ai.zara.app.integration.appfunctions

import ai.zara.app.ZaraApplication
import ai.zara.app.integration.AndroidOperationRequest
import androidx.annotation.RequiresApi
import androidx.appfunctions.AppFunction
import androidx.appfunctions.AppFunctionInvalidArgumentException
import androidx.appfunctions.AppFunctionService
import androidx.appfunctions.AppFunctionServiceEntryPoint
import org.json.JSONObject
import java.util.concurrent.CompletableFuture
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException
import kotlin.coroutines.suspendCoroutine

@RequiresApi(36)
@AppFunctionServiceEntryPoint(
    serviceName = "ZaraAppFunctionService",
    appFunctionXmlFileName = "zara_app_function_service",
)
abstract class BaseZaraAppFunctionService : AppFunctionService() {
    /**
     * Ask the active Zara runtime to handle a natural-language request.
     *
     * @param text The user request to send to Zara.
     * @return Zara's text response.
     */
    @AppFunction(isDescribedByKDoc = true)
    suspend fun askZara(text: String): String {
        val prompt = text.trim()
        if (prompt.isEmpty() || prompt.length > MAX_TEXT_CHARS) {
            throw AppFunctionInvalidArgumentException("text must contain 1..$MAX_TEXT_CHARS characters")
        }
        val result = zara.appSession.submitText(prompt).await()
        return result.text
    }

    /**
     * Query Zara's embedded local Prolog workspace.
     *
     * @param query A bounded Prolog query accepted by Zara's mobile query policy.
     * @return The query result terms and runtime generation.
     */
    @AppFunction(isDescribedByKDoc = true)
    suspend fun queryLocalProlog(query: String): String {
        val text = query.trim()
        if (text.isEmpty() || text.length > MAX_QUERY_CHARS) {
            throw AppFunctionInvalidArgumentException("query must contain 1..$MAX_QUERY_CHARS characters")
        }
        val result = zara.appSession.queryLocalProlog(text).await()
        return JSONObject()
            .put("query", result.query)
            .put("generation", result.generation)
            .put("terms", result.terms)
            .toString()
    }

    /**
     * Execute an Android operation through Zara's local Prolog authority policy.
     * The operation is as powerful as the locally configured Android backend actually permits.
     *
     * @param backend Backend name such as auto, accessibility, notification, ime, intent, shizuku, root, or device_policy.
     * @param operation Zara Android operation identifier.
     * @param argumentsJson JSON object containing string-valued backend arguments.
     * @return A JSON execution receipt including the actual backend identity used.
     */
    @AppFunction(isDescribedByKDoc = true)
    suspend fun executeAndroid(
        backend: String = "auto",
        operation: String,
        argumentsJson: String = "{}",
    ): String {
        if (argumentsJson.length > MAX_ARGUMENTS_JSON_CHARS) {
            throw AppFunctionInvalidArgumentException("argumentsJson is too large")
        }
        val arguments = try {
            val json = JSONObject(argumentsJson)
            buildMap {
                json.keys().forEach { key ->
                    val value = json.get(key)
                    if (value !is String) {
                        throw AppFunctionInvalidArgumentException("Android arguments must be strings")
                    }
                    put(key, value)
                }
            }
        } catch (error: AppFunctionInvalidArgumentException) {
            throw error
        } catch (_: Throwable) {
            throw AppFunctionInvalidArgumentException("argumentsJson must be a JSON object")
        }

        val result = zara.androidIntegration.execute(
            AndroidOperationRequest(
                backend = backend,
                operation = operation,
                arguments = arguments,
            ),
        ).await()
        return JSONObject()
            .put("success", result.success)
            .put("backend", result.backend)
            .put("identity", result.identity)
            .put("output", result.output)
            .put("error", result.error?.name)
            .put("message", result.message)
            .toString()
    }

    private val zara: ZaraApplication
        get() = application as ZaraApplication

    private suspend fun <T> CompletableFuture<T>.await(): T = suspendCoroutine { continuation ->
        whenComplete { value, error ->
            if (error != null) continuation.resumeWithException(error)
            else continuation.resume(value)
        }
    }

    private companion object {
        const val MAX_TEXT_CHARS = 64 * 1024
        const val MAX_QUERY_CHARS = 16 * 1024
        const val MAX_ARGUMENTS_JSON_CHARS = 256 * 1024
    }
}
