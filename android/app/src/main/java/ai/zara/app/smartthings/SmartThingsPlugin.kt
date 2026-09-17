package ai.zara.app.smartthings

import ai.zara.app.auth.CredentialCipher
import ai.zara.app.auth.SealedCredential
import ai.zara.app.prolog.PrologWorkspace
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.ByteArrayOutputStream
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.EOFException
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.net.URL
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import javax.net.ssl.HttpsURLConnection
import org.json.JSONArray
import org.json.JSONObject

sealed interface SmartThingsArgument {
    data class TextValue(val value: String) : SmartThingsArgument
    data class IntegerValue(val value: Int) : SmartThingsArgument
    data class NumberValue(val value: Double) : SmartThingsArgument
    data class BooleanValue(val value: Boolean) : SmartThingsArgument
}

sealed interface SmartThingsAction {
    data object ListDevices : SmartThingsAction
    data class DeviceStatus(val deviceId: String) : SmartThingsAction
    data class Command(
        val deviceId: String,
        val component: String,
        val capability: String,
        val command: String,
        val arguments: List<SmartThingsArgument>,
    ) : SmartThingsAction
    data class LocalError(val code: String, val subject: String) : SmartThingsAction
}

data class SmartThingsDevice(
    val deviceId: String,
    val label: String,
)

data class SmartThingsPluginReply(
    val text: String,
    val success: Boolean,
)

object SmartThingsPrologCodec {
    private const val MAX_TERM_LENGTH = 4_096
    private const val MAX_NESTING = 8
    private const val MAX_LIST_ITEMS = 32
    private const val MAX_ARGUMENTS = 8

    fun decode(text: String): SmartThingsAction? {
        val source = text.trim()
        if (!source.startsWith("smartthings_action")) return null
        require(source.length <= MAX_TERM_LENGTH) { "SmartThings Prolog effect is too large" }
        val root = Parser(source).parse()
        require(root is Term.Compound && root.functor == "smartthings_action" && root.arguments.size == 1) {
            "Malformed SmartThings Prolog effect"
        }
        return decodeAction(root.arguments.single())
    }

    private fun decodeAction(term: Term): SmartThingsAction = when (term) {
        is Term.Atom -> {
            require(term.value == "list_devices") { "Unknown SmartThings Prolog action" }
            SmartThingsAction.ListDevices
        }
        is Term.Compound -> when (term.functor) {
            "status" -> {
                require(term.arguments.size == 1) { "SmartThings status requires one device id" }
                SmartThingsAction.DeviceStatus(scalar(term.arguments.single(), "device id"))
            }
            "command" -> decodeCommand(term)
            "error" -> {
                require(term.arguments.size == 2) { "SmartThings error requires code and subject" }
                SmartThingsAction.LocalError(
                    scalar(term.arguments[0], "error code"),
                    scalar(term.arguments[1], "error subject"),
                )
            }
            else -> throw IllegalArgumentException("Unknown SmartThings Prolog action")
        }
        else -> throw IllegalArgumentException("Malformed SmartThings Prolog action")
    }

    private fun decodeCommand(term: Term.Compound): SmartThingsAction.Command {
        require(term.arguments.size == 5) { "SmartThings command requires five arguments" }
        val arguments = term.arguments[4] as? Term.ListTerm
            ?: throw IllegalArgumentException("SmartThings command arguments must be a list")
        require(arguments.values.size <= MAX_LIST_ITEMS) { "SmartThings command has too many arguments" }
        return SmartThingsAction.Command(
            deviceId = scalar(term.arguments[0], "device id"),
            component = scalar(term.arguments[1], "component"),
            capability = scalar(term.arguments[2], "capability"),
            command = scalar(term.arguments[3], "command"),
            arguments = arguments.values.map(::argument),
        )
    }

    private fun argument(term: Term): SmartThingsArgument = when (term) {
        is Term.Atom -> when (term.value) {
            "true" -> SmartThingsArgument.BooleanValue(true)
            "false" -> SmartThingsArgument.BooleanValue(false)
            else -> SmartThingsArgument.TextValue(term.value)
        }
        is Term.NumberTerm -> {
            val integer = term.value.toIntOrNull()
            if (integer != null && !term.value.contains('.')) {
                SmartThingsArgument.IntegerValue(integer)
            } else {
                val number = term.value.toDoubleOrNull()
                    ?: throw IllegalArgumentException("Invalid SmartThings numeric argument")
                require(number.isFinite()) { "Invalid SmartThings numeric argument" }
                SmartThingsArgument.NumberValue(number)
            }
        }
        else -> throw IllegalArgumentException("SmartThings command arguments must be scalar values")
    }

    private fun scalar(term: Term, label: String): String = when (term) {
        is Term.Atom -> term.value
        is Term.NumberTerm -> term.value
        else -> throw IllegalArgumentException("SmartThings $label must be scalar")
    }.also { value ->
        require(value.isNotBlank() && value.length <= 256) { "SmartThings $label is invalid" }
        require(value.none { it.code < 0x20 }) { "SmartThings $label contains control characters" }
    }

    private sealed interface Term {
        data class Atom(val value: String) : Term
        data class NumberTerm(val value: String) : Term
        data class Compound(val functor: String, val arguments: List<Term>) : Term
        data class ListTerm(val values: List<Term>) : Term
    }

    private class Parser(private val source: String) {
        private var index = 0

        fun parse(): Term {
            val term = parseTerm(0)
            skipWhitespace()
            require(index == source.length) { "Unexpected SmartThings Prolog effect suffix" }
            return term
        }

        private fun parseTerm(depth: Int): Term {
            require(depth <= MAX_NESTING) { "SmartThings Prolog effect is nested too deeply" }
            skipWhitespace()
            require(index < source.length) { "Unexpected end of SmartThings Prolog effect" }
            return when {
                source[index] == '\'' || source[index] == '"' -> Term.Atom(parseQuoted())
                source[index] == '[' -> parseList(depth + 1)
                source[index].isDigit() || (source[index] == '-' && nextIsDigit()) -> parseNumber()
                source[index].isLetter() || source[index] == '_' -> parseAtomOrCompound(depth + 1)
                else -> throw IllegalArgumentException("Invalid SmartThings Prolog effect token")
            }
        }

        private fun parseAtomOrCompound(depth: Int): Term {
            val atom = parseIdentifier()
            skipWhitespace()
            if (index >= source.length || source[index] != '(') return Term.Atom(atom)
            index += 1
            skipWhitespace()
            val arguments = mutableListOf<Term>()
            if (index < source.length && source[index] == ')') {
                index += 1
                return Term.Compound(atom, arguments)
            }
            while (true) {
                require(arguments.size < MAX_ARGUMENTS) { "SmartThings Prolog effect has too many arguments" }
                arguments += parseTerm(depth)
                skipWhitespace()
                require(index < source.length) { "Unterminated SmartThings Prolog compound" }
                when (source[index]) {
                    ',' -> index += 1
                    ')' -> {
                        index += 1
                        return Term.Compound(atom, arguments)
                    }
                    else -> throw IllegalArgumentException("Malformed SmartThings Prolog compound")
                }
            }
        }

        private fun parseList(depth: Int): Term.ListTerm {
            index += 1
            skipWhitespace()
            val values = mutableListOf<Term>()
            if (index < source.length && source[index] == ']') {
                index += 1
                return Term.ListTerm(values)
            }
            while (true) {
                require(values.size < MAX_LIST_ITEMS) { "SmartThings Prolog list is too large" }
                values += parseTerm(depth)
                skipWhitespace()
                require(index < source.length) { "Unterminated SmartThings Prolog list" }
                when (source[index]) {
                    ',' -> index += 1
                    ']' -> {
                        index += 1
                        return Term.ListTerm(values)
                    }
                    else -> throw IllegalArgumentException("Malformed SmartThings Prolog list")
                }
            }
        }

        private fun parseQuoted(): String {
            val quote = source[index++]
            val value = StringBuilder()
            var escaped = false
            while (index < source.length) {
                val character = source[index++]
                if (escaped) {
                    value.append(character)
                    escaped = false
                } else if (character == '\\') {
                    escaped = true
                } else if (character == quote) {
                    return value.toString()
                } else {
                    value.append(character)
                }
            }
            throw IllegalArgumentException("Unterminated SmartThings Prolog string")
        }

        private fun parseNumber(): Term.NumberTerm {
            val start = index
            if (source[index] == '-') index += 1
            while (index < source.length && source[index].isDigit()) index += 1
            if (index < source.length && source[index] == '.') {
                index += 1
                require(index < source.length && source[index].isDigit()) { "Invalid SmartThings number" }
                while (index < source.length && source[index].isDigit()) index += 1
            }
            return Term.NumberTerm(source.substring(start, index))
        }

        private fun parseIdentifier(): String {
            val start = index
            index += 1
            while (index < source.length && (source[index].isLetterOrDigit() || source[index] == '_')) index += 1
            return source.substring(start, index)
        }

        private fun nextIsDigit(): Boolean =
            index + 1 < source.length && source[index + 1].isDigit()

        private fun skipWhitespace() {
            while (index < source.length && source[index].isWhitespace()) index += 1
        }
    }
}

interface SmartThingsGateway {
    fun listDevices(): List<SmartThingsDevice>
    fun status(deviceId: String): String
    fun command(command: SmartThingsAction.Command)
}

fun interface SmartThingsTokenProvider {
    fun accessToken(): String
}

data class SmartThingsHttpRequest(
    val method: String,
    val path: String,
    val accessToken: String,
    val body: String? = null,
)

data class SmartThingsHttpResponse(
    val statusCode: Int,
    val body: String,
)

interface SmartThingsHttpTransport {
    fun execute(request: SmartThingsHttpRequest): SmartThingsHttpResponse
}

class UrlConnectionSmartThingsTransport : SmartThingsHttpTransport {
    override fun execute(request: SmartThingsHttpRequest): SmartThingsHttpResponse {
        require(request.method == "GET" || request.method == "POST") { "Unsupported SmartThings HTTP method" }
        require(request.path.startsWith('/') && ".." !in request.path) { "Invalid SmartThings API path" }
        require(request.accessToken.length in 1..MAX_TOKEN_CHARS) { "SmartThings access token is invalid" }
        require(request.accessToken.none(Char::isWhitespace)) { "SmartThings access token is invalid" }
        val url = URL("$BASE_URL${request.path}")
        val connection = url.openConnection() as HttpsURLConnection
        connection.instanceFollowRedirects = false
        connection.connectTimeout = CONNECT_TIMEOUT_MILLIS
        connection.readTimeout = READ_TIMEOUT_MILLIS
        connection.requestMethod = request.method
        connection.setRequestProperty("Accept", "application/json")
        connection.setRequestProperty("Authorization", "Bearer ${request.accessToken}")
        val body = request.body
        if (body != null) {
            val bytes = body.toByteArray(Charsets.UTF_8)
            require(bytes.size <= MAX_REQUEST_BYTES) { "SmartThings request body is too large" }
            connection.doOutput = true
            connection.setRequestProperty("Content-Type", "application/json")
            connection.outputStream.use { it.write(bytes) }
        }
        return try {
            val status = connection.responseCode
            val input = if (status in 200..299) connection.inputStream else connection.errorStream
            SmartThingsHttpResponse(status, input?.use(::readBounded) ?: "")
        } finally {
            connection.disconnect()
        }
    }

    private fun readBounded(input: java.io.InputStream): String {
        val output = ByteArrayOutputStream()
        val buffer = ByteArray(8_192)
        while (true) {
            val count = input.read(buffer)
            if (count < 0) break
            require(output.size() + count <= MAX_RESPONSE_BYTES) { "SmartThings response is too large" }
            output.write(buffer, 0, count)
        }
        return output.toString(Charsets.UTF_8.name())
    }

    private companion object {
        const val BASE_URL = "https://api.smartthings.com/v1"
        const val CONNECT_TIMEOUT_MILLIS = 10_000
        const val READ_TIMEOUT_MILLIS = 15_000
        const val MAX_REQUEST_BYTES = 64 * 1024
        const val MAX_RESPONSE_BYTES = 1024 * 1024
        const val MAX_TOKEN_CHARS = 4_096
    }
}

class SmartThingsApiGateway(
    private val tokenProvider: SmartThingsTokenProvider,
    private val transport: SmartThingsHttpTransport = UrlConnectionSmartThingsTransport(),
) : SmartThingsGateway {
    override fun listDevices(): List<SmartThingsDevice> {
        val response = request("GET", "/devices")
        val items = parseObject(response.body).optJSONArray("items") ?: JSONArray()
        require(items.length() <= MAX_DEVICES) { "SmartThings returned too many devices" }
        return (0 until items.length()).map { index ->
            val item = items.getJSONObject(index)
            val id = validateDeviceId(item.getString("deviceId"))
            val label = boundedLabel(item.optString("label").ifBlank { item.optString("name") }.ifBlank { id })
            SmartThingsDevice(id, label)
        }
    }

    override fun status(deviceId: String): String {
        val id = validateDeviceId(deviceId)
        val response = request("GET", "/devices/$id/status")
        return parseObject(response.body).toString()
    }

    override fun command(command: SmartThingsAction.Command) {
        val deviceId = validateDeviceId(command.deviceId)
        val payload = JSONObject().put(
            "commands",
            JSONArray().put(
                JSONObject()
                    .put("component", validateName(command.component, "component"))
                    .put("capability", validateName(command.capability, "capability"))
                    .put("command", validateName(command.command, "command"))
                    .put("arguments", JSONArray().apply {
                        command.arguments.forEach { put(argumentValue(it)) }
                    }),
            ),
        )
        request("POST", "/devices/$deviceId/commands", payload.toString())
    }

    private fun request(method: String, path: String, body: String? = null): SmartThingsHttpResponse {
        val token = tokenProvider.accessToken()
        val response = transport.execute(SmartThingsHttpRequest(method, path, token, body))
        if (response.statusCode in 200..299) return response
        throw IllegalStateException(
            when (response.statusCode) {
                401 -> "SmartThings authorization is missing, expired, or revoked"
                403 -> "SmartThings token does not have the required SmartThings scope"
                404 -> "SmartThings device was not found"
                429 -> "SmartThings rate limit reached; retry later"
                else -> "SmartThings API request failed (HTTP ${response.statusCode})"
            },
        )
    }

    private fun parseObject(body: String): JSONObject = try {
        JSONObject(body)
    } catch (error: Exception) {
        throw IllegalStateException("SmartThings returned malformed JSON", error)
    }

    private fun argumentValue(argument: SmartThingsArgument): Any = when (argument) {
        is SmartThingsArgument.TextValue -> argument.value
        is SmartThingsArgument.IntegerValue -> argument.value
        is SmartThingsArgument.NumberValue -> argument.value
        is SmartThingsArgument.BooleanValue -> argument.value
    }

    private fun validateDeviceId(value: String): String {
        require(value.matches(DEVICE_ID)) { "Invalid SmartThings device id" }
        return value
    }

    private fun validateName(value: String, label: String): String {
        require(value.matches(API_NAME)) { "Invalid SmartThings $label" }
        return value
    }

    private fun boundedLabel(value: String): String {
        require(value.length <= 256 && value.none { it.code < 0x20 }) { "Invalid SmartThings device label" }
        return value
    }

    private companion object {
        val DEVICE_ID = Regex("[A-Za-z0-9._:-]{1,256}")
        val API_NAME = Regex("[A-Za-z0-9._:-]{1,128}")
        const val MAX_DEVICES = 500
    }
}

enum class SmartThingsCredentialState { MISSING, READY, EXPIRED, CORRUPT }

class SmartThingsCredentialStore(
    private val file: File,
    private val cipher: CredentialCipher,
    private val nowMillis: () -> Long = System::currentTimeMillis,
) {
    fun savePersonalAccessToken(token: String, expiresAtEpochMillis: Long) {
        val plaintext = token.toByteArray(Charsets.UTF_8)
        require(plaintext.size in 1..MAX_TOKEN_BYTES) { "SmartThings access token is invalid" }
        require(token.none(Char::isWhitespace)) { "SmartThings access token is invalid" }
        require(expiresAtEpochMillis > nowMillis()) { "SmartThings access token is already expired" }
        file.parentFile?.mkdirs()
        val sealed = try {
            cipher.seal(plaintext)
        } finally {
            plaintext.fill(0)
        }
        val temporary = File.createTempFile(".${file.name}.", ".tmp", file.parentFile)
        try {
            FileOutputStream(temporary).use { raw ->
                val output = DataOutputStream(BufferedOutputStream(raw))
                output.writeInt(MAGIC)
                output.writeInt(VERSION)
                output.writeLong(expiresAtEpochMillis)
                output.writeInt(sealed.iv.size)
                output.write(sealed.iv)
                output.writeInt(sealed.ciphertext.size)
                output.write(sealed.ciphertext)
                output.flush()
                raw.fd.sync()
            }
            atomicReplace(temporary, file)
        } finally {
            if (temporary.exists()) temporary.delete()
        }
    }

    fun state(): SmartThingsCredentialState {
        if (!file.isFile) return SmartThingsCredentialState.MISSING
        return try {
            val stored = readStored()
            val plaintext = cipher.open(stored.sealed)
            try {
                validatePlaintext(plaintext)
                if (stored.expiresAtEpochMillis <= nowMillis()) {
                    SmartThingsCredentialState.EXPIRED
                } else {
                    SmartThingsCredentialState.READY
                }
            } finally {
                plaintext.fill(0)
            }
        } catch (_: Exception) {
            SmartThingsCredentialState.CORRUPT
        }
    }

    fun requireAccessToken(): String {
        require(file.isFile) { "SmartThings is not connected" }
        val stored = try {
            readStored()
        } catch (error: Exception) {
            throw IllegalStateException("SmartThings credential store is corrupt", error)
        }
        check(stored.expiresAtEpochMillis > nowMillis()) { "SmartThings access token has expired" }
        val plaintext = try {
            cipher.open(stored.sealed)
        } catch (error: Exception) {
            throw IllegalStateException("SmartThings credential could not be decrypted", error)
        }
        return try {
            validatePlaintext(plaintext)
            plaintext.toString(Charsets.UTF_8)
        } finally {
            plaintext.fill(0)
        }
    }

    fun clear(): Boolean = !file.exists() || file.delete()

    private fun readStored(): StoredCredential {
        require(file.length() in 1..MAX_FILE_BYTES.toLong()) { "SmartThings credential file is invalid" }
        DataInputStream(BufferedInputStream(FileInputStream(file))).use { input ->
            require(input.readInt() == MAGIC) { "Invalid SmartThings credential magic" }
            require(input.readInt() == VERSION) { "Unsupported SmartThings credential version" }
            val expiresAt = input.readLong()
            require(expiresAt > 0) { "Invalid SmartThings credential expiry" }
            val iv = readBounded(input, 1, 64, "IV")
            val ciphertext = readBounded(input, 1, 4_096, "ciphertext")
            require(input.read() == -1) { "Trailing SmartThings credential data" }
            return StoredCredential(expiresAt, SealedCredential(iv, ciphertext))
        }
    }

    private fun validatePlaintext(value: ByteArray) {
        require(value.size in 1..MAX_TOKEN_BYTES) { "SmartThings access token is invalid" }
        val text = value.toString(Charsets.UTF_8)
        require(text.none(Char::isWhitespace)) { "SmartThings access token is invalid" }
    }

    private fun readBounded(
        input: DataInputStream,
        minimum: Int,
        maximum: Int,
        label: String,
    ): ByteArray {
        val length = input.readInt()
        require(length in minimum..maximum) { "SmartThings credential $label has invalid size" }
        val value = ByteArray(length)
        try {
            input.readFully(value)
        } catch (error: EOFException) {
            value.fill(0)
            throw error
        }
        return value
    }

    private fun atomicReplace(source: File, destination: File) {
        try {
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING,
            )
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING)
        }
    }

    private data class StoredCredential(
        val expiresAtEpochMillis: Long,
        val sealed: SealedCredential,
    )

    private companion object {
        const val MAGIC = 0x5A535431
        const val VERSION = 1
        const val MAX_TOKEN_BYTES = 3_072
        const val MAX_FILE_BYTES = 8 * 1024
    }
}

class StoredSmartThingsTokenProvider(
    private val store: SmartThingsCredentialStore,
) : SmartThingsTokenProvider {
    override fun accessToken(): String = store.requireAccessToken()
}

class SmartThingsPluginActor(
    private val gateway: SmartThingsGateway,
) : AutoCloseable {
    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-smartthings").apply { isDaemon = true }
    }
    @Volatile private var closed = false

    fun dispatch(terms: List<String>): CompletableFuture<SmartThingsPluginReply?> {
        val actions = try {
            terms.mapNotNull(SmartThingsPrologCodec::decode)
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        if (actions.isEmpty()) return CompletableFuture.completedFuture(null)
        if (actions.size != 1) {
            return CompletableFuture.failedFuture(
                IllegalArgumentException("A Prolog turn may emit only one SmartThings effect"),
            )
        }
        val action = actions.single()
        if (action is SmartThingsAction.LocalError) {
            return CompletableFuture.completedFuture(localError(action))
        }
        return submit { execute(action) }
    }

    private fun execute(action: SmartThingsAction): SmartThingsPluginReply = when (action) {
        SmartThingsAction.ListDevices -> {
            val devices = gateway.listDevices()
            val visible = devices.take(MAX_VISIBLE_DEVICES)
            val body = if (visible.isEmpty()) {
                "No SmartThings devices are visible to this credential."
            } else {
                visible.joinToString("\n") { "${it.label} [${it.deviceId}]" } +
                    if (devices.size > visible.size) "\n… ${devices.size - visible.size} more" else ""
            }
            SmartThingsPluginReply(body, true)
        }
        is SmartThingsAction.DeviceStatus -> SmartThingsPluginReply(
            "SmartThings status for ${action.deviceId}:\n${gateway.status(action.deviceId)}",
            true,
        )
        is SmartThingsAction.Command -> {
            gateway.command(action)
            SmartThingsPluginReply("SmartThings command accepted for ${action.deviceId}.", true)
        }
        is SmartThingsAction.LocalError -> localError(action)
    }

    private fun localError(error: SmartThingsAction.LocalError): SmartThingsPluginReply =
        SmartThingsPluginReply(
            text = when (error.code) {
                "unknown_device" -> "Unknown SmartThings device alias: ${error.subject}"
                "invalid_level" -> "SmartThings level must be between 0 and 100: ${error.subject}"
                else -> "SmartThings rule rejected ${error.subject}: ${error.code}"
            },
            success = false,
        )

    private fun <T> submit(block: () -> T): CompletableFuture<T> {
        if (closed) return CompletableFuture.failedFuture(IllegalStateException("SmartThings plugin is closed"))
        val future = CompletableFuture<T>()
        actor.execute {
            try {
                future.complete(block())
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        return future
    }

    override fun close() {
        if (closed) return
        closed = true
        actor.shutdownNow()
    }

    private companion object {
        const val MAX_VISIBLE_DEVICES = 100
    }
}

object SmartThingsPrologPlugin {
    const val SOURCE_NAME = "smartthings.pl"

    val source: String = """
        % Zara Android SmartThings plugin.
        % Keep credentials out of Prolog. Configure aliases as facts, for example:
        % smartthings_device(desk_lamp, "00000000-0000-0000-0000-000000000000").

        smartthings_device(_Alias, _DeviceId) :- fail.

        smartthings_target(Device, DeviceId, ok) :-
            smartthings_device(Device, DeviceId),
            !.
        smartthings_target(Device, _DeviceId, error(Device)).

        smartthings_devices(Result) :-
            Result = smartthings_action(list_devices).

        smartthings_status(Device, Result) :-
            smartthings_target(Device, DeviceId, Target),
            smartthings_status_target(Target, DeviceId, Result).
        smartthings_status_target(ok, DeviceId, smartthings_action(status(DeviceId))).
        smartthings_status_target(error(Device), _DeviceId,
            smartthings_action(error(unknown_device, Device))).

        smartthings_command(Device, Component, Capability, Command, Arguments, Result) :-
            smartthings_target(Device, DeviceId, Target),
            smartthings_command_target(
                Target, DeviceId, Component, Capability, Command, Arguments, Result
            ).
        smartthings_command_target(
            ok, DeviceId, Component, Capability, Command, Arguments,
            smartthings_action(command(DeviceId, Component, Capability, Command, Arguments))
        ).
        smartthings_command_target(
            error(Device), _DeviceId, _Component, _Capability, _Command, _Arguments,
            smartthings_action(error(unknown_device, Device))
        ).

        smartthings_on(Device, Result) :-
            smartthings_command(Device, main, switch, on, [], Result).
        smartthings_off(Device, Result) :-
            smartthings_command(Device, main, switch, off, [], Result).
        smartthings_set_level(Device, Level, Result) :-
            number(Level),
            Level >= 0,
            Level =< 100,
            !,
            smartthings_command(Device, main, switchLevel, setLevel, [Level], Result).
        smartthings_set_level(_Device, Level,
            smartthings_action(error(invalid_level, Level))).

        smartthings_on_explain(Device, Result) :- smartthings_on(Device, Result).
        smartthings_off_explain(Device, Result) :- smartthings_off(Device, Result).
        smartthings_status_explain(Device, Result) :- smartthings_status(Device, Result).

        expert_activation(smartthings_on, on).
        expert_activation(smartthings_off, off).
        expert_activation(smartthings_status, status).
    """.trimIndent() + "\n"

    fun install(workspace: PrologWorkspace) {
        if (workspace.listSources().none { it.name == SOURCE_NAME }) {
            workspace.saveSource(SOURCE_NAME, source)
        }
    }
}
