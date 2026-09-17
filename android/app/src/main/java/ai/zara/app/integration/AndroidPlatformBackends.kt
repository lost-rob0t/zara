package ai.zara.app.integration

import ai.zara.app.integration.admin.ZaraDeviceAdminReceiver
import android.app.admin.DevicePolicyManager
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Process
import java.io.ByteArrayOutputStream
import java.io.File
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

class RawIntentBackend(
    private val context: Context,
) : AndroidOperationBackend {
    override val backend = AndroidBackend.INTENT
    override val minimumAuthority = AndroidAuthorityLevel.UNRESTRICTED
    override val identity: String
        get() = "app:${Process.myUid()}"

    override fun isAvailable(): Boolean = true

    override fun supports(operation: String): Boolean = operation in OPERATIONS

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult {
        val intent = buildIntent(request.arguments)
        when (request.operation) {
            "intent.start_activity" -> {
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
                context.startActivity(intent)
            }
            "intent.send_broadcast" -> context.sendBroadcast(intent)
            "intent.start_service" -> context.startService(intent)
            "intent.start_foreground_service" -> {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                    context.startForegroundService(intent)
                } else {
                    context.startService(intent)
                }
            }
            else -> return unsupported()
        }
        return AndroidOperationResult.completed(backend.atom, identity)
    }

    private fun buildIntent(args: Map<String, String>): Intent {
        val encoded = args["intent_uri"]
        val intent = if (encoded != null) {
            Intent.parseUri(encoded, Intent.URI_INTENT_SCHEME or Intent.URI_ANDROID_APP_SCHEME)
        } else {
            Intent().apply {
                args["action"]?.let(::setAction)
                args["data"]?.let { data = Uri.parse(it) }
                args["type"]?.let(::setType)
                args["package"]?.let(::setPackage)
                args["component"]?.let { flattened ->
                    component = requireNotNull(ComponentName.unflattenFromString(flattened)) {
                        "invalid component"
                    }
                }
                args["categories"]
                    ?.split(',')
                    ?.asSequence()
                    ?.map(String::trim)
                    ?.filter(String::isNotEmpty)
                    ?.forEach(::addCategory)
                args["flags"]?.let { flags = Integer.decode(it) }
                args.forEach { (key, value) ->
                    when {
                        key.startsWith("extra.string.") ->
                            putExtra(key.removePrefix("extra.string."), value)
                        key.startsWith("extra.int.") ->
                            putExtra(key.removePrefix("extra.int."), value.toInt())
                        key.startsWith("extra.long.") ->
                            putExtra(key.removePrefix("extra.long."), value.toLong())
                        key.startsWith("extra.bool.") ->
                            putExtra(key.removePrefix("extra.bool."), value.toBooleanStrict())
                        key.startsWith("extra.uri.") ->
                            putExtra(key.removePrefix("extra.uri."), Uri.parse(value))
                    }
                }
            }
        }
        args["flags_add"]?.let { intent.addFlags(Integer.decode(it)) }
        return intent
    }

    private fun unsupported(): AndroidOperationResult =
        AndroidOperationResult.failed(
            AndroidOperationError.UNSUPPORTED_OPERATION,
            backend = backend.atom,
            identity = identity,
        )

    private companion object {
        val OPERATIONS = setOf(
            "intent.start_activity",
            "intent.send_broadcast",
            "intent.start_service",
            "intent.start_foreground_service",
        )
    }
}

open class AppShellBackend(
    private val executable: List<String> = listOf("/system/bin/sh", "-c"),
) : AndroidOperationBackend {
    override val backend = AndroidBackend.SHELL
    override val minimumAuthority = AndroidAuthorityLevel.UNRESTRICTED
    override val identity: String
        get() = "app:${Process.myUid()}"

    override fun isAvailable(): Boolean = File("/system/bin/sh").canExecute()

    override fun supports(operation: String): Boolean = operation == "shell.exec"

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult =
        executeCommand(request, executable, backend.atom, identity)
}

class RootShellBackend : AndroidOperationBackend {
    override val backend = AndroidBackend.ROOT
    override val minimumAuthority = AndroidAuthorityLevel.UNRESTRICTED
    override val identity: String = "root:0"

    override fun isAvailable(): Boolean = SU_PATHS.any(File::canExecute)

    override fun supports(operation: String): Boolean = operation == "shell.exec"

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult =
        executeCommand(request, listOf(resolveSu(), "-c"), backend.atom, identity)

    private fun resolveSu(): String = SU_PATHS.firstOrNull(File::canExecute) ?: "su"

    private companion object {
        val SU_PATHS = listOf(
            "/system/bin/su",
            "/system/xbin/su",
            "/sbin/su",
            "/su/bin/su",
        )
    }
}

class DeviceOwnerBackend(
    private val context: Context,
) : AndroidOperationBackend {
    private val policy = context.getSystemService(DevicePolicyManager::class.java)
    private val admin = ComponentName(context, ZaraDeviceAdminReceiver::class.java)

    override val backend = AndroidBackend.DEVICE_POLICY
    override val minimumAuthority = AndroidAuthorityLevel.UNRESTRICTED
    override val identity: String = "device_owner:${context.packageName}"

    override fun isAvailable(): Boolean = policy.isDeviceOwnerApp(context.packageName)

    override fun supports(operation: String): Boolean = operation in OPERATIONS

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult {
        when (request.operation) {
            "device_policy.lock_now" -> policy.lockNow()
            "device_policy.reboot" -> policy.reboot(admin)
            else -> return AndroidOperationResult.failed(
                AndroidOperationError.UNSUPPORTED_OPERATION,
                backend = backend.atom,
                identity = identity,
            )
        }
        return AndroidOperationResult.completed(backend.atom, identity)
    }

    private companion object {
        val OPERATIONS = setOf("device_policy.lock_now", "device_policy.reboot")
    }
}

private fun executeCommand(
    request: AndroidOperationRequest,
    prefix: List<String>,
    backend: String,
    identity: String,
): AndroidOperationResult {
    val command = request.arguments["command"]
        ?: return AndroidOperationResult.failed(
            AndroidOperationError.FAILED,
            backend = backend,
            identity = identity,
            message = "command is required",
        )
    val timeoutMs = request.arguments["timeout_ms"]
        ?.toLongOrNull()
        ?.coerceIn(1_000L, 300_000L)
        ?: 30_000L
    val process = ProcessBuilder(prefix + command)
        .redirectErrorStream(true)
        .start()
    val output = ByteArrayOutputStream(minOf(MAX_COMMAND_OUTPUT, 8192))
    val reader = thread(name = "zara-shell-drain", isDaemon = true) {
        process.inputStream.use { input ->
            val buffer = ByteArray(8192)
            while (true) {
                val read = input.read(buffer)
                if (read < 0) break
                val remaining = MAX_COMMAND_OUTPUT - output.size()
                if (remaining > 0) output.write(buffer, 0, minOf(read, remaining))
            }
        }
    }
    if (!process.waitFor(timeoutMs, TimeUnit.MILLISECONDS)) {
        process.destroyForcibly()
        reader.join(1_000)
        return AndroidOperationResult.failed(
            AndroidOperationError.FAILED,
            backend = backend,
            identity = identity,
            message = "command timed out",
        )
    }
    reader.join(1_000)
    val text = output.toString(Charsets.UTF_8.name())
    val exit = process.exitValue()
    return if (exit == 0) {
        AndroidOperationResult.completed(backend, identity, text)
    } else {
        AndroidOperationResult.failed(
            AndroidOperationError.FAILED,
            backend = backend,
            identity = identity,
            message = "command exited $exit: ${text.take(2048)}",
        )
    }
}

private const val MAX_COMMAND_OUTPUT = 256 * 1024
