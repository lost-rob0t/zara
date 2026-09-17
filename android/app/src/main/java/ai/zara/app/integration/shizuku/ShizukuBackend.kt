package ai.zara.app.integration.shizuku

import ai.zara.app.BuildConfig
import ai.zara.app.integration.AndroidAuthorityLevel
import ai.zara.app.integration.AndroidBackend
import ai.zara.app.integration.AndroidOperationBackend
import ai.zara.app.integration.AndroidOperationError
import ai.zara.app.integration.AndroidOperationRequest
import ai.zara.app.integration.AndroidOperationResult
import android.content.ComponentName
import android.content.Context
import android.content.ServiceConnection
import android.content.pm.PackageManager
import android.os.IBinder
import rikka.shizuku.Shizuku
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

class ShizukuBackend(
    private val context: Context,
) : AndroidOperationBackend {
    private val remote = AtomicReference<IZaraPrivilegedService?>(null)
    private val binding = AtomicBoolean(false)
    private val args = Shizuku.UserServiceArgs(
        ComponentName(context, ZaraPrivilegedUserService::class.java),
    )
        .processNameSuffix("zara_privileged")
        .tag("zara-privileged-v1")
        .version(BuildConfig.VERSION_CODE)
        .debuggable(BuildConfig.DEBUG)
        .daemon(false)

    private val connection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName?, service: IBinder?) {
            remote.set(IZaraPrivilegedService.Stub.asInterface(service))
            binding.set(false)
        }

        override fun onServiceDisconnected(name: ComponentName?) {
            remote.set(null)
            binding.set(false)
        }
    }

    private val binderReceived = Shizuku.OnBinderReceivedListener { maybeBind() }
    private val binderDead = Shizuku.OnBinderDeadListener {
        remote.set(null)
        binding.set(false)
    }
    private val permissionResult = Shizuku.OnRequestPermissionResultListener { requestCode, grantResult ->
        if (requestCode == PERMISSION_REQUEST && grantResult == PackageManager.PERMISSION_GRANTED) {
            maybeBind()
        }
    }

    init {
        Shizuku.addBinderReceivedListenerSticky(binderReceived)
        Shizuku.addBinderDeadListener(binderDead)
        Shizuku.addRequestPermissionResultListener(permissionResult)
        maybeBind()
    }

    override val backend = AndroidBackend.SHIZUKU
    override val minimumAuthority = AndroidAuthorityLevel.UNRESTRICTED

    override val identity: String
        get() {
            val uid = runCatching { remote.get()?.uid() ?: Shizuku.getUid() }.getOrNull()
            return when (uid) {
                0 -> "sui:root:0"
                2000 -> "shizuku:shell:2000"
                null -> "shizuku:unknown"
                else -> "shizuku:uid:$uid"
            }
        }

    override fun isAvailable(): Boolean {
        maybeBind()
        return runCatching {
            Shizuku.pingBinder() &&
                !Shizuku.isPreV11() &&
                Shizuku.checkSelfPermission() == PackageManager.PERMISSION_GRANTED
        }.getOrDefault(false)
    }

    override fun supports(operation: String): Boolean = operation == "shell.exec"

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult {
        if (!isAvailable()) {
            if (runCatching { Shizuku.pingBinder() }.getOrDefault(false) &&
                runCatching { Shizuku.checkSelfPermission() }.getOrDefault(PackageManager.PERMISSION_DENIED) !=
                PackageManager.PERMISSION_GRANTED
            ) {
                runCatching { Shizuku.requestPermission(PERMISSION_REQUEST) }
            }
            return AndroidOperationResult.failed(
                AndroidOperationError.BACKEND_UNAVAILABLE,
                backend = backend.atom,
                identity = identity,
                message = "Shizuku/Sui binder or permission is unavailable",
            )
        }
        val service = awaitRemote()
            ?: return AndroidOperationResult.failed(
                AndroidOperationError.BACKEND_UNAVAILABLE,
                backend = backend.atom,
                identity = identity,
                message = "Shizuku UserService is still binding",
            )
        val command = request.arguments["command"]
            ?: return AndroidOperationResult.failed(
                AndroidOperationError.FAILED,
                backend.atom,
                identity,
                "command is required",
            )
        val timeout = request.arguments["timeout_ms"]
            ?.toIntOrNull()
            ?.coerceIn(1_000, 300_000)
            ?: 30_000
        return try {
            val output = service.executeShell(command, timeout)
            AndroidOperationResult.completed(backend.atom, identity, output)
        } catch (error: Throwable) {
            remote.set(null)
            binding.set(false)
            AndroidOperationResult.failed(
                AndroidOperationError.FAILED,
                backend.atom,
                identity,
                error::class.java.simpleName,
            )
        }
    }

    override fun cancel() {
        Shizuku.removeBinderReceivedListener(binderReceived)
        Shizuku.removeBinderDeadListener(binderDead)
        Shizuku.removeRequestPermissionResultListener(permissionResult)
        runCatching { Shizuku.unbindUserService(args, connection, false) }
        remote.set(null)
    }

    private fun maybeBind() {
        if (remote.get() != null || binding.get()) return
        val canBind = runCatching {
            Shizuku.pingBinder() &&
                !Shizuku.isPreV11() &&
                Shizuku.checkSelfPermission() == PackageManager.PERMISSION_GRANTED
        }.getOrDefault(false)
        if (!canBind || !binding.compareAndSet(false, true)) return
        runCatching { Shizuku.bindUserService(args, connection) }
            .onFailure { binding.set(false) }
    }

    private fun awaitRemote(): IZaraPrivilegedService? {
        remote.get()?.let { return it }
        maybeBind()
        repeat(20) {
            Thread.sleep(25)
            remote.get()?.let { return it }
        }
        return null
    }

    private companion object {
        const val PERMISSION_REQUEST = 0x5A41
    }
}
