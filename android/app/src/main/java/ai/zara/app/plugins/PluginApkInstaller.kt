package ai.zara.app.plugins

import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.content.pm.PackageInstaller
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Build
import android.os.Handler
import android.os.Looper
import java.io.File
import java.io.IOException
import java.security.MessageDigest
import java.util.UUID
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.CopyOnWriteArraySet
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import org.json.JSONArray
import org.json.JSONObject

internal enum class PluginInstallPhase {
    IDLE, VERIFYING, REVIEW, PERMISSION_REQUIRED, INSTALLING, INSTALLED, FAILED,
}

internal data class PluginApkCandidate(
    val fileName: String,
    val packageName: String,
    val version: String,
    val sha256: String,
    val certificates: List<String>,
)

internal data class PluginInstallState(
    val phase: PluginInstallPhase = PluginInstallPhase.IDLE,
    val candidate: PluginApkCandidate? = null,
    val sessionId: Int = -1,
    val nonce: String = "",
    val message: String? = null,
)

internal class PluginApkInstaller(private val context: Context) {
    private val lock = Any()
    private val preferences = context.getSharedPreferences("plugin-apk-installer", Context.MODE_PRIVATE)
    private val directory = File(context.cacheDir, "verified-plugin-apks")
    private val handler = Handler(Looper.getMainLooper())
    private val observers = CopyOnWriteArraySet<(PluginInstallState) -> Unit>()
    private val executor = ThreadPoolExecutor(
        1, 1, 0L, TimeUnit.MILLISECONDS, ArrayBlockingQueue(1),
        { task -> Thread(task, "zara-plugin-installer").apply { isDaemon = true } },
        ThreadPoolExecutor.AbortPolicy(),
    )
    @Volatile private var working = false
    @Volatile private var current = restore()

    fun state(): PluginInstallState = current

    fun observe(observer: (PluginInstallState) -> Unit): AutoCloseable {
        observers.add(observer)
        handler.post { if (observer in observers) observer(current) }
        return AutoCloseable { observers.remove(observer) }
    }

    fun prepare(uri: Uri, expectedSha256: String) {
        submit(editablePhases, clearCandidate = true) { previous ->
            previous.candidate?.let { cachedFile(it).delete() }
            val checksum = PluginApkSecurity.normalizeSha256(expectedSha256)
            check(directory.mkdirs() || directory.isDirectory) { "Plugin APK storage is unavailable." }
            val apk = File.createTempFile("plugin-", ".apk", directory)
            var retained = false
            try {
                val input = context.contentResolver.openInputStream(uri)
                    ?: throw IOException("The APK provider returned no file.")
                input.use { source ->
                    apk.outputStream().use { output ->
                        PluginApkSecurity.copyVerified(source, output, checksum)
                        output.fd.sync()
                    }
                }
                val candidate = inspect(apk, checksum)
                publish(PluginInstallState(PluginInstallPhase.REVIEW, candidate))
                retained = true
            } finally {
                if (!retained) apk.delete()
            }
        }
    }

    fun requestInstall() {
        if (current.candidate == null) return
        submit(setOf(PluginInstallPhase.REVIEW, PluginInstallPhase.PERMISSION_REQUIRED, PluginInstallPhase.FAILED)) { previous ->
            val candidate = checkNotNull(previous.candidate)
            if (!context.packageManager.canRequestPackageInstalls()) {
                publish(
                    PluginInstallState(
                        PluginInstallPhase.PERMISSION_REQUIRED,
                        candidate,
                        message = "Allow Zara to install apps, then return here and tap Install plugin.",
                    ),
                )
                return@submit
            }
            val apk = cachedFile(candidate)
            check(apk.isFile) { "The staged APK is no longer available. Select it again." }
            val installer = context.packageManager.packageInstaller
            val parameters = PackageInstaller.SessionParams(PackageInstaller.SessionParams.MODE_FULL_INSTALL).apply {
                setAppPackageName(candidate.packageName)
                setSize(apk.length())
                if (Build.VERSION.SDK_INT >= 31) {
                    setRequireUserAction(PackageInstaller.SessionParams.USER_ACTION_REQUIRED)
                }
            }
            val sessionId = installer.createSession(parameters)
            val nonce = UUID.randomUUID().toString()
            var session: PackageInstaller.Session? = null
            var committed = false
            try {
                publish(
                    PluginInstallState(PluginInstallPhase.VERIFYING, candidate, sessionId, nonce),
                    durable = true,
                )
                val opened = installer.openSession(sessionId)
                session = opened
                apk.inputStream().use { input ->
                    opened.openWrite("plugin.apk", 0, apk.length()).use { output ->
                        PluginApkSecurity.copyVerified(input, output, candidate.sha256)
                        opened.fsync(output)
                    }
                }
                val callback = PendingIntent.getBroadcast(
                    context,
                    sessionId,
                    Intent(context, PluginInstallReceiver::class.java)
                        .setAction(INSTALL_RESULT_ACTION)
                        .putExtra(EXTRA_NONCE, nonce),
                    PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_MUTABLE,
                )
                publish(
                    PluginInstallState(
                        PluginInstallPhase.INSTALLING,
                        candidate,
                        sessionId,
                        nonce,
                        "Waiting for Android installation confirmation.",
                    ),
                    durable = true,
                )
                opened.commit(callback.intentSender)
                committed = true
            } finally {
                if (!committed) runCatching { installer.abandonSession(sessionId) }
                runCatching { session?.close() }
            }
        }
    }

    fun permissionResult() {
        synchronized(lock) {
            if (current.phase != PluginInstallPhase.PERMISSION_REQUIRED) return
            val granted = context.packageManager.canRequestPackageInstalls()
            publish(
                current.copy(
                    phase = if (granted) PluginInstallPhase.REVIEW else PluginInstallPhase.PERMISSION_REQUIRED,
                    message = if (granted) {
                        "Permission granted. Tap Install plugin to continue."
                    } else {
                        "Install permission was not granted. You can retry or choose another APK."
                    },
                ),
            )
        }
    }

    fun dismiss() {
        synchronized(lock) {
            if (working) return
            if (current.sessionId >= 0) {
                runCatching { context.packageManager.packageInstaller.abandonSession(current.sessionId) }
            }
            current.candidate?.let { cachedFile(it).delete() }
            publish(PluginInstallState(message = "Installer cleared. Any completed Android installation is unchanged."))
        }
    }

    @Suppress("DEPRECATION")
    fun receiveResult(intent: Intent): Intent? = synchronized(lock) {
        if (!matchesCurrentSession(intent)) return@synchronized null
        when (intent.getIntExtra(PackageInstaller.EXTRA_STATUS, PackageInstaller.STATUS_FAILURE)) {
            PackageInstaller.STATUS_PENDING_USER_ACTION -> {
                val confirmation = intent.getParcelableExtra<Intent>(Intent.EXTRA_INTENT)
                if (confirmation == null) confirmationFailed(intent)
                confirmation
            }
            PackageInstaller.STATUS_SUCCESS -> {
                current.candidate?.let { cachedFile(it).delete() }
                publish(
                    current.copy(
                        phase = PluginInstallPhase.INSTALLED,
                        sessionId = -1,
                        nonce = "",
                        message = "Android installed the APK. Trust, enablement, and capabilities were not granted.",
                    ),
                )
                null
            }
            else -> {
                val status = intent.getIntExtra(PackageInstaller.EXTRA_STATUS, PackageInstaller.STATUS_FAILURE)
                val message = when (status) {
                    PackageInstaller.STATUS_FAILURE_ABORTED -> "Android installation was cancelled."
                    PackageInstaller.STATUS_FAILURE_BLOCKED -> "Android blocked installation. Review device or install-source restrictions."
                    PackageInstaller.STATUS_FAILURE_CONFLICT -> "The APK conflicts with an installed package or signing certificate."
                    PackageInstaller.STATUS_FAILURE_INCOMPATIBLE -> "This APK is not compatible with this device."
                    PackageInstaller.STATUS_FAILURE_INVALID -> "Android rejected the APK. Obtain a complete, signed APK from its publisher."
                    PackageInstaller.STATUS_FAILURE_STORAGE -> "Android could not install the APK. Free device storage and retry."
                    else -> "Android could not install the APK. Review its system message and retry."
                }
                publish(current.copy(phase = PluginInstallPhase.FAILED, sessionId = -1, nonce = "", message = message))
                null
            }
        }
    }

    fun confirmationFailed(intent: Intent) {
        synchronized(lock) {
            if (!matchesCurrentSession(intent)) return
            runCatching { context.packageManager.packageInstaller.abandonSession(current.sessionId) }
            publish(
                current.copy(
                    phase = PluginInstallPhase.FAILED,
                    sessionId = -1,
                    nonce = "",
                    message = "Android's confirmation screen could not open. Keep Zara open and retry.",
                ),
            )
        }
    }

    private fun matchesCurrentSession(intent: Intent): Boolean =
        current.phase == PluginInstallPhase.INSTALLING &&
            intent.action == INSTALL_RESULT_ACTION &&
            PluginApkSecurity.matchesCallback(
                current.sessionId,
                current.nonce,
                intent.getIntExtra(PackageInstaller.EXTRA_SESSION_ID, -1),
                intent.getStringExtra(EXTRA_NONCE),
            )

    @Suppress("DEPRECATION")
    private fun inspect(apk: File, checksum: String): PluginApkCandidate {
        val info = context.packageManager.getPackageArchiveInfo(
            apk.absolutePath,
            PackageManager.GET_SIGNING_CERTIFICATES,
        ) ?: throw IllegalArgumentException(
            "Select a complete Android APK, not a ZIP, split APK bundle, or desktop plugin.",
        )
        val certificates = info.signingInfo?.apkContentsSigners?.map { signature ->
            PluginApkSecurity.hex(MessageDigest.getInstance("SHA-256").digest(signature.toByteArray()))
        }.orEmpty()
        PluginApkSecurity.validateIdentity(info.packageName, context.packageName, certificates)
        return PluginApkCandidate(
            apk.name,
            info.packageName,
            (info.versionName ?: info.longVersionCode.toString()).take(120),
            checksum,
            certificates,
        )
    }

    private fun cachedFile(candidate: PluginApkCandidate): File {
        require(candidate.fileName.matches(Regex("plugin-[A-Za-z0-9-]+\\.apk"))) {
            "Invalid APK cache identity."
        }
        return File(directory, candidate.fileName)
    }

    private fun submit(
        allowed: Set<PluginInstallPhase>,
        clearCandidate: Boolean = false,
        action: (PluginInstallState) -> Unit,
    ) {
        synchronized(lock) {
            if (working || current.phase !in allowed) return
            val previous = current
            working = true
            executor.execute {
                try {
                    publish(
                        PluginInstallState(
                            PluginInstallPhase.VERIFYING,
                            if (clearCandidate) null else previous.candidate,
                            message = "Verifying APK…",
                        ),
                    )
                    action(previous)
                } catch (failure: Exception) {
                    synchronized(lock) {
                        if (current.phase != PluginInstallPhase.INSTALLED) {
                            val message = when (failure) {
                                is IllegalArgumentException,
                                is IllegalStateException -> failure.message?.take(240)
                                is SecurityException -> "Android denied access. Choose the APK again or review install permissions."
                                else -> "The APK could not be read or installed. Check the file and available storage, then retry."
                            }
                            publish(
                                current.copy(
                                    phase = PluginInstallPhase.FAILED,
                                    sessionId = -1,
                                    nonce = "",
                                    message = message ?: "Plugin installation failed. Choose the APK again.",
                                ),
                            )
                        }
                    }
                } finally {
                    working = false
                }
            }
        }
    }

    private fun publish(value: PluginInstallState, durable: Boolean = false) {
        synchronized(lock) {
            val json = JSONObject()
                .put("phase", value.phase.name)
                .put("session", value.sessionId)
                .put("nonce", value.nonce)
                .put("message", value.message ?: JSONObject.NULL)
            value.candidate?.let { candidate ->
                json.put(
                    "candidate",
                    JSONObject()
                        .put("file", candidate.fileName)
                        .put("package", candidate.packageName)
                        .put("version", candidate.version)
                        .put("sha256", candidate.sha256)
                        .put("certificates", JSONArray(candidate.certificates)),
                )
            }
            val saved = preferences.edit().putString("state", json.toString()).commit()
            if (durable && !saved) throw IOException("Installation state could not be saved.")
            current = value
            handler.post { observers.forEach { it(value) } }
        }
    }

    private fun restore(): PluginInstallState = try {
        val raw = preferences.getString("state", null)
        if (raw == null || raw.length > 16_384) {
            PluginInstallState()
        } else {
            val json = JSONObject(raw)
            val candidate = json.optJSONObject("candidate")?.let { item ->
                val certificates = item.getJSONArray("certificates")
                require(certificates.length() in 1..8)
                PluginApkCandidate(
                    item.getString("file"),
                    item.getString("package"),
                    item.getString("version").take(120),
                    PluginApkSecurity.normalizeSha256(item.getString("sha256")),
                    (0 until certificates.length()).map(certificates::getString),
                ).also {
                    cachedFile(it)
                    PluginApkSecurity.validateIdentity(it.packageName, context.packageName, it.certificates)
                }
            }
            val phase = PluginInstallPhase.valueOf(json.getString("phase"))
            val session = json.optInt("session", -1)
            if (phase == PluginInstallPhase.VERIFYING) {
                if (session >= 0) runCatching { context.packageManager.packageInstaller.abandonSession(session) }
                PluginInstallState(
                    PluginInstallPhase.FAILED,
                    candidate,
                    message = "APK preparation was interrupted. Select the APK again or retry.",
                )
            } else {
                PluginInstallState(
                    phase,
                    candidate,
                    session,
                    json.optString("nonce"),
                    if (json.isNull("message")) null else json.optString("message").take(240),
                )
            }
        }
    } catch (_: Exception) {
        PluginInstallState(
            PluginInstallPhase.FAILED,
            message = "Installer state could not be restored. Select the APK again.",
        )
    }

    companion object {
        const val INSTALL_RESULT_ACTION = "ai.zara.app.PLUGIN_INSTALL_RESULT"
        const val EXTRA_NONCE = "ai.zara.app.plugin_install_nonce"
        private val editablePhases = setOf(
            PluginInstallPhase.IDLE,
            PluginInstallPhase.REVIEW,
            PluginInstallPhase.PERMISSION_REQUIRED,
            PluginInstallPhase.INSTALLED,
            PluginInstallPhase.FAILED,
        )
    }
}
