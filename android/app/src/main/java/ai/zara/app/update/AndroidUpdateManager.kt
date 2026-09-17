package ai.zara.app.update

import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.content.pm.PackageInstaller
import android.net.Uri
import android.provider.Settings
import java.io.ByteArrayOutputStream
import java.io.File
import java.net.HttpURLConnection
import java.net.URL
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import org.json.JSONArray
import org.json.JSONObject

enum class UpdatePhase { IDLE, CHECKING, CURRENT, AVAILABLE, DOWNLOADING, READY, FAILED }

data class UpdateState(
    val phase: UpdatePhase,
    val release: UpdateRelease? = null,
    val progressPercent: Int? = null,
    val downloadedApk: File? = null,
    val message: String? = null,
)

class AndroidUpdateManager(
    private val context: Context,
    private val currentVersion: String,
    private val releasesUrl: String = "https://api.github.com/repos/lost-rob0t/zara/releases?per_page=20",
) : AutoCloseable {
    private data class Candidate(
        val release: UpdateRelease,
        val checksumUrl: String,
    )

    private val executor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-update").apply { isDaemon = true }
    }
    private val updateDirectory = File(context.cacheDir, "verified-updates")
    @Volatile private var current = UpdateState(UpdatePhase.IDLE)
    @Volatile private var candidate: Candidate? = null
    @Volatile private var observer: ((UpdateState) -> Unit)? = null
    @Volatile private var closed = false

    fun state(): UpdateState = current

    fun setObserver(value: ((UpdateState) -> Unit)?) {
        observer = value
        value?.invoke(current)
    }

    fun check(): CompletableFuture<UpdateState> = submit {
        update(UpdateState(UpdatePhase.CHECKING))
        try {
            val releases = JSONArray(readText(releasesUrl, MAX_METADATA_BYTES))
            val available = (0 until releases.length())
                .mapNotNull { index -> releaseCandidate(releases.getJSONObject(index)) }
                .filter { UpdateSecurity.isNewer(it.release.version, currentVersion) }
                .maxWithOrNull { left, right ->
                    when {
                        UpdateSecurity.isNewer(left.release.version, right.release.version) -> 1
                        UpdateSecurity.isNewer(right.release.version, left.release.version) -> -1
                        else -> 0
                    }
                }
            candidate = available
            if (available == null) {
                UpdateState(UpdatePhase.CURRENT, message = "Zara is current")
            } else {
                UpdateState(UpdatePhase.AVAILABLE, release = available.release)
            }.also(::update)
        } catch (error: Throwable) {
            UpdateState(
                UpdatePhase.FAILED,
                message = error.message ?: "Update check failed",
            ).also(::update)
        }
    }

    fun download(): CompletableFuture<UpdateState> = submit {
        val selected = candidate ?: error("Check for an update first")
        update(UpdateState(UpdatePhase.DOWNLOADING, release = selected.release, progressPercent = 0))
        try {
            check(updateDirectory.mkdirs() || updateDirectory.isDirectory) {
                "Update cache is unavailable"
            }
            val destination = File(updateDirectory, "zara-${selected.release.version}.apk")
            downloadFile(selected.release.apkUrl, destination) { percent ->
                update(
                    UpdateState(
                        UpdatePhase.DOWNLOADING,
                        release = selected.release,
                        progressPercent = percent,
                    )
                )
            }
            check(UpdateSecurity.verifySha256(destination, selected.release.sha256)) {
                destination.delete()
                "Downloaded APK checksum does not match the signed release metadata"
            }
            UpdateState(
                UpdatePhase.READY,
                release = selected.release,
                progressPercent = 100,
                downloadedApk = destination,
                message = "Verified and ready for Android confirmation",
            ).also(::update)
        } catch (error: Throwable) {
            UpdateState(
                UpdatePhase.FAILED,
                release = selected.release,
                message = error.message ?: "Update download failed",
            ).also(::update)
        }
    }

    fun requestInstall(): Result<Unit> = runCatching {
        val apk = current.downloadedApk
        check(current.phase == UpdatePhase.READY && apk?.isFile == true) {
            "No verified update is ready"
        }
        check(UpdateSecurity.verifySha256(apk, checkNotNull(current.release).sha256)) {
            "Cached update checksum no longer matches"
        }
        if (!context.packageManager.canRequestPackageInstalls()) {
            val settings = Intent(
                Settings.ACTION_MANAGE_UNKNOWN_APP_SOURCES,
                Uri.parse("package:${context.packageName}"),
            ).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            context.startActivity(settings)
            error("Allow Zara to install verified updates, then tap Install again")
        }
        val installer = context.packageManager.packageInstaller
        val parameters = PackageInstaller.SessionParams(
            PackageInstaller.SessionParams.MODE_FULL_INSTALL,
        ).apply {
            setAppPackageName(context.packageName)
        }
        val sessionId = installer.createSession(parameters)
        val session = installer.openSession(sessionId)
        try {
            apk.inputStream().use { input ->
                session.openWrite("zara.apk", 0, apk.length()).use { output ->
                    input.copyTo(output)
                    session.fsync(output)
                }
            }
            val callback = PendingIntent.getBroadcast(
                context,
                sessionId,
                Intent(context, UpdateInstallReceiver::class.java),
                PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_MUTABLE,
            )
            session.commit(callback.intentSender)
        } catch (error: Throwable) {
            session.abandon()
            throw error
        } finally {
            session.close()
        }
    }

    private fun releaseCandidate(json: JSONObject): Candidate? {
        if (json.optBoolean("draft", false)) return null
        val version = json.optString("tag_name").removePrefix("v")
        val sourceSha = json.optString("target_commitish")
        val assets = json.optJSONArray("assets") ?: return null
        val apk = (0 until assets.length())
            .map { assets.getJSONObject(it) }
            .firstOrNull { it.optString("name").endsWith(".apk") }
            ?: return null
        val checksum = (0 until assets.length())
            .map { assets.getJSONObject(it) }
            .firstOrNull { it.optString("name") == "${apk.optString("name")}.sha256" }
            ?: return null
        val checksumUrl = checksum.optString("browser_download_url")
        val checksumText = readText(checksumUrl, MAX_CHECKSUM_BYTES)
        val sha256 = checksumText.trim().substringBefore(' ').lowercase()
        val release = UpdateRelease(
            version = version,
            sourceSha = sourceSha,
            apkUrl = apk.optString("browser_download_url"),
            sha256 = sha256,
        )
        return UpdateSecurity.validate(release).getOrNull()?.let {
            Candidate(it, checksumUrl)
        }
    }

    private fun readText(url: String, maxBytes: Int): String {
        val connection = open(url)
        return try {
            check(connection.responseCode in 200..299) { "GitHub returned ${connection.responseCode}" }
            val length = connection.contentLengthLong
            check(length <= maxBytes || length < 0) { "Update metadata is too large" }
            connection.inputStream.use { input ->
                val output = ByteArrayOutputStream()
                val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
                while (true) {
                    val read = input.read(buffer)
                    if (read < 0) break
                    check(output.size() + read <= maxBytes) { "Update metadata is too large" }
                    output.write(buffer, 0, read)
                }
                output.toByteArray().decodeToString()
            }
        } finally {
            connection.disconnect()
        }
    }

    private fun downloadFile(url: String, destination: File, progress: (Int) -> Unit) {
        val temporary = File(destination.parentFile, "${destination.name}.part")
        val connection = open(url)
        try {
            check(connection.responseCode in 200..299) { "GitHub returned ${connection.responseCode}" }
            val total = connection.contentLengthLong
            check(total in 1..MAX_APK_BYTES) { "APK size is missing or outside the safe limit" }
            var received = 0L
            connection.inputStream.use { input ->
                temporary.outputStream().use { output ->
                    val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
                    while (true) {
                        val read = input.read(buffer)
                        if (read < 0) break
                        received += read
                        check(received <= MAX_APK_BYTES) { "APK exceeded the safe size limit" }
                        output.write(buffer, 0, read)
                        progress(((received * 100) / total).toInt().coerceIn(0, 100))
                    }
                }
            }
            check(received == total) { "APK download was truncated" }
            if (destination.exists()) check(destination.delete()) { "Old update cache could not be replaced" }
            check(temporary.renameTo(destination)) { "Verified update cache could not be finalized" }
        } finally {
            connection.disconnect()
            if (temporary.exists()) temporary.delete()
        }
    }

    private fun open(url: String): HttpURLConnection {
        require(url.startsWith("https://")) { "Update transport must use HTTPS" }
        return (URL(url).openConnection() as HttpURLConnection).apply {
            connectTimeout = 10_000
            readTimeout = 30_000
            instanceFollowRedirects = true
            setRequestProperty("Accept", "application/vnd.github+json")
            setRequestProperty("User-Agent", "zara-android/$currentVersion")
        }
    }

    private fun update(state: UpdateState) {
        current = state
        observer?.invoke(state)
    }

    private fun <T> submit(block: () -> T): CompletableFuture<T> {
        if (closed) return CompletableFuture.failedFuture(IllegalStateException("Updater is closed"))
        val future = CompletableFuture<T>()
        executor.execute {
            try {
                future.complete(block())
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        return future
    }

    override fun close() {
        closed = true
        observer = null
        executor.shutdownNow()
    }

    companion object {
        private const val MAX_METADATA_BYTES = 2 * 1024 * 1024
        private const val MAX_CHECKSUM_BYTES = 4 * 1024
        private const val MAX_APK_BYTES = 256L * 1024 * 1024
    }
}
