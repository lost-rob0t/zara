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

enum class UpdatePhase {
    IDLE,
    CHECKING,
    CURRENT,
    AVAILABLE,
    DOWNLOADING,
    READY,
    INSTALLING,
    INSTALLED,
    FAILED,
}

data class UpdateState(
    val phase: UpdatePhase,
    val release: UpdateRelease? = null,
    val progressPercent: Int? = null,
    val downloadedApk: File? = null,
    val message: String? = null,
    val choices: List<UpdateRelease> = emptyList(),
    val selectedId: String? = null,
)

class AndroidUpdateManager(
    private val context: Context,
    private val currentVersion: String,
    private val currentSourceSha: String,
    private val releasesUrl: String = "https://api.github.com/repos/lost-rob0t/zara/releases?per_page=20",
    private val rollingManifestUrl: String =
        "https://github.com/lost-rob0t/zara/releases/download/android-latest/zara-latest.manifest.txt",
) : AutoCloseable {
    private data class Candidate(
        val release: UpdateRelease,
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
        candidate = null
        update(UpdateState(UpdatePhase.CHECKING))
        try {
            val releases = JSONArray(readText(releasesUrl, MAX_METADATA_BYTES))
            val versioned = (0 until releases.length())
                .mapNotNull { index -> releaseCandidate(releases.getJSONObject(index))?.release }
                .filter { release ->
                    UpdateSecurity.isInstallCandidate(release, currentVersion, currentSourceSha)
                }
                .sortedWith { left, right ->
                    when {
                        UpdateSecurity.isNewer(left.version, right.version) -> -1
                        UpdateSecurity.isNewer(right.version, left.version) -> 1
                        else -> 0
                    }
                }
            val rolling = runCatching { rollingMasterCandidate() }
                .getOrNull()
                ?.takeIf { release ->
                    UpdateSecurity.isInstallCandidate(release, currentVersion, currentSourceSha)
                }
            val choices = buildList {
                rolling?.let(::add)
                addAll(versioned)
            }
            val selected = choices.firstOrNull()
            candidate = selected?.let(::Candidate)
            if (selected == null) {
                UpdateState(
                    phase = UpdatePhase.CURRENT,
                    message = "Zara is current",
                    choices = choices,
                )
            } else {
                UpdateState(
                    phase = UpdatePhase.AVAILABLE,
                    release = selected,
                    message = if (selected.channel == UpdateChannel.Master) {
                        "Master (fastest green) selected"
                    } else {
                        "Versioned release selected"
                    },
                    choices = choices,
                    selectedId = selected.selectionId,
                )
            }.also(::update)
        } catch (error: Throwable) {
            candidate = null
            UpdateState(
                UpdatePhase.FAILED,
                message = error.message ?: "Update check failed",
            ).also(::update)
        }
    }

    fun select(selectionId: String): CompletableFuture<UpdateState> = submit {
        check(current.phase !in setOf(UpdatePhase.CHECKING, UpdatePhase.DOWNLOADING, UpdatePhase.INSTALLING)) {
            "Updater is busy"
        }
        val selected = current.choices.firstOrNull { it.selectionId == selectionId }
            ?: error("Selected update is no longer available")
        check(UpdateSecurity.isInstallCandidate(selected, currentVersion, currentSourceSha)) {
            "Selected update is already installed or older"
        }
        candidate = Candidate(selected)
        current.copy(
            phase = UpdatePhase.AVAILABLE,
            release = selected,
            progressPercent = null,
            downloadedApk = null,
            message = if (selected.channel == UpdateChannel.Master) {
                "Master (fastest green) selected"
            } else {
                "Versioned release selected"
            },
            selectedId = selected.selectionId,
        ).also(::update)
    }

    fun download(): CompletableFuture<UpdateState> = submit {
        val selected = candidate ?: error("Check for an update first")
        check(current.phase == UpdatePhase.AVAILABLE && current.release == selected.release) {
            "Update selection is stale; check again"
        }
        val choices = current.choices
        val selectedId = selected.release.selectionId
        update(
            UpdateState(
                UpdatePhase.DOWNLOADING,
                release = selected.release,
                progressPercent = 0,
                choices = choices,
                selectedId = selectedId,
            )
        )
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
                        choices = choices,
                        selectedId = selectedId,
                    )
                )
            }
            check(UpdateSecurity.verifySha256(destination, selected.release.sha256)) {
                destination.delete()
                "Downloaded APK checksum does not match release metadata"
            }
            UpdateState(
                UpdatePhase.READY,
                release = selected.release,
                progressPercent = 100,
                downloadedApk = destination,
                message = "Verified and ready for Android confirmation",
                choices = choices,
                selectedId = selectedId,
            ).also(::update)
        } catch (error: Throwable) {
            UpdateState(
                UpdatePhase.FAILED,
                release = selected.release,
                message = error.message ?: "Update download failed",
                choices = choices,
                selectedId = selectedId,
            ).also(::update)
        }
    }

    fun requestInstall(): Result<Unit> = runCatching {
        val ready = current
        val apk = ready.downloadedApk
        check(ready.phase == UpdatePhase.READY && apk?.isFile == true) {
            "No verified update is ready"
        }
        checkNotNull(ready.release)
        if (!context.packageManager.canRequestPackageInstalls()) {
            val settings = Intent(
                Settings.ACTION_MANAGE_UNKNOWN_APP_SOURCES,
                Uri.parse("package:${context.packageName}"),
            ).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            context.startActivity(settings)
            error("Allow Zara to install verified updates, then tap Install again")
        }
        submit {
            installVerifiedUpdate(ready, apk)
        }
        Unit
    }

    fun recordInstallStatus(status: Int, statusMessage: String?) {
        if (closed || status == PackageInstaller.STATUS_PENDING_USER_ACTION) return
        val snapshot = current
        val message = statusMessage?.takeIf { it.isNotBlank() }
        if (status == PackageInstaller.STATUS_SUCCESS) {
            snapshot.downloadedApk?.delete()
            update(
                UpdateState(
                    phase = UpdatePhase.INSTALLED,
                    release = snapshot.release,
                    progressPercent = 100,
                    message = message ?: "Zara update installed",
                )
            )
            return
        }

        val retryable = snapshot.downloadedApk?.isFile == true && snapshot.release != null
        update(
            snapshot.copy(
                phase = if (retryable) UpdatePhase.READY else UpdatePhase.FAILED,
                message = message ?: "Android rejected the update installation",
            )
        )
    }

    private fun installVerifiedUpdate(ready: UpdateState, apk: File): UpdateState {
        val release = checkNotNull(ready.release)
        return try {
            check(UpdateSecurity.verifySha256(apk, release.sha256)) {
                apk.delete()
                "Cached update checksum no longer matches"
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
            ready.copy(
                phase = UpdatePhase.INSTALLING,
                release = release,
                progressPercent = 100,
                downloadedApk = apk,
                message = "Waiting for Android installation confirmation",
            ).also(::update)
        } catch (error: Throwable) {
            val retryable = apk.isFile
            ready.copy(
                phase = if (retryable) UpdatePhase.READY else UpdatePhase.FAILED,
                downloadedApk = apk.takeIf(File::isFile),
                message = error.message ?: "Update installation failed",
            ).also(::update)
        }
    }

    private fun rollingMasterCandidate(): UpdateRelease {
        val manifest = readText(rollingManifestUrl, MAX_MANIFEST_BYTES)
            .lineSequence()
            .map(String::trim)
            .filter { it.isNotEmpty() && !it.startsWith("#") }
            .associate { line ->
                val separator = line.indexOf('=')
                require(separator > 0) { "Rolling master manifest is malformed" }
                line.substring(0, separator) to line.substring(separator + 1)
            }
        require(manifest["schema"] == "1") { "Rolling master manifest schema is unsupported" }
        require(manifest["channel"] == "android-latest") { "Rolling master channel is invalid" }
        require(manifest["mutable"] == "true") { "Rolling master manifest must declare mutability" }
        val sourceSha = manifest["source_sha"] ?: error("Rolling master source SHA is missing")
        val apkName = manifest["phone_apk"] ?: error("Rolling master APK name is missing")
        require(apkName == "zara-latest.apk") { "Rolling master APK name is unexpected" }
        val sha256 = manifest["phone_sha256"] ?: error("Rolling master checksum is missing")
        val apkUrl = rollingManifestUrl.substringBeforeLast('/') + "/" + apkName
        val release = UpdateRelease(
            version = "master",
            sourceSha = sourceSha,
            apkUrl = apkUrl,
            sha256 = sha256,
            channel = UpdateChannel.Master,
        )
        return UpdateSecurity.validate(release).getOrThrow()
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
        val checksumUrl = UpdateSecurity.requireTrustedTransport(
            checksum.optString("browser_download_url")
        )
        val checksumText = readText(checksumUrl, MAX_CHECKSUM_BYTES)
        val sha256 = checksumText.trim().substringBefore(' ').lowercase()
        val release = UpdateRelease(
            version = version,
            sourceSha = sourceSha,
            apkUrl = apk.optString("browser_download_url"),
            sha256 = sha256,
        )
        return UpdateSecurity.validate(release).getOrNull()?.let(::Candidate)
    }

    private fun readText(url: String, maxBytes: Int): String {
        val connection = openTrusted(url)
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
        val connection = openTrusted(url)
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

    private fun openTrusted(url: String): HttpURLConnection {
        var currentUrl = UpdateSecurity.requireTrustedTransport(url)
        repeat(MAX_REDIRECTS + 1) { redirectCount ->
            val connection = (URL(currentUrl).openConnection() as HttpURLConnection).apply {
                connectTimeout = 10_000
                readTimeout = 30_000
                instanceFollowRedirects = false
                setRequestProperty("Accept", "application/vnd.github+json")
                setRequestProperty("User-Agent", "zara-android/$currentVersion")
            }
            val status = connection.responseCode
            if (status !in REDIRECT_CODES) return connection
            if (redirectCount >= MAX_REDIRECTS) {
                connection.disconnect()
                error("Update redirect limit exceeded")
            }
            val location = connection.getHeaderField("Location")
            check(!location.isNullOrBlank()) { "Update redirect is missing a location" }
            val next = URL(URL(currentUrl), location).toString()
            connection.disconnect()
            currentUrl = UpdateSecurity.requireTrustedTransport(next)
        }
        error("Update redirect limit exceeded")
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
        private const val MAX_MANIFEST_BYTES = 16 * 1024
        private const val MAX_APK_BYTES = 256L * 1024 * 1024
        private const val MAX_REDIRECTS = 5
        private val REDIRECT_CODES = setOf(
            HttpURLConnection.HTTP_MOVED_PERM,
            HttpURLConnection.HTTP_MOVED_TEMP,
            HttpURLConnection.HTTP_SEE_OTHER,
            307,
            308,
        )
    }
}
