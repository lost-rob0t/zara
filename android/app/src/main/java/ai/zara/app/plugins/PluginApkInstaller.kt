package ai.zara.app.plugins

import android.content.ClipData
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.content.pm.ApplicationInfo
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Build
import androidx.core.content.FileProvider
import java.io.File
import java.io.OutputStream
import java.io.Serializable
import java.util.UUID
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.NonCancellable
import kotlinx.coroutines.currentCoroutineContext
import kotlinx.coroutines.ensureActive
import kotlinx.coroutines.withContext

class PluginApkFileProvider : FileProvider()

data class StagedPluginApk(
    val fileName: String,
    val identity: PluginApkIdentity,
    val sha256: String,
    val bytes: Long,
) : Serializable

class PluginApkInstaller(private val context: Context) {
    private val directory = File(context.cacheDir, "plugin-install")
    private val authority = "${context.packageName}.plugin-apks"

    suspend fun stage(uri: Uri): StagedPluginApk {
        require(uri.scheme == "content") { "Choose an APK with the Android file picker" }
        var temporary: File? = null
        try {
            return withContext(Dispatchers.IO) {
                check(directory.mkdirs() || directory.isDirectory) { "APK cache is unavailable" }
                val file = File(directory, "${UUID.randomUUID()}.apk")
                temporary = file
                val jobContext = currentCoroutineContext()
                val copied = checkNotNull(context.contentResolver.openInputStream(uri)).use { input ->
                    file.outputStream().use { output ->
                        PluginApkPolicy.copy(input, output) { jobContext.ensureActive() }
                    }
                }
                val identity = inspect(file)
                PluginApkPolicy.validateIdentity(identity, context.packageName, Build.VERSION.SDK_INT)
                StagedPluginApk(file.name, identity, copied.sha256, copied.bytes)
            }
        } catch (error: Exception) {
            withContext(NonCancellable + Dispatchers.IO) { temporary?.delete() }
            throw error
        }
    }

    @Suppress("DEPRECATION")
    suspend fun installIntent(candidate: StagedPluginApk): Intent = withContext(Dispatchers.IO) {
        check(context.packageManager.canRequestPackageInstalls()) { "Android install permission is required" }
        val file = candidateFile(candidate)
        check(file.isFile && file.length() == candidate.bytes) { "Select the APK again" }
        val jobContext = currentCoroutineContext()
        val actual = file.inputStream().use { input ->
            PluginApkPolicy.copy(input, object : OutputStream() {
                override fun write(value: Int) = Unit
                override fun write(buffer: ByteArray, offset: Int, length: Int) = Unit
            }) { jobContext.ensureActive() }
        }
        check(actual.sha256 == candidate.sha256) { "APK changed after review" }
        val identity = inspect(file)
        PluginApkPolicy.validateIdentity(identity, context.packageName, Build.VERSION.SDK_INT)
        check(identity == candidate.identity) { "APK identity changed after review" }
        val uri = FileProvider.getUriForFile(context, authority, file)
        val intent = Intent(Intent.ACTION_INSTALL_PACKAGE).apply {
            setDataAndType(uri, APK_MIME)
            clipData = ClipData.newRawUri("Plugin APK", uri)
            addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
            putExtra(Intent.EXTRA_RETURN_RESULT, true)
        }
        val systemInstaller = context.packageManager.queryIntentActivities(
            intent,
            PackageManager.MATCH_DEFAULT_ONLY or PackageManager.MATCH_SYSTEM_ONLY,
        ).firstOrNull {
            it.activityInfo.exported &&
                (it.activityInfo.applicationInfo.flags and ApplicationInfo.FLAG_SYSTEM) != 0
        }?.activityInfo ?: error("Android package installer is unavailable")
        intent.component = ComponentName(systemInstaller.packageName, systemInstaller.name)
        intent
    }

    suspend fun discard(candidate: StagedPluginApk) = withContext(NonCancellable + Dispatchers.IO) {
        val file = candidateFile(candidate)
        val uri = FileProvider.getUriForFile(context, authority, file)
        context.revokeUriPermission(uri, Intent.FLAG_GRANT_READ_URI_PERMISSION)
        check(!file.exists() || file.delete()) { "Temporary APK could not be removed" }
        Unit
    }

    private fun candidateFile(candidate: StagedPluginApk): File {
        require(candidate.fileName.matches(Regex("[0-9a-f-]{36}\\.apk"))) { "Invalid APK cache identity" }
        return File(directory, candidate.fileName).also {
            check(it.canonicalFile.parentFile == directory.canonicalFile) { "Invalid APK cache path" }
        }
    }

    @Suppress("DEPRECATION")
    private fun inspect(file: File): PluginApkIdentity {
        val info = checkNotNull(context.packageManager.getPackageArchiveInfo(
            file.absolutePath,
            PackageManager.GET_SIGNING_CERTIFICATES,
        )) { "Not a readable Android APK" }
        val application = checkNotNull(info.applicationInfo) { "APK application metadata is missing" }
        return PluginApkIdentity(
            packageName = info.packageName,
            versionName = info.versionName ?: info.longVersionCode.toString(),
            versionCode = info.longVersionCode,
            signerSha256 = info.signingInfo?.apkContentsSigners.orEmpty()
                .map { PluginApkPolicy.certificateDigest(it.toByteArray()) }.sorted(),
            minSdk = application.minSdkVersion,
            hasSplits = !info.splitNames.isNullOrEmpty(),
        )
    }

    companion object {
        const val APK_MIME = "application/vnd.android.package-archive"
    }
}
