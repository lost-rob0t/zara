package ai.zara.org.storage

import ai.zara.org.core.OrgDailySpec
import ai.zara.org.core.OrgParser
import ai.zara.org.core.OrgTask
import ai.zara.org.core.OrgTangler
import ai.zara.org.core.cycleTodoState
import android.content.Context
import android.net.Uri
import android.os.Bundle
import android.util.Base64
import java.time.ZoneId

interface OrgRepository {
    fun listOrgFiles(): List<OrgFileRef>
    fun read(file: OrgFileRef): String
    fun write(file: OrgFileRef, text: String)
    fun writeRelative(relativePath: String, text: String): OrgFileRef
    fun appendAgendaCapture(text: String, relativePath: String = "agenda/inbox.org"): OrgFileRef
    fun allTasks(): List<OrgTask>
    fun cycleTodo(task: OrgTask): OrgTask
    fun tangle(file: OrgFileRef): List<OrgFileRef>
}

enum class OrgHomeMode {
    SHARED,
    CUSTOM_SAF,
}

data class OrgHomeSelection(
    val mode: OrgHomeMode,
    val customTreeUri: Uri? = null,
)

data class OrgDailyConfiguration(
    val relativePathTemplate: String,
    val datePattern: String,
    val zoneId: String,
) {
    fun toSpec(): OrgDailySpec = OrgDailySpec(
        relativePathTemplate = relativePathTemplate,
        datePattern = datePattern,
        zoneId = ZoneId.of(zoneId),
    )
}

object SharedOrgHomeContract {
    const val AUTHORITY = "ai.zara.org.sync.home"
    const val PERMISSION = "ai.zara.org.permission.ORG_HOME"
    const val COLUMN_PATH = "path"
    const val COLUMN_NAME = "name"
    const val COLUMN_URI = "uri"
    const val METHOD_APPEND_ORG = "append_org"
    const val METHOD_WRITE_TEXT = "write_text"
    const val EXTRA_TEXT = "text"
    const val EXTRA_URI = "uri"

    val filesUri: Uri = Uri.parse("content://$AUTHORITY/files")

    fun fileUri(relativePath: String): Uri =
        Uri.parse("content://$AUTHORITY/file/${encode(relativePath)}")

    fun decodeFileUri(uri: Uri): String {
        require(uri.authority == AUTHORITY) { "Unexpected Org Sync authority" }
        require(uri.pathSegments.firstOrNull() == "file") { "Not an Org Sync file URI" }
        val token = uri.pathSegments.getOrNull(1) ?: error("Missing Org Sync file token")
        return decode(token)
    }

    private fun encode(value: String): String =
        Base64.encodeToString(value.toByteArray(Charsets.UTF_8), Base64.URL_SAFE or Base64.NO_WRAP or Base64.NO_PADDING)

    private fun decode(value: String): String =
        String(Base64.decode(value, Base64.URL_SAFE or Base64.NO_WRAP or Base64.NO_PADDING), Charsets.UTF_8)
}

object OrgHome {
    private const val PREFS = "zara-org-home"
    private const val KEY_MODE = "mode"
    private const val KEY_CUSTOM_URI = "custom-tree-uri"
    private const val KEY_DAILY_PATH_TEMPLATE = "daily-path-template"
    private const val KEY_DAILY_DATE_PATTERN = "daily-date-pattern"
    private const val KEY_DAILY_ZONE_ID = "daily-zone-id"

    fun selection(context: Context): OrgHomeSelection {
        val prefs = context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
        val mode = runCatching {
            OrgHomeMode.valueOf(prefs.getString(KEY_MODE, OrgHomeMode.SHARED.name)!!)
        }.getOrDefault(OrgHomeMode.SHARED)
        val uri = prefs.getString(KEY_CUSTOM_URI, null)?.let(Uri::parse)
        return OrgHomeSelection(mode, uri)
    }

    fun dailySpec(context: Context): OrgDailySpec? {
        val prefs = context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
        val pathTemplate = prefs.getString(KEY_DAILY_PATH_TEMPLATE, null)
            ?.trim()
            ?.takeIf(String::isNotEmpty)
            ?: return null
        val datePattern = prefs.getString(KEY_DAILY_DATE_PATTERN, null)
            ?.trim()
            ?.takeIf(String::isNotEmpty)
            ?: return null
        val zoneId = prefs.getString(KEY_DAILY_ZONE_ID, null)
            ?.trim()
            ?.takeIf(String::isNotEmpty)
            ?: return null
        return runCatching {
            OrgDailyConfiguration(pathTemplate, datePattern, zoneId).toSpec()
        }.getOrNull()
    }

    fun configureDaily(
        context: Context,
        relativePathTemplate: String,
        datePattern: String,
        zoneId: String,
    ) {
        val configuration = OrgDailyConfiguration(
            relativePathTemplate = relativePathTemplate.trim(),
            datePattern = datePattern.trim(),
            zoneId = zoneId.trim(),
        )
        configuration.toSpec()
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_DAILY_PATH_TEMPLATE, configuration.relativePathTemplate)
            .putString(KEY_DAILY_DATE_PATTERN, configuration.datePattern)
            .putString(KEY_DAILY_ZONE_ID, configuration.zoneId)
            .apply()
    }

    fun clearDaily(context: Context) {
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .remove(KEY_DAILY_PATH_TEMPLATE)
            .remove(KEY_DAILY_DATE_PATTERN)
            .remove(KEY_DAILY_ZONE_ID)
            .apply()
    }

    fun useShared(context: Context) {
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_MODE, OrgHomeMode.SHARED.name)
            .apply()
    }

    fun useCustomSaf(context: Context, uri: Uri) {
        val flags = android.content.Intent.FLAG_GRANT_READ_URI_PERMISSION or
            android.content.Intent.FLAG_GRANT_WRITE_URI_PERMISSION
        context.contentResolver.takePersistableUriPermission(uri, flags)
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_MODE, OrgHomeMode.CUSTOM_SAF.name)
            .putString(KEY_CUSTOM_URI, uri.toString())
            .apply()
    }

    fun open(
        context: Context,
        fallbackTodoStates: List<String> = OrgParser.defaultTodoStates,
    ): OrgRepository? {
        val home = selection(context)
        return when (home.mode) {
            OrgHomeMode.SHARED -> if (sharedAvailable(context)) {
                SharedOrgRepository(context, fallbackTodoStates)
            } else {
                null
            }
            OrgHomeMode.CUSTOM_SAF -> home.customTreeUri?.let {
                OrgTreeRepository(context, it, fallbackTodoStates)
            }
        }
    }

    fun sharedAvailable(context: Context): Boolean =
        context.packageManager.resolveContentProvider(SharedOrgHomeContract.AUTHORITY, 0) != null
}

class SharedOrgRepository(
    private val context: Context,
    private val fallbackTodoStates: List<String> = OrgParser.defaultTodoStates,
) : OrgRepository {
    private val resolver = context.contentResolver

    override fun listOrgFiles(): List<OrgFileRef> {
        val result = mutableListOf<OrgFileRef>()
        resolver.query(
            SharedOrgHomeContract.filesUri,
            arrayOf(
                SharedOrgHomeContract.COLUMN_PATH,
                SharedOrgHomeContract.COLUMN_NAME,
                SharedOrgHomeContract.COLUMN_URI,
            ),
            null,
            null,
            null,
        )?.use { cursor ->
            val pathIndex = cursor.getColumnIndexOrThrow(SharedOrgHomeContract.COLUMN_PATH)
            val nameIndex = cursor.getColumnIndexOrThrow(SharedOrgHomeContract.COLUMN_NAME)
            val uriIndex = cursor.getColumnIndexOrThrow(SharedOrgHomeContract.COLUMN_URI)
            while (cursor.moveToNext()) {
                result += OrgFileRef(
                    name = cursor.getString(nameIndex),
                    relativePath = cursor.getString(pathIndex),
                    uri = Uri.parse(cursor.getString(uriIndex)),
                )
            }
        }
        return result.sortedBy { it.relativePath.lowercase() }
    }

    override fun read(file: OrgFileRef): String =
        resolver.openInputStream(file.uri)?.bufferedReader()?.use { it.readText() }
            ?: error("Unable to read ${file.relativePath}")

    override fun write(file: OrgFileRef, text: String) {
        writeText(file.relativePath, text)
    }

    override fun writeRelative(relativePath: String, text: String): OrgFileRef {
        writeText(relativePath, text)
        return OrgFileRef(
            name = relativePath.substringAfterLast('/'),
            relativePath = relativePath,
            uri = SharedOrgHomeContract.fileUri(relativePath),
        )
    }

    override fun appendAgendaCapture(text: String, relativePath: String): OrgFileRef {
        val result = resolver.call(
            SharedOrgHomeContract.filesUri,
            SharedOrgHomeContract.METHOD_APPEND_ORG,
            relativePath,
            Bundle().apply { putString(SharedOrgHomeContract.EXTRA_TEXT, text) },
        ) ?: error("Org Sync provider did not return an append result")
        val uri = result.getString(SharedOrgHomeContract.EXTRA_URI)?.let(Uri::parse)
            ?: error("Org Sync provider did not return a file URI")
        return OrgFileRef(relativePath.substringAfterLast('/'), relativePath, uri)
    }

    override fun allTasks(): List<OrgTask> = listOrgFiles().flatMap { file ->
        OrgParser.parse(read(file), file.relativePath, fallbackTodoStates).tasks
    }

    override fun cycleTodo(task: OrgTask): OrgTask {
        val file = listOrgFiles().firstOrNull { it.relativePath == task.path }
            ?: error("Missing task file ${task.path}")
        val source = read(file)
        val mutation = OrgParser.cycleTodoState(source, task, fallbackTodoStates)
        writeText(file.relativePath, mutation.source)
        return task.copy(state = mutation.state)
    }

    override fun tangle(file: OrgFileRef): List<OrgFileRef> {
        val result = OrgTangler.tangle(read(file), file.relativePath)
        return result.outputs.map { output ->
            writeText(output.path, output.content)
            OrgFileRef(
                name = output.path.substringAfterLast('/'),
                relativePath = output.path,
                uri = SharedOrgHomeContract.fileUri(output.path),
            )
        }
    }

    private fun writeText(relativePath: String, text: String) {
        resolver.call(
            SharedOrgHomeContract.filesUri,
            SharedOrgHomeContract.METHOD_WRITE_TEXT,
            relativePath,
            Bundle().apply { putString(SharedOrgHomeContract.EXTRA_TEXT, text) },
        ) ?: error("Unable to write $relativePath through Org Sync")
    }
}
