package ai.zara.app.prolog

import java.security.MessageDigest

enum class AndroidMemorySensitivity {
    PUBLIC,
    PRIVATE,
    SECRET,
}

data class AndroidAppMemory(
    val packageName: String,
    val activityName: String,
    val label: String,
    val profile: String,
)

data class AndroidActionMemory(
    val id: String,
    val kind: String,
    val backend: String,
    val target: String,
    val source: String,
)

data class AndroidHookMemory(
    val id: String,
    val event: String,
    val automation: String,
    val source: String,
)

data class AndroidMemoryEvent(
    val kind: String,
    val subject: String,
    val detail: String,
    val provenance: String,
    val sensitivity: AndroidMemorySensitivity,
)

class AndroidKnowledgeBase(
    private val workspace: PrologWorkspace,
    private val clockMillis: () -> Long = System::currentTimeMillis,
    private val shardByteLimit: Int = DEFAULT_SHARD_BYTES,
) {
    init {
        require(shardByteLimit >= MIN_SHARD_BYTES) { "Android memory shard limit is too small" }
    }

    @Synchronized
    fun sources(): List<PrologSource> =
        workspace.listSources().filter { SOURCE_NAME.matches(it.name) }

    @Synchronized
    fun rememberApp(app: AndroidAppMemory) {
        appendFactOnce(
            "android_app(${q(app.packageName)},${q(app.activityName)},${q(app.label)},${q(app.profile)})."
        )
    }

    @Synchronized
    fun rememberAction(action: AndroidActionMemory) {
        appendFactOnce(
            "android_action(${q(action.id)},${q(action.kind)},${q(action.backend)},${q(action.target)},${q(action.source)})."
        )
    }

    @Synchronized
    fun rememberHook(hook: AndroidHookMemory) {
        appendFactOnce(
            "android_hook(${q(hook.id)},${q(hook.event)},${q(hook.automation)},${q(hook.source)})."
        )
    }

    @Synchronized
    fun rememberObservation(
        actionId: String,
        status: String,
        backend: String,
        detail: String,
    ) {
        appendRecord(
            "android_observation(${q(actionId)},${q(status)},${q(backend)},${q(detail.take(MAX_DETAIL_CHARS))},${clockMillis()})."
        )
    }

    @Synchronized
    fun rememberEvent(event: AndroidMemoryEvent) {
        val storedDetail = when (event.sensitivity) {
            AndroidMemorySensitivity.SECRET -> "redacted:${sha256(event.detail)}"
            AndroidMemorySensitivity.PUBLIC,
            AndroidMemorySensitivity.PRIVATE -> event.detail.take(MAX_DETAIL_CHARS)
        }
        appendRecord(
            "android_event(${q(event.kind)},${q(event.subject)},${q(storedDetail)},${q(event.provenance)},${q(event.sensitivity.name.lowercase())},${clockMillis()})."
        )
    }

    @Synchronized
    fun modelContext(limit: Int = DEFAULT_CONTEXT_RECORDS): String {
        require(limit >= 0) { "Android memory context limit must be non-negative" }
        if (limit == 0) return ""
        val records = sources().flatMap { source ->
            source.text.lineSequence()
                .map { line -> line.trim() }
                .filter { line -> line.isNotEmpty() }
                .toList()
        }
        return records.takeLast(limit).asReversed().joinToString("\n")
    }

    private fun appendFactOnce(record: String) {
        if (sources().any { source -> source.text.lineSequence().any { it.trim() == record } }) return
        appendRecord(record)
    }

    private fun appendRecord(record: String) {
        val line = "$record\n"
        val lineBytes = line.encodeToByteArray().size
        require(lineBytes <= shardByteLimit) { "Android memory record exceeds shard limit" }

        val existing = sources()
        val latest = existing.maxByOrNull { shardIndex(it.name) }
        if (latest != null) {
            val updated = latest.text + line
            if (updated.encodeToByteArray().size <= shardByteLimit) {
                workspace.saveSource(latest.name, updated)
                return
            }
        }

        val nextIndex = (latest?.let { shardIndex(it.name) } ?: -1) + 1
        workspace.saveSource(shardName(nextIndex), line)
    }

    private fun shardIndex(name: String): Int =
        SOURCE_NAME.matchEntire(name)?.groupValues?.get(1)?.toInt() ?: -1

    private fun shardName(index: Int): String = "android-memory-${index.toString().padStart(4, '0')}.pl"

    private fun q(value: String): String = buildString {
        append('"')
        value.forEach { character ->
            when (character) {
                '\\' -> append("\\\\")
                '"' -> append("\\\"")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> if (character.code >= 0x20 && character.code != 0x7f) append(character)
            }
        }
        append('"')
    }

    private fun sha256(value: String): String =
        MessageDigest.getInstance("SHA-256")
            .digest(value.encodeToByteArray())
            .joinToString("") { byte -> (byte.toInt() and 0xff).toString(16).padStart(2, '0') }

    companion object {
        private val SOURCE_NAME = Regex("android-memory-([0-9]{4,})\\.pl")
        private const val DEFAULT_SHARD_BYTES = 64 * 1024
        private const val MIN_SHARD_BYTES = 256
        private const val DEFAULT_CONTEXT_RECORDS = 64
        private const val MAX_DETAIL_CHARS = 512
    }
}
