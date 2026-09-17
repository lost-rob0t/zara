package ai.zara.app.widget

import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.ServerConnection
import java.io.File
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

data class WidgetRuntimeSnapshot(
    val remote: String,
    val local: String,
    val mode: String,
    val capturedAtEpochMillis: Long,
) {
    companion object {
        fun unknown() = WidgetRuntimeSnapshot("DISCONNECTED", "LOCAL UNKNOWN", "AUTO", 0)

        fun from(
            server: ServerConnection,
            local: LocalServerPhase,
            mode: RuntimeMode,
            capturedAtEpochMillis: Long,
        ) = WidgetRuntimeSnapshot(
            remote = when (server) {
                ServerConnection.Disconnected -> "DISCONNECTED"
                is ServerConnection.Connecting -> "CONNECTING"
                is ServerConnection.Connected -> "CONNECTED"
                is ServerConnection.Reconnecting -> "RECONNECTING ${server.attempt}"
                is ServerConnection.OfflineDegraded -> "OFFLINE"
            },
            local = "LOCAL ${local.name}",
            mode = mode.name.uppercase(),
            capturedAtEpochMillis = capturedAtEpochMillis.coerceAtLeast(0),
        )
    }
}

class WidgetRuntimeSnapshotStore(private val file: File) {
    fun load(): WidgetRuntimeSnapshot {
        if (!file.isFile || file.length() !in 1..MAX_BYTES.toLong()) return WidgetRuntimeSnapshot.unknown()
        return runCatching {
            val values = file.readLines()
            require(values.size == 5 && values[0] == VERSION)
            val snapshot = WidgetRuntimeSnapshot(values[1], values[2], values[3], values[4].toLong())
            require(snapshot.remote.matches(LABEL) && snapshot.local.matches(LABEL) && snapshot.mode.matches(LABEL))
            snapshot
        }.getOrElse { WidgetRuntimeSnapshot.unknown() }
    }

    fun save(snapshot: WidgetRuntimeSnapshot) {
        require(snapshot.remote.matches(LABEL) && snapshot.local.matches(LABEL) && snapshot.mode.matches(LABEL)) {
            "Widget runtime snapshot contains an invalid label"
        }
        val directory = file.absoluteFile.parentFile
            ?: throw IllegalStateException("Widget runtime snapshot path has no parent")
        check(directory.exists() || directory.mkdirs()) { "Widget runtime snapshot directory could not be created" }
        val temporary = Files.createTempFile(directory.toPath(), ".${file.name}.", ".tmp").toFile()
        try {
            temporary.writeText(
                listOf(
                    VERSION,
                    snapshot.remote,
                    snapshot.local,
                    snapshot.mode,
                    snapshot.capturedAtEpochMillis.toString(),
                ).joinToString("\n", postfix = "\n"),
            )
            try {
                Files.move(
                    temporary.toPath(),
                    file.toPath(),
                    StandardCopyOption.ATOMIC_MOVE,
                    StandardCopyOption.REPLACE_EXISTING,
                )
            } catch (_: AtomicMoveNotSupportedException) {
                Files.move(temporary.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING)
            }
        } finally {
            temporary.delete()
        }
    }

    companion object {
        private const val VERSION = "ZARA-WIDGET-RUNTIME/1"
        private const val MAX_BYTES = 8 * 1024
        private val LABEL = Regex("[A-Z0-9 _-]{1,32}")
    }
}
