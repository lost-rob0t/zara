package ai.zara.app.automation

import ai.zara.app.device.DeviceActionErrorCode
import ai.zara.app.device.DeviceActionResult
import ai.zara.app.watch.WatchAdbConnection
import ai.zara.app.watch.WatchAdbTransfer
import android.content.Context

enum class AdbAutomationKey(val keyCode: Int) {
    Back(4),
    Home(3),
    Enter(66),
    Recents(187),
    Tab(61),
    Escape(111),
    Delete(67),
    Up(19),
    Down(20),
    Left(21),
    Right(22);

    companion object {
        fun fromAtom(value: String): AdbAutomationKey = when (value) {
            "back" -> Back
            "home" -> Home
            "enter" -> Enter
            "recents" -> Recents
            "tab" -> Tab
            "escape" -> Escape
            "delete" -> Delete
            "up" -> Up
            "down" -> Down
            "left" -> Left
            "right" -> Right
            else -> throw IllegalArgumentException("unsupported ADB automation key")
        }
    }
}

interface AdbAutomationPort {
    fun isAvailable(): Boolean
    fun tap(x: Int, y: Int): DeviceActionResult
    fun swipe(x1: Int, y1: Int, x2: Int, y2: Int, durationMs: Int): DeviceActionResult
    fun typeText(text: String): DeviceActionResult
    fun key(key: AdbAutomationKey): DeviceActionResult
    fun wait(durationMs: Int): DeviceActionResult
    fun screenshotPng(): Result<ByteArray>
}

class AndroidAdbAutomationAdapter(
    context: Context,
) : AdbAutomationPort {
    private val appContext = context.applicationContext

    override fun isAvailable(): Boolean = manager().isConnected

    override fun tap(x: Int, y: Int): DeviceActionResult {
        requireCoordinate(x)
        requireCoordinate(y)
        return shell("input tap $x $y")
    }

    override fun swipe(
        x1: Int,
        y1: Int,
        x2: Int,
        y2: Int,
        durationMs: Int,
    ): DeviceActionResult {
        requireCoordinate(x1)
        requireCoordinate(y1)
        requireCoordinate(x2)
        requireCoordinate(y2)
        require(durationMs in 1..MAX_DURATION_MS) { "ADB swipe duration is out of range" }
        return shell("input swipe $x1 $y1 $x2 $y2 $durationMs")
    }

    override fun typeText(text: String): DeviceActionResult {
        val encoded = encodeInputText(text)
        return shell("input text $encoded")
    }

    override fun key(key: AdbAutomationKey): DeviceActionResult =
        shell("input keyevent ${key.keyCode}")

    override fun wait(durationMs: Int): DeviceActionResult {
        require(durationMs in 0..MAX_DURATION_MS) { "ADB wait duration is out of range" }
        Thread.sleep(durationMs.toLong())
        return DeviceActionResult.Completed
    }

    override fun screenshotPng(): Result<ByteArray> {
        val connection = manager()
        if (!connection.isConnected) {
            return Result.failure(IllegalStateException("ADB target is not connected"))
        }
        return runCatching {
            val png = WatchAdbTransfer.exec(
                connection,
                "screencap -p",
                MAX_SCREENSHOT_BYTES,
            )
            require(
                png.size >= PNG_SIGNATURE.size &&
                    png.copyOfRange(0, PNG_SIGNATURE.size).contentEquals(PNG_SIGNATURE)
            ) {
                "ADB target did not return a PNG screenshot"
            }
            png
        }
    }

    private fun shell(command: String): DeviceActionResult {
        val connection = manager()
        if (!connection.isConnected) {
            return DeviceActionResult.Error(
                DeviceActionErrorCode.Unavailable,
                "ADB target is not connected",
            )
        }
        return try {
            WatchAdbTransfer.shell(connection, command)
            DeviceActionResult.Completed
        } catch (_: SecurityException) {
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (error: IllegalArgumentException) {
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments, error.message)
        } catch (_: Throwable) {
            DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
    }

    private fun manager() = WatchAdbConnection.get(appContext)

    private fun requireCoordinate(value: Int) {
        require(value in 0..MAX_COORDINATE) { "ADB coordinate is out of range" }
    }

    private fun encodeInputText(value: String): String {
        require(value.encodeToByteArray().size in 1..MAX_TEXT_BYTES) {
            "ADB text must contain 1 to $MAX_TEXT_BYTES bytes"
        }
        require(SAFE_TEXT.matches(value)) {
            "ADB text contains unsupported shell-sensitive characters"
        }
        return value.replace(" ", "%s")
    }

    private companion object {
        const val MAX_COORDINATE = 16_384
        const val MAX_DURATION_MS = 5_000
        const val MAX_TEXT_BYTES = 512
        const val MAX_SCREENSHOT_BYTES = 16 * 1024 * 1024
        val SAFE_TEXT = Regex("[A-Za-z0-9 _.,:@/+\\-]{1,512}")
        val PNG_SIGNATURE = byteArrayOf(
            0x89.toByte(),
            0x50,
            0x4E,
            0x47,
            0x0D,
            0x0A,
            0x1A,
            0x0A,
        )
    }
}
