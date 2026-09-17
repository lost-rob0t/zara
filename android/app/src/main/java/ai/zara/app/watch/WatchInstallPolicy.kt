package ai.zara.app.watch

enum class WatchInstallTransport {
    WIFI_ADB,
}

data class WatchPairingInput(
    val host: String,
    val port: Int,
    val code: String,
)

data class WatchConnectionInput(
    val host: String,
    val port: Int,
)

object WatchInstallPolicy {
    const val bluetoothSideloadSupported = false
    val installTransport = WatchInstallTransport.WIFI_ADB
    const val transportNotice =
        "Wear OS 3+ removed Bluetooth ADB. Zara can detect and configure a paired watch over Bluetooth/Data Layer, but the one-time APK sideload uses Wi-Fi ADB."
}

object WatchInstallInput {
    fun parsePairing(host: String, port: String, code: String): WatchPairingInput? {
        val normalizedHost = host.trim()
        val normalizedPort = port.trim().toIntOrNull()
        val normalizedCode = code.filter(Char::isDigit)
        if (normalizedHost.isBlank()) return null
        if (normalizedPort == null || normalizedPort !in 1..65535) return null
        if (normalizedCode.length != 6) return null
        return WatchPairingInput(normalizedHost, normalizedPort, normalizedCode)
    }

    fun parseConnection(host: String, port: String): WatchConnectionInput? {
        val normalizedHost = host.trim()
        val normalizedPort = port.trim().toIntOrNull()
        if (normalizedHost.isBlank()) return null
        if (normalizedPort == null || normalizedPort !in 1..65535) return null
        return WatchConnectionInput(normalizedHost, normalizedPort)
    }
}
