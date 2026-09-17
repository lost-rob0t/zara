package ai.zara.app.watch

enum class WatchSetupPhase {
    IDLE,
    SCANNING,
    PAIRING,
    PAIRED,
    CONNECTING,
    CONNECTED,
    DOWNLOADING,
    INSTALLING,
    INSTALLED,
    ERROR,
}

data class NearbyWatch(
    val id: String,
    val name: String,
    val nearby: Boolean,
    val zaraInstalled: Boolean,
    val transport: String,
)

data class WatchDebugEndpoint(
    val host: String,
    val port: Int,
    val pairing: Boolean,
    val serviceName: String,
)

data class WatchSetupState(
    val phase: WatchSetupPhase = WatchSetupPhase.IDLE,
    val status: String = "Ready",
    val watches: List<NearbyWatch> = emptyList(),
    val debugEndpoints: List<WatchDebugEndpoint> = emptyList(),
    val host: String = "",
    val pairPort: String = "",
    val connectPort: String = "",
    val pairCode: String = "",
    val connectedDevice: String? = null,
    val progress: Float? = null,
)
