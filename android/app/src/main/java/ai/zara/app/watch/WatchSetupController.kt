package ai.zara.app.watch

import android.content.Context
import java.io.File
import java.io.IOException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class WatchSetupController(
    context: Context,
) {
    private val appContext = context.applicationContext
    private val executor: ExecutorService = Executors.newSingleThreadExecutor()
    private val bluetoothScanner = BluetoothWatchScanner(appContext)
    private val nodeScanner = WearNodeScanner(appContext)
    private val debugDiscovery = WatchDebugDiscovery(appContext)
    private val apkRepository = WearApkRepository(appContext)

    @Volatile
    private var current = WatchSetupState(status = WatchInstallPolicy.transportNotice)

    @Volatile
    private var observer: ((WatchSetupState) -> Unit)? = null

    fun state(): WatchSetupState = current

    fun setObserver(value: ((WatchSetupState) -> Unit)?) {
        observer = value
        value?.invoke(current)
    }

    fun setHost(value: String) = update { it.copy(host = value.trim()) }

    fun setPairPort(value: String) = update { it.copy(pairPort = value.filter(Char::isDigit)) }

    fun setConnectPort(value: String) = update { it.copy(connectPort = value.filter(Char::isDigit)) }

    fun setPairCode(value: String) = update { it.copy(pairCode = value.filter(Char::isDigit).take(6)) }

    fun useEndpoint(endpoint: WatchDebugEndpoint) {
        update { state ->
            if (endpoint.pairing) {
                state.copy(host = endpoint.host, pairPort = endpoint.port.toString())
            } else {
                state.copy(host = endpoint.host, connectPort = endpoint.port.toString())
            }
        }
    }

    fun scan() {
        update {
            it.copy(
                phase = WatchSetupPhase.SCANNING,
                watches = emptyList(),
                debugEndpoints = emptyList(),
                status = "Scanning Bluetooth-paired watches, Wear Data Layer, and wireless ADB endpoints…",
            )
        }

        bluetoothScanner.scan()
            .onSuccess { watches -> update { it.copy(watches = mergeWatches(it.watches, watches)) } }
            .onFailure { error ->
                update { it.copy(status = "Bluetooth scan unavailable: ${message(error)}") }
            }

        nodeScanner.scan { result ->
            result.onSuccess { watches ->
                update { it.copy(watches = mergeWatches(it.watches, watches)) }
            }.onFailure { error ->
                update { it.copy(status = "Wear discovery unavailable: ${message(error)}") }
            }
        }

        debugDiscovery.scan(
            onUpdate = { endpoints -> update { it.copy(debugEndpoints = endpoints) } },
            onDone = {
                update {
                    it.copy(
                        phase = WatchSetupPhase.IDLE,
                        status = when {
                            it.debugEndpoints.isNotEmpty() ->
                                "Watch endpoints found. Pair once, then connect using the separate connection port."
                            it.watches.isNotEmpty() ->
                                "Watch detected over Bluetooth/Data Layer. Enable watch Wireless debugging to sideload Zara Wear."
                            else ->
                                "No watch endpoint found. Enable watch Wireless debugging, then scan again."
                        },
                    )
                }
            },
        )
    }

    fun pair() {
        val input = WatchInstallInput.parsePairing(current.host, current.pairPort, current.pairCode)
        if (input == null) {
            fail("Enter the watch IP, pairing port, and 6-digit pairing code.")
            return
        }
        update { it.copy(phase = WatchSetupPhase.PAIRING, status = "Pairing Zara with ${input.host}:${input.port}…") }
        executor.execute {
            runCatching {
                WatchAdbConnection.get(appContext).pair(input.host, input.port, input.code)
            }.onSuccess {
                update {
                    it.copy(
                        phase = WatchSetupPhase.PAIRED,
                        pairCode = "",
                        status = "Paired. Return to the watch Wireless debugging screen and use its connection port.",
                    )
                }
            }.onFailure { error -> fail(message(error)) }
        }
    }

    fun connect() {
        val input = WatchInstallInput.parseConnection(current.host, current.connectPort)
        if (input == null) {
            fail("Enter the watch IP and connection port from the main Wireless debugging screen.")
            return
        }
        update { it.copy(phase = WatchSetupPhase.CONNECTING, status = "Connecting to ${input.host}:${input.port}…") }
        executor.execute {
            runCatching {
                val manager = WatchAdbConnection.get(appContext)
                if (manager.isConnected) manager.disconnect()
                if (!manager.connect(input.host, input.port)) throw IOException("The watch refused the ADB connection")
                val model = WatchAdbTransfer.shell(manager, "getprop ro.product.model").trim()
                val release = WatchAdbTransfer.shell(manager, "getprop ro.build.version.release").trim()
                listOf(model, release.takeIf { it.isNotBlank() }?.let { "Android $it" })
                    .filterNotNull()
                    .filter(String::isNotBlank)
                    .joinToString(" · ")
                    .ifBlank { "${input.host}:${input.port}" }
            }.onSuccess { label ->
                update {
                    it.copy(
                        phase = WatchSetupPhase.CONNECTED,
                        connectedDevice = label,
                        status = "Connected to $label",
                    )
                }
            }.onFailure { error -> fail(message(error)) }
        }
    }

    fun installLatest() {
        val manager = WatchAdbConnection.get(appContext)
        if (!manager.isConnected) {
            fail("Connect to the watch before installing Zara.")
            return
        }
        executor.execute {
            runCatching {
                update {
                    it.copy(
                        phase = WatchSetupPhase.DOWNLOADING,
                        progress = 0f,
                        status = "Downloading verified Zara Wear + Agenda APKs…",
                    )
                }
                val wearApk = apkRepository.downloadWear { written, total ->
                    updateDownloadProgress(written, total, 0f, 0.20f)
                }
                val agendaApk = apkRepository.downloadAgenda { written, total ->
                    updateDownloadProgress(written, total, 0.20f, 0.35f)
                }

                installPackage(manager, wearApk, "zara-wear", 0.35f, 0.68f)
                try {
                    installPackage(manager, agendaApk, "zara-agenda", 0.68f, 0.96f)
                } catch (error: Throwable) {
                    runCatching { uninstallIfPresent(manager, AGENDA_PACKAGE) }
                    throw error
                }

                requirePackage(manager, WEAR_PACKAGE)
                requirePackage(manager, AGENDA_PACKAGE)
                uninstallIfPresent(manager, LEGACY_WEAR_PACKAGE)
                update { it.copy(progress = 0.99f, status = "Verified Zara Wear + Agenda on watch…") }
            }.onSuccess {
                update {
                    it.copy(
                        phase = WatchSetupPhase.INSTALLED,
                        progress = 1f,
                        status = "Zara Wear + Agenda installed. Pick Zara Agenda from the watch-face chooser.",
                    )
                }
                nodeScanner.scan { result ->
                    result.onSuccess { watches ->
                        update { it.copy(watches = mergeWatches(it.watches, watches)) }
                    }
                }
            }.onFailure { error -> fail(message(error)) }
        }
    }

    fun uninstallZara() {
        val manager = WatchAdbConnection.get(appContext)
        if (!manager.isConnected) {
            fail("Connect to the watch before uninstalling Zara.")
            return
        }
        update {
            it.copy(
                phase = WatchSetupPhase.REMOVING,
                progress = null,
                status = "Removing Zara Agenda and Zara Wear…",
            )
        }
        executor.execute {
            runCatching {
                uninstallIfPresent(manager, AGENDA_PACKAGE)
                uninstallIfPresent(manager, WEAR_PACKAGE)
                uninstallIfPresent(manager, LEGACY_WEAR_PACKAGE)
            }.onSuccess {
                update {
                    it.copy(
                        phase = WatchSetupPhase.CONNECTED,
                        status = "Zara Wear + Agenda removed from the watch.",
                    )
                }
            }.onFailure { error -> fail(message(error)) }
        }
    }

    fun disconnect() {
        executor.execute {
            runCatching { WatchAdbConnection.get(appContext).disconnect() }
            update {
                it.copy(
                    phase = WatchSetupPhase.IDLE,
                    connectedDevice = null,
                    progress = null,
                    status = WatchInstallPolicy.transportNotice,
                )
            }
        }
    }

    private fun installPackage(
        manager: AdbConnectionManager,
        apk: File,
        slug: String,
        startProgress: Float,
        endProgress: Float,
    ) {
        val remotePath = "/data/local/tmp/$slug-${System.currentTimeMillis()}.apk"
        update { it.copy(phase = WatchSetupPhase.INSTALLING, status = "Sending $slug to the watch…") }
        try {
            WatchAdbTransfer.push(manager, apk, remotePath) { written, total ->
                if (total > 0) {
                    val fraction = (written.toFloat() / total).coerceIn(0f, 1f)
                    val progress = startProgress + (endProgress - startProgress) * fraction * 0.85f
                    update { it.copy(progress = progress) }
                }
            }
            update { it.copy(status = "Installing $slug on the watch…") }
            val output = WatchAdbTransfer.shell(manager, "pm install -r -t -d $remotePath").trim()
            if (!output.contains("Success", ignoreCase = true)) {
                throw IOException(output.ifBlank { "Watch package manager returned no result for $slug" })
            }
            update { it.copy(progress = endProgress) }
        } finally {
            runCatching { WatchAdbTransfer.shell(manager, "rm -f $remotePath") }
        }
    }

    private fun updateDownloadProgress(written: Long, total: Long, start: Float, end: Float) {
        if (total <= 0) return
        val fraction = (written.toFloat() / total).coerceIn(0f, 1f)
        update { it.copy(progress = start + (end - start) * fraction) }
    }

    private fun requirePackage(manager: AdbConnectionManager, packageName: String) {
        val path = WatchAdbTransfer.shell(manager, "pm path $packageName").trim()
        if (!path.startsWith("package:")) {
            throw IOException("$packageName was not present after installation")
        }
    }

    private fun uninstallIfPresent(manager: AdbConnectionManager, packageName: String) {
        val path = WatchAdbTransfer.shell(manager, "pm path $packageName").trim()
        if (!path.startsWith("package:")) return
        val output = WatchAdbTransfer.shell(manager, "pm uninstall $packageName").trim()
        if (!output.contains("Success", ignoreCase = true)) {
            throw IOException(output.ifBlank { "Could not uninstall $packageName" })
        }
    }

    private fun mergeWatches(
        existing: List<NearbyWatch>,
        incoming: List<NearbyWatch>,
    ): List<NearbyWatch> =
        (existing + incoming)
            .groupBy { it.name.trim().lowercase() }
            .values
            .map { group ->
                val preferred = group.firstOrNull { it.zaraInstalled }
                    ?: group.firstOrNull { it.transport.startsWith("Wear Data Layer") }
                    ?: group.first()
                preferred.copy(
                    nearby = group.any { it.nearby },
                    zaraInstalled = group.any { it.zaraInstalled },
                    transport = group.map { it.transport }.distinct().joinToString(" + "),
                )
            }
            .sortedWith(compareByDescending<NearbyWatch> { it.nearby }.thenBy { it.name.lowercase() })

    private fun fail(reason: String) {
        update { it.copy(phase = WatchSetupPhase.ERROR, progress = null, status = reason) }
    }

    private fun update(transform: (WatchSetupState) -> WatchSetupState) {
        synchronized(this) {
            current = transform(current)
        }
        observer?.invoke(current)
    }

    private fun message(error: Throwable): String =
        generateSequence(error) { it.cause }
            .mapNotNull { it.message?.takeIf(String::isNotBlank) }
            .firstOrNull()
            ?: error.javaClass.simpleName

    companion object {
        private const val WEAR_PACKAGE = "ai.zara.app"
        private const val AGENDA_PACKAGE = "ai.zara.agenda"
        private const val LEGACY_WEAR_PACKAGE = "ai.zara.wear"
    }
}
