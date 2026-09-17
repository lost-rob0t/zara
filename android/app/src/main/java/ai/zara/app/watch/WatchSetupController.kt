package ai.zara.app.watch

import android.content.Context
import java.io.IOException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class WatchSetupController(
    context: Context,
) {
    private val appContext = context.applicationContext
    private val executor: ExecutorService = Executors.newSingleThreadExecutor()
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
        update { it.copy(phase = WatchSetupPhase.SCANNING, status = "Scanning paired watches and wireless ADB endpoints…") }
        nodeScanner.scan { result ->
            result.onSuccess { watches ->
                update { it.copy(watches = watches) }
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
                        status = if (it.debugEndpoints.isEmpty()) {
                            "No wireless-debugging endpoint found. Open Watch Settings → Developer options → Wireless debugging, then scan again."
                        } else {
                            "Watch endpoints found. Pair once, then connect using the separate connection port."
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
                update { it.copy(phase = WatchSetupPhase.DOWNLOADING, progress = 0f, status = "Downloading verified Zara Wear APK…") }
                val apk = apkRepository.download { written, total ->
                    if (total > 0) {
                        update { it.copy(progress = (written.toFloat() / total).coerceIn(0f, 1f) * 0.35f) }
                    }
                }
                val remotePath = "/data/local/tmp/zara-wear-${System.currentTimeMillis()}.apk"
                update { it.copy(phase = WatchSetupPhase.INSTALLING, status = "Sending Zara to the watch…") }
                WatchAdbTransfer.push(manager, apk, remotePath) { written, total ->
                    if (total > 0) {
                        val transfer = (written.toFloat() / total).coerceIn(0f, 1f)
                        update { it.copy(progress = 0.35f + transfer * 0.6f) }
                    }
                }
                update { it.copy(progress = 0.97f, status = "Installing Zara on the watch…") }
                val output = WatchAdbTransfer.shell(manager, "pm install -r -t -d $remotePath").trim()
                runCatching { WatchAdbTransfer.shell(manager, "rm -f $remotePath") }
                if (!output.contains("Success", ignoreCase = true)) {
                    throw IOException(output.ifBlank { "Watch package manager returned no result" })
                }

                val legacyPackage = WatchAdbTransfer.shell(manager, "pm path ai.zara.wear").trim()
                if (legacyPackage.startsWith("package:")) {
                    runCatching { WatchAdbTransfer.shell(manager, "pm uninstall ai.zara.wear") }
                }
            }.onSuccess {
                update {
                    it.copy(
                        phase = WatchSetupPhase.INSTALLED,
                        progress = 1f,
                        status = "Zara Wear installed. Bluetooth/Data Layer can take over normal phone↔watch communication.",
                    )
                }
                nodeScanner.scan { result ->
                    result.onSuccess { watches -> update { it.copy(watches = watches) } }
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
}
