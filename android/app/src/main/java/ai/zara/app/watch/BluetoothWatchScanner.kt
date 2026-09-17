package ai.zara.app.watch

import android.Manifest
import android.bluetooth.BluetoothClass
import android.bluetooth.BluetoothManager
import android.content.Context
import android.content.pm.PackageManager
import android.os.Build

class BluetoothWatchScanner(
    context: Context,
) {
    private val appContext = context.applicationContext
    private val manager = appContext.getSystemService(Context.BLUETOOTH_SERVICE) as BluetoothManager

    fun scan(): Result<List<NearbyWatch>> = runCatching {
        if (
            Build.VERSION.SDK_INT >= Build.VERSION_CODES.S &&
            appContext.checkSelfPermission(Manifest.permission.BLUETOOTH_CONNECT) != PackageManager.PERMISSION_GRANTED
        ) {
            throw SecurityException("Nearby devices permission is required to list the Bluetooth-paired watch")
        }

        @Suppress("MissingPermission")
        manager.adapter?.bondedDevices.orEmpty()
            .filter(::looksLikeWatch)
            .map { device ->
                @Suppress("MissingPermission")
                NearbyWatch(
                    id = "bt:${device.address}",
                    name = device.name?.takeIf { it.isNotBlank() } ?: device.address,
                    nearby = true,
                    zaraInstalled = false,
                    transport = "Bluetooth paired",
                )
            }
            .sortedBy { it.name.lowercase() }
    }

    @Suppress("MissingPermission")
    private fun looksLikeWatch(device: android.bluetooth.BluetoothDevice): Boolean {
        val major = device.bluetoothClass?.majorDeviceClass
        if (major == BluetoothClass.Device.Major.WEARABLE) return true
        val name = device.name.orEmpty().lowercase()
        return WATCH_NAME_HINTS.any(name::contains)
    }

    companion object {
        private val WATCH_NAME_HINTS = listOf(
            "watch",
            "wear",
            "ticwatch",
            "fossil",
        )
    }
}
