package ai.zara.app.voice

import android.content.Context
import android.media.AudioDeviceCallback
import android.media.AudioDeviceInfo
import android.media.AudioManager
import android.os.Handler
import android.os.Looper

class AndroidAudioRoutePlatform(context: Context) : AudioRoutePlatform {
    private val audioManager = context.applicationContext
        .getSystemService(Context.AUDIO_SERVICE) as AudioManager
    private val handler = Handler(Looper.getMainLooper())
    private val lock = Any()
    private var callback: AudioDeviceCallback? = null
    private var listener: ((AudioRouteSnapshot) -> Unit)? = null

    override fun snapshot(): AudioRouteSnapshot = AudioRouteSnapshot(
        audioManager.getDevices(AudioManager.GET_DEVICES_OUTPUTS)
            .mapTo(linkedSetOf()) { classify(it.type) }
    )

    override fun start(onChanged: (AudioRouteSnapshot) -> Unit) {
        val deviceCallback = synchronized(lock) {
            check(callback == null) { "Android audio route monitor already active" }
            listener = onChanged
            object : AudioDeviceCallback() {
                override fun onAudioDevicesAdded(addedDevices: Array<out AudioDeviceInfo>) = publish()
                override fun onAudioDevicesRemoved(removedDevices: Array<out AudioDeviceInfo>) = publish()
            }.also { callback = it }
        }
        audioManager.registerAudioDeviceCallback(deviceCallback, handler)
    }

    override fun stop() {
        val deviceCallback = synchronized(lock) {
            val active = callback ?: return
            callback = null
            listener = null
            active
        }
        audioManager.unregisterAudioDeviceCallback(deviceCallback)
    }

    private fun publish() {
        val target = synchronized(lock) { listener } ?: return
        target(snapshot())
    }

    internal companion object {
        fun classify(type: Int): AudioRouteKind = when (type) {
            AudioDeviceInfo.TYPE_BUILTIN_EARPIECE,
            AudioDeviceInfo.TYPE_BUILTIN_SPEAKER,
            -> AudioRouteKind.BuiltIn

            AudioDeviceInfo.TYPE_WIRED_HEADSET,
            AudioDeviceInfo.TYPE_WIRED_HEADPHONES,
            AudioDeviceInfo.TYPE_LINE_ANALOG,
            AudioDeviceInfo.TYPE_LINE_DIGITAL,
            -> AudioRouteKind.Wired

            AudioDeviceInfo.TYPE_BLUETOOTH_SCO,
            AudioDeviceInfo.TYPE_BLUETOOTH_A2DP,
            AudioDeviceInfo.TYPE_BLE_HEADSET,
            AudioDeviceInfo.TYPE_BLE_SPEAKER,
            AudioDeviceInfo.TYPE_BLE_BROADCAST,
            -> AudioRouteKind.Bluetooth

            AudioDeviceInfo.TYPE_USB_DEVICE,
            AudioDeviceInfo.TYPE_USB_ACCESSORY,
            AudioDeviceInfo.TYPE_USB_HEADSET,
            -> AudioRouteKind.Usb

            else -> AudioRouteKind.Other
        }
    }
}
