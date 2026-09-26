package ai.zara.app.phone

import ai.zara.app.device.DeviceActionArguments
import ai.zara.app.device.DeviceActionErrorCode
import ai.zara.app.device.DeviceActionResult
import ai.zara.app.device.DeviceCapabilityAdapter
import ai.zara.app.runtime.DeviceCapability
import android.Manifest
import android.content.Context
import android.content.pm.PackageManager
import android.telephony.SmsManager

interface SmsGateway {
    fun isAvailable(): Boolean
    fun send(destination: String, text: String)
}

class AndroidSmsGateway(context: Context) : SmsGateway {
    private val appContext = context.applicationContext
    private val smsManager: SmsManager by lazy {
        appContext.getSystemService(SmsManager::class.java)
            ?: throw IllegalStateException("SMS service is unavailable")
    }

    override fun isAvailable(): Boolean =
        appContext.packageManager.hasSystemFeature(PackageManager.FEATURE_TELEPHONY_MESSAGING) &&
            appContext.checkSelfPermission(Manifest.permission.SEND_SMS) == PackageManager.PERMISSION_GRANTED

    override fun send(destination: String, text: String) {
        check(isAvailable()) { "SMS permission or telephony messaging is unavailable" }
        val parts = smsManager.divideMessage(text)
        if (parts.size <= 1) {
            smsManager.sendTextMessage(destination, null, text, null, null)
        } else {
            smsManager.sendMultipartTextMessage(destination, null, ArrayList(parts), null, null)
        }
    }
}

class SmsSendAdapter(
    private val gateway: SmsGateway,
) : DeviceCapabilityAdapter {
    override val capability: DeviceCapability = DeviceCapability.SmsSend

    override fun isAvailable(): Boolean = runCatching(gateway::isAvailable).getOrDefault(false)

    override fun execute(arguments: DeviceActionArguments): DeviceActionResult {
        val sms = arguments as? DeviceActionArguments.SendSms
            ?: return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        if (!isAvailable()) return DeviceActionResult.Error(DeviceActionErrorCode.Unavailable)
        return try {
            gateway.send(sms.to, sms.text)
            DeviceActionResult.Completed
        } catch (_: SecurityException) {
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (_: IllegalArgumentException) {
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        } catch (_: IllegalStateException) {
            DeviceActionResult.Error(DeviceActionErrorCode.Unavailable)
        } catch (_: Throwable) {
            DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
    }
}
