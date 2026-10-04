package ai.zara.app.phone

import ai.zara.app.device.DeviceActionArguments
import ai.zara.app.device.DeviceActionErrorCode
import ai.zara.app.device.DeviceActionResult
import ai.zara.app.runtime.DeviceCapability
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class SmsSendAdapterTest {
    @Test
    fun availableGatewaySendsBoundedSms() {
        val gateway = FakeSmsGateway(available = true)
        val adapter = SmsSendAdapter(gateway)

        assertTrue(adapter.isAvailable())
        assertEquals(DeviceCapability.SmsSend, adapter.capability)
        assertEquals(
            DeviceActionResult.Completed,
            adapter.execute(DeviceActionArguments.SendSms("+15551234567", "hello")),
        )
        assertEquals(listOf("+15551234567" to "hello"), gateway.sent)
    }

    @Test
    fun unavailableOrWrongArgumentsFailClosed() {
        val unavailable = SmsSendAdapter(FakeSmsGateway(available = false))
        assertFalse(unavailable.isAvailable())

        val result = unavailable.execute(DeviceActionArguments.OpenApp("messages"))
        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
            result,
        )
    }

    private class FakeSmsGateway(
        private val available: Boolean,
    ) : SmsGateway {
        val sent = mutableListOf<Pair<String, String>>()

        override fun isAvailable(): Boolean = available

        override fun send(destination: String, text: String) {
            sent += destination to text
        }
    }
}
