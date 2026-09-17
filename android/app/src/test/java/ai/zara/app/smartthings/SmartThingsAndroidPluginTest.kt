package ai.zara.app.smartthings

import ai.zara.app.auth.CredentialCipher
import ai.zara.app.auth.SealedCredential
import ai.zara.app.runtime.LocalQueryResult
import java.io.File
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class SmartThingsAndroidPluginTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun controllerAsksPrologForEffectsBeforeCallingGateway() {
        val queries = mutableListOf<String>()
        val gateway = RecordingGateway()
        val actor = SmartThingsPluginActor(gateway)
        val store = credentialStore()
        val controller = SmartThingsAndroidPlugin(
            queryProlog = { query ->
                queries += query
                CompletableFuture.completedFuture(
                    LocalQueryResult(
                        query = query,
                        terms = listOf(effectFor(query)),
                        generation = 1,
                    ),
                )
            },
            actor = actor,
            credentials = store,
        )

        assertEquals("Desk lamp [device-1]", controller.devices().get(2, TimeUnit.SECONDS).text)
        assertTrue(controller.status("desk_lamp").get(2, TimeUnit.SECONDS).success)
        assertTrue(controller.turnOn("desk_lamp").get(2, TimeUnit.SECONDS).success)
        assertTrue(controller.turnOff("desk_lamp").get(2, TimeUnit.SECONDS).success)
        assertTrue(controller.setLevel("desk_lamp", 42).get(2, TimeUnit.SECONDS).success)

        assertEquals(
            listOf(
                "smartthings_devices(Result)",
                "smartthings_status(desk_lamp, Result)",
                "smartthings_on(desk_lamp, Result)",
                "smartthings_off(desk_lamp, Result)",
                "smartthings_set_level(desk_lamp, 42, Result)",
            ),
            queries,
        )
        assertEquals(2, gateway.commandCount)
        assertEquals(42, (gateway.lastCommand?.arguments?.single() as SmartThingsArgument.IntegerValue).value)
        controller.close()
    }

    @Test
    fun controllerRejectsUnboundedAliasesBeforeProlog() {
        var queried = false
        val actor = SmartThingsPluginActor(RecordingGateway())
        val controller = SmartThingsAndroidPlugin(
            queryProlog = {
                queried = true
                CompletableFuture.completedFuture(LocalQueryResult(it, emptyList(), 1))
            },
            actor = actor,
            credentials = credentialStore(),
        )

        val failure = runCatching {
            controller.turnOn("desk lamp").get(2, TimeUnit.SECONDS)
        }.exceptionOrNull()

        assertNotNull(failure)
        assertTrue(!queried)
        controller.close()
    }

    @Test
    fun personalTokenLifecycleStaysOutsideProlog() {
        var now = 10_000L
        val store = SmartThingsCredentialStore(
            file = File(temporary.root, "token.bin"),
            cipher = ReversingCipher(),
            nowMillis = { now },
        )
        val controller = SmartThingsAndroidPlugin(
            queryProlog = { CompletableFuture.completedFuture(LocalQueryResult(it, emptyList(), 1)) },
            actor = SmartThingsPluginActor(RecordingGateway()),
            credentials = store,
        )

        assertEquals(SmartThingsCredentialState.MISSING, controller.credentialState())
        controller.connectPersonalAccessToken("secret-token", 20_000L)
        assertEquals(SmartThingsCredentialState.READY, controller.credentialState())
        now = 20_001L
        assertEquals(SmartThingsCredentialState.EXPIRED, controller.credentialState())
        assertTrue(controller.disconnect())
        assertEquals(SmartThingsCredentialState.MISSING, controller.credentialState())
        controller.close()
    }

    private fun credentialStore(): SmartThingsCredentialStore = SmartThingsCredentialStore(
        file = File(temporary.root, "token-${System.nanoTime()}.bin"),
        cipher = ReversingCipher(),
        nowMillis = { 1_000L },
    )

    private fun effectFor(query: String): String = when {
        query == "smartthings_devices(Result)" -> "smartthings_action(list_devices)"
        query.startsWith("smartthings_status") -> "smartthings_action(status('device-1'))"
        query.startsWith("smartthings_on") ->
            "smartthings_action(command('device-1',main,switch,on,[]))"
        query.startsWith("smartthings_off") ->
            "smartthings_action(command('device-1',main,switch,off,[]))"
        query.startsWith("smartthings_set_level") ->
            "smartthings_action(command('device-1',main,switchLevel,setLevel,[42]))"
        else -> error("unexpected query")
    }

    private class RecordingGateway : SmartThingsGateway {
        var commandCount = 0
        var lastCommand: SmartThingsAction.Command? = null

        override fun listDevices(): List<SmartThingsDevice> =
            listOf(SmartThingsDevice("device-1", "Desk lamp"))

        override fun status(deviceId: String): String = """{"switch":"on"}"""

        override fun command(command: SmartThingsAction.Command) {
            commandCount += 1
            lastCommand = command
        }
    }

    private class ReversingCipher : CredentialCipher {
        override fun seal(plaintext: ByteArray): SealedCredential =
            SealedCredential(byteArrayOf(1, 2, 3), plaintext.reversedArray())

        override fun open(sealed: SealedCredential): ByteArray = sealed.ciphertext.reversedArray()
    }
}
