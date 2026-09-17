package ai.zara.app.smartthings

import ai.zara.app.auth.CredentialCipher
import ai.zara.app.auth.SealedCredential
import java.io.File
import java.util.concurrent.TimeUnit
import org.json.JSONObject
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class SmartThingsPluginTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun prologCodecAcceptsOnlyBoundedSmartThingsEffects() {
        assertEquals(
            SmartThingsAction.ListDevices,
            SmartThingsPrologCodec.decode("smartthings_action(list_devices)"),
        )
        assertEquals(
            SmartThingsAction.DeviceStatus("device-1"),
            SmartThingsPrologCodec.decode("smartthings_action(status('device-1'))"),
        )
        assertEquals(
            SmartThingsAction.Command(
                deviceId = "device-1",
                component = "main",
                capability = "switchLevel",
                command = "setLevel",
                arguments = listOf(SmartThingsArgument.IntegerValue(42)),
            ),
            SmartThingsPrologCodec.decode(
                "smartthings_action(command(\"device-1\",main,switchLevel,setLevel,[42]))",
            ),
        )
        assertEquals(
            SmartThingsAction.LocalError("unknown_device", "desk_lamp"),
            SmartThingsPrologCodec.decode(
                "smartthings_action(error(unknown_device,desk_lamp))",
            ),
        )
        assertEquals(null, SmartThingsPrologCodec.decode("ordinary_result(ok)"))

        val failure = runCatching {
            SmartThingsPrologCodec.decode("smartthings_action(command(device-1,main,switch,on,[]))")
        }.exceptionOrNull()
        assertNotNull(failure)
    }

    @Test
    fun actorSerializesGatewayCallsAndRejectsAmbiguousEffects() {
        val gateway = RecordingGateway()
        val actor = SmartThingsPluginActor(gateway)

        val devices = actor.dispatch(listOf("smartthings_action(list_devices)"))
            .get(2, TimeUnit.SECONDS)
        val status = actor.dispatch(listOf("smartthings_action(status('device-1'))"))
            .get(2, TimeUnit.SECONDS)

        assertEquals("Desk lamp [device-1]", devices?.text)
        assertTrue(devices?.success == true)
        assertTrue(status?.text?.contains("\"switch\"") == true)
        assertEquals(1, gateway.threadNames.distinct().size)
        assertTrue(gateway.threadNames.single().contains("zara-smartthings"))

        val ambiguous = runCatching {
            actor.dispatch(
                listOf(
                    "smartthings_action(status('device-1'))",
                    "smartthings_action(status('device-2'))",
                ),
            ).get(2, TimeUnit.SECONDS)
        }.exceptionOrNull()
        assertNotNull(ambiguous)
        actor.close()
    }

    @Test
    fun actorTurnsPrologValidationFailuresIntoLocalRepliesWithoutNetworkCalls() {
        val gateway = RecordingGateway()
        val actor = SmartThingsPluginActor(gateway)

        val reply = actor.dispatch(
            listOf("smartthings_action(error(unknown_device,kitchen))"),
        ).get(2, TimeUnit.SECONDS)

        assertEquals("Unknown SmartThings device alias: kitchen", reply?.text)
        assertFalse(reply?.success ?: true)
        assertTrue(gateway.threadNames.isEmpty())
        actor.close()
    }

    @Test
    fun apiGatewayUsesScopedBearerTransportAndCanonicalCommandPayload() {
        val transport = RecordingTransport(
            mutableListOf(
                SmartThingsHttpResponse(
                    200,
                    """{"items":[{"deviceId":"device-1","label":"Desk lamp","name":"lamp"}]}""",
                ),
                SmartThingsHttpResponse(
                    200,
                    """{"components":{"main":{"switch":{"switch":{"value":"on"}}}}}""",
                ),
                SmartThingsHttpResponse(200, "{}"),
            ),
        )
        val gateway = SmartThingsApiGateway(
            tokenProvider = SmartThingsTokenProvider { "secret-token" },
            transport = transport,
        )

        assertEquals(listOf(SmartThingsDevice("device-1", "Desk lamp")), gateway.listDevices())
        assertTrue(gateway.status("device-1").contains("\"value\":\"on\""))
        gateway.command(
            SmartThingsAction.Command(
                deviceId = "device-1",
                component = "main",
                capability = "switchLevel",
                command = "setLevel",
                arguments = listOf(SmartThingsArgument.IntegerValue(42)),
            ),
        )

        assertEquals("GET", transport.requests[0].method)
        assertEquals("/devices", transport.requests[0].path)
        assertEquals("secret-token", transport.requests[0].accessToken)
        assertEquals("/devices/device-1/status", transport.requests[1].path)
        val command = JSONObject(transport.requests[2].body!!)
            .getJSONArray("commands")
            .getJSONObject(0)
        assertEquals("main", command.getString("component"))
        assertEquals("switchLevel", command.getString("capability"))
        assertEquals("setLevel", command.getString("command"))
        assertEquals(42, command.getJSONArray("arguments").getInt(0))
    }

    @Test
    fun apiGatewayDoesNotLeakRemoteErrorBodies() {
        val gateway = SmartThingsApiGateway(
            tokenProvider = SmartThingsTokenProvider { "secret-token" },
            transport = RecordingTransport(
                mutableListOf(SmartThingsHttpResponse(403, "server says secret-token is invalid")),
            ),
        )

        val failure = runCatching { gateway.listDevices() }.exceptionOrNull()
        assertNotNull(failure)
        assertTrue(failure?.message?.contains("required SmartThings scope") == true)
        assertFalse(failure?.message?.contains("secret-token") == true)
    }

    @Test
    fun credentialStoreEncryptsPatAndFailsClosedAfterExpiry() {
        var now = 1_000L
        val file = File(temporary.root, "smartthings.bin")
        val store = SmartThingsCredentialStore(
            file = file,
            cipher = ReversingCipher(),
            nowMillis = { now },
        )

        store.savePersonalAccessToken("secret-token", expiresAtEpochMillis = 2_000L)

        assertEquals(SmartThingsCredentialState.READY, store.state())
        assertEquals("secret-token", store.requireAccessToken())
        assertFalse(file.readBytes().toString(Charsets.UTF_8).contains("secret-token"))

        now = 2_001L
        assertEquals(SmartThingsCredentialState.EXPIRED, store.state())
        assertNotNull(runCatching { store.requireAccessToken() }.exceptionOrNull())
        assertTrue(store.clear())
        assertEquals(SmartThingsCredentialState.MISSING, store.state())
    }

    @Test
    fun bundledPrologSourceKeepsAliasesAsUserOwnedFacts() {
        val source = SmartThingsPrologPlugin.source

        assertTrue(source.contains("smartthings_device(_Alias, _DeviceId) :- fail."))
        assertTrue(source.contains("smartthings_action(list_devices)"))
        assertTrue(source.contains("smartthings_action(status(DeviceId))"))
        assertTrue(source.contains("smartthings_action(command("))
        assertTrue(source.contains("expert_activation(smartthings_on, on)."))
        assertTrue(source.contains("expert_activation(smartthings_off, off)."))
        assertFalse(source.contains("access_token"))
        assertFalse(source.contains("client_secret"))
    }

    private class RecordingGateway : SmartThingsGateway {
        val threadNames = mutableListOf<String>()

        override fun listDevices(): List<SmartThingsDevice> {
            threadNames += Thread.currentThread().name
            return listOf(SmartThingsDevice("device-1", "Desk lamp"))
        }

        override fun status(deviceId: String): String {
            threadNames += Thread.currentThread().name
            return """{"switch":"on"}"""
        }

        override fun command(command: SmartThingsAction.Command) {
            threadNames += Thread.currentThread().name
        }
    }

    private class RecordingTransport(
        private val responses: MutableList<SmartThingsHttpResponse>,
    ) : SmartThingsHttpTransport {
        val requests = mutableListOf<SmartThingsHttpRequest>()

        override fun execute(request: SmartThingsHttpRequest): SmartThingsHttpResponse {
            requests += request
            return responses.removeAt(0)
        }
    }

    private class ReversingCipher : CredentialCipher {
        override fun seal(plaintext: ByteArray): SealedCredential =
            SealedCredential(byteArrayOf(1, 2, 3), plaintext.reversedArray())

        override fun open(sealed: SealedCredential): ByteArray = sealed.ciphertext.reversedArray()
    }
}
