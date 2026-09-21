package ai.zara.app.finance

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class StarFundsClientTest {
    private class RecordingTransport : StarFundsHttpTransport {
        data class Call(
            val method: String,
            val url: String,
            val token: String?,
            val body: String?,
        )

        val calls = mutableListOf<Call>()

        override fun request(
            method: String,
            url: String,
            bearerToken: String?,
            body: String?,
        ): StarFundsHttpResponse {
            calls += Call(method, url, bearerToken, body)
            return StarFundsHttpResponse(200, """{"ok":true}""")
        }
    }

    @Test
    fun remoteEndpointRequiresHttpsAndRejectsCredentials() {
        StarFundsEndpoint.create("https://funds.starintel.actor")

        runCatching { StarFundsEndpoint.create("http://funds.starintel.actor") }
            .onSuccess { error("remote HTTP must be rejected") }

        runCatching { StarFundsEndpoint.create("https://user:pass@funds.starintel.actor") }
            .onSuccess { error("URL credentials must be rejected") }
    }

    @Test
    fun loopbackHttpIsAllowedForDevelopment() {
        assertEquals(
            "http://127.0.0.1:5010",
            StarFundsEndpoint.create("http://127.0.0.1:5010/").baseUrl,
        )
    }

    @Test
    fun paperAttemptUsesOnlyPaperTaskRoute() {
        val transport = RecordingTransport()
        val client =
            StarFundsClient(
                endpoint = StarFundsEndpoint.create("https://funds.starintel.actor"),
                tokenProvider = StarFundsTokenProvider { "private-token" },
                transport = transport,
            )

        val response =
            client.kalshiPaperAttempt(
                ticker = "KXTEST",
                word = "test",
                minimumMentions = 2,
                side = "YES",
                limitPrice = 0.42,
                maxSize = 3,
                minimumVolume = 100.0,
                maximumSpread = 0.05,
            )

        assertEquals(200, response.status)
        assertEquals(1, transport.calls.size)
        val call = transport.calls.single()
        assertEquals("POST", call.method)
        assertEquals(
            "https://funds.starintel.actor/api/v1/star-funds/tasks/kalshi-paper-attempt",
            call.url,
        )
        assertEquals("private-token", call.token)
        assertTrue(call.body!!.contains("\"side\":\"yes\""))
        assertFalse(call.url.contains("live", ignoreCase = true))
        assertFalse(call.body.contains("live", ignoreCase = true))
    }

    @Test
    fun researchTopIsBoundedBeforeNetworkCall() {
        val transport = RecordingTransport()
        val client =
            StarFundsClient(
                StarFundsEndpoint.create("https://funds.starintel.actor"),
                StarFundsTokenProvider { null },
                transport,
            )

        client.researchOpportunities(500)

        assertEquals("""{"top":100}""", transport.calls.single().body)
    }
}
