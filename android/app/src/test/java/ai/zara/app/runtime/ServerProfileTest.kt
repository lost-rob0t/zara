package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ServerProfileTest {

    @Test fun `canonical tcp endpoint is accepted`() {
        val profile = ServerProfile.create("tcp://zara.example:7731")
        assertEquals("tcp://zara.example:7731", profile.endpoint)
    }

    @Test fun `ipv6 endpoint is accepted`() {
        val profile = ServerProfile.create("tcp://[2001:db8::1]:7731")
        assertEquals("tcp://[2001:db8::1]:7731", profile.endpoint)
    }

    @Test fun `credentials query fragment and paths are rejected`() {
        listOf(
            "tcp://user:secret@zara.example:7731",
            "tcp://zara.example:7731/?token=secret",
            "tcp://zara.example:7731/#frag",
            "tcp://zara.example:7731/path",
        ).forEach { endpoint ->
            assertThrows(IllegalArgumentException::class.java) { ServerProfile.create(endpoint) }
        }
    }

    @Test fun `non tcp and incomplete endpoints are rejected`() {
        listOf(
            "http://zara.example:7731",
            "ipc:///tmp/zara.sock",
            "tcp://zara.example",
            "tcp://:7731",
            "tcp://zara.example:0",
            "tcp://zara.example:65536",
            "",
        ).forEach { endpoint ->
            assertThrows(IllegalArgumentException::class.java) { ServerProfile.create(endpoint) }
        }
    }
}
