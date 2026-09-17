package ai.zara.store.repository

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class FdroidIndexDecoderTest {
    @Test
    fun decodesStandardV2RepositoryWithoutZaraMetadata() {
        val snapshot = FdroidIndexDecoder.decodeV2Untrusted(
            """
            {
              "repo": {
                "address": "https://repo.zara.invalid/fdroid/repo",
                "timestamp": 1726531200000
              },
              "packages": {
                "ai.zara.notes": {
                  "metadata": {
                    "added": 1726531200000,
                    "lastUpdated": 1726531200000
                  }
                },
                "ai.zara.store": {
                  "metadata": {
                    "added": 1726531200000,
                    "lastUpdated": 1726531200000
                  }
                }
              }
            }
            """.trimIndent(),
        )

        assertEquals("https://repo.zara.invalid/fdroid/repo", snapshot.address)
        assertEquals(1726531200000, snapshot.timestamp)
        assertEquals(listOf("ai.zara.notes", "ai.zara.store"), snapshot.packageNames)
    }

    @Test
    fun malformedIndexFailsClosed() {
        assertThrows(Exception::class.java) {
            FdroidIndexDecoder.decodeV2Untrusted("{ not-json }")
        }
    }

    @Test
    fun blankRepositoryAddressIsRejectedAfterDecode() {
        assertThrows(IllegalArgumentException::class.java) {
            FdroidIndexDecoder.decodeV2Untrusted(
                """
                {
                  "repo": {
                    "address": "",
                    "timestamp": 1
                  }
                }
                """.trimIndent(),
            )
        }
    }
}
