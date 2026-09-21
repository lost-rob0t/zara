package ai.zara.app.donations

import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class DonationStoreTest {
    @Test
    fun importPersistsValidatedDocumentAndReloads() {
        val directory = Files.createTempDirectory("zara-donations-test").toFile()
        try {
            val file = directory.resolve("donations.json")
            val store = DonationStore(file)
            val payload =
                """{"version":"ZARA-DONATIONS/1","campaigns":[{"id":"infra","title":"Infra","goal_usd":"100.00","raised_usd":"25.00"}]}"""

            val imported = store.importDocument(payload)
            val reloaded = store.state()

            assertTrue(file.isFile)
            assertEquals("25.00", imported.ledger.totalRaisedUsd.toPlainString())
            assertEquals(imported.ledger, reloaded.ledger)
            assertEquals(null, reloaded.failure)
        } finally {
            directory.deleteRecursively()
        }
    }

    @Test
    fun invalidImportDoesNotReplaceLastGoodDocument() {
        val directory = Files.createTempDirectory("zara-donations-test").toFile()
        try {
            val file = directory.resolve("donations.json")
            val store = DonationStore(file)
            val good =
                """{"version":"ZARA-DONATIONS/1","campaigns":[{"id":"infra","title":"Infra","raised_usd":"25.00"}]}"""
            store.importDocument(good)

            try {
                store.importDocument(
                    """{"version":"ZARA-DONATIONS/1","campaigns":[{"id":"infra","title":"Infra","raised_usd":"-1"}]}""",
                )
            } catch (_: DonationDocumentException) {
            }

            assertTrue(file.readText().contains("\"raised_usd\":\"25.00\""))
            assertEquals("25.00", store.state().ledger.totalRaisedUsd.toPlainString())
        } finally {
            directory.deleteRecursively()
        }
    }

    @Test
    fun missingFileIsAnEmptyHealthyState() {
        val directory = Files.createTempDirectory("zara-donations-test").toFile()
        try {
            val state = DonationStore(directory.resolve("missing.json")).state()
            assertTrue(state.ledger.campaigns.isEmpty())
            assertFalse(state.failure != null)
        } finally {
            directory.deleteRecursively()
        }
    }
}
