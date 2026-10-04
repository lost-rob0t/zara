package ai.zara.app.samsunghealth

import java.io.File
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Test

class HealthOpenPgpRecipientStoreTest {
    @Test
    fun storesMultiplePublicRecipientKeysWithoutPrivateKeyMaterial() {
        val file = kotlin.io.path.createTempDirectory().resolve("recipients.bin").toFile()
        val store = HealthOpenPgpRecipientStore(file) { key -> setOf(key.first().toLong()) }

        store.add(byteArrayOf(1, 2, 3))
        store.add(byteArrayOf(4, 5, 6))

        val loaded = store.load()
        assertEquals(2, loaded.size)
        assertArrayEquals(byteArrayOf(1, 2, 3), loaded[0])
        assertArrayEquals(byteArrayOf(4, 5, 6), loaded[1])
        assertEquals(false, file.readText(Charsets.ISO_8859_1).contains("PRIVATE KEY"))
    }

    @Test
    fun deduplicatesRecipientKeyIdsAcrossImports() {
        val file = kotlin.io.path.createTempDirectory().resolve("recipients.bin").toFile()
        val store = HealthOpenPgpRecipientStore(file) { setOf(42L) }

        store.add(byteArrayOf(1))
        store.add(byteArrayOf(2))

        assertEquals(1, store.load().size)
        assertArrayEquals(byteArrayOf(2), store.load().single())
    }

    @Test(expected = HealthOpenPgpRecipientStoreException::class)
    fun rejectsMalformedStoredEnvelope() {
        val file = kotlin.io.path.createTempDirectory().resolve("recipients.bin").toFile()
        file.writeBytes(byteArrayOf(0, 1, 2))

        HealthOpenPgpRecipientStore(file) { setOf(1L) }.load()
    }
}
