package ai.zara.app.samsunghealth

import ai.zara.app.auth.CredentialCipher
import ai.zara.app.auth.SealedCredential
import ai.zara.ui.health.HealthGoalMetric
import ai.zara.ui.health.HealthGoalTarget
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class HealthGoalStoreTest {
    @Test
    fun encryptedStoreRoundTripsEveryGoalWithoutPlaintextLeakage() {
        val file = Files.createTempDirectory("zara-health-goals").resolve("goals.bin").toFile()
        val store = HealthGoalStore(file, FakeCipher())
        val goals = HealthGoalMetric.entries.map { HealthGoalTarget(it, it.defaultTarget) }

        store.save(goals)

        assertEquals(goals, store.load())
        val persisted = file.readBytes()
        assertFalse(String(persisted).contains("steps"))
        assertFalse(String(persisted).contains("sleep"))
        assertTrue(file.parentFile.isDirectory)
    }

    @Test
    fun missingStoreUsesSafeStepAndSleepDefaults() {
        val file = Files.createTempDirectory("zara-health-goals-empty").resolve("goals.bin").toFile()
        val goals = HealthGoalStore(file, FakeCipher()).loadOrDefaults()

        assertEquals(10_000, goals.first { it.metric == HealthGoalMetric.STEPS }.target)
        assertEquals(480, goals.first { it.metric == HealthGoalMetric.SLEEP }.target)
    }

    @Test(expected = HealthGoalStoreException::class)
    fun corruptEnvelopeFailsClosedInsteadOfResettingGoals() {
        val file = Files.createTempDirectory("zara-health-goals-corrupt").resolve("goals.bin").toFile()
        file.writeBytes(byteArrayOf(1, 2, 3))
        HealthGoalStore(file, FakeCipher()).load()
    }
}

private class FakeCipher : CredentialCipher {
    override fun seal(plaintext: ByteArray): SealedCredential =
        SealedCredential(
            byteArrayOf(9),
            plaintext.reversedArray().map { (it.toInt() xor 0x5A).toByte() }.toByteArray(),
        )

    override fun open(sealed: SealedCredential): ByteArray =
        sealed.ciphertext.map { (it.toInt() xor 0x5A).toByte() }.toByteArray().reversedArray()
}
