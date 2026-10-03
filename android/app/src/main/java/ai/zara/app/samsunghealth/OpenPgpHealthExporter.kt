package ai.zara.app.samsunghealth

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.security.SecureRandom
import java.util.Date
import org.bouncycastle.bcpg.CompressionAlgorithmTags
import org.bouncycastle.bcpg.SymmetricKeyAlgorithmTags
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.PGPCompressedDataGenerator
import org.bouncycastle.openpgp.PGPEncryptedDataGenerator
import org.bouncycastle.openpgp.PGPLiteralData
import org.bouncycastle.openpgp.PGPLiteralDataGenerator
import org.bouncycastle.openpgp.PGPPublicKey
import org.bouncycastle.openpgp.PGPPublicKeyRingCollection
import org.bouncycastle.openpgp.PGPUtil
import org.bouncycastle.openpgp.operator.jcajce.JcaKeyFingerprintCalculator
import org.bouncycastle.openpgp.operator.jcajce.JcePGPDataEncryptorBuilder
import org.bouncycastle.openpgp.operator.jcajce.JcePublicKeyKeyEncryptionMethodGenerator

class HealthOpenPgpException(message: String, cause: Throwable? = null) : Exception(message, cause)

/** Interoperable binary OpenPGP export for one or many recipient public keys. */
class OpenPgpHealthExporter {
    private val provider = BouncyCastleProvider()

    fun encrypt(plaintext: ByteArray, armoredRecipientKeys: List<ByteArray>): ByteArray {
        require(plaintext.isNotEmpty()) { "health export must not be empty" }
        require(plaintext.size <= MAX_PLAINTEXT_BYTES) { "health export is too large" }
        require(armoredRecipientKeys.size in 1..MAX_RECIPIENTS) {
            "health export requires 1 to $MAX_RECIPIENTS recipients"
        }
        try {
            val recipients = armoredRecipientKeys
                .flatMap(::encryptionKeys)
                .distinctBy(PGPPublicKey::getKeyID)
            require(recipients.isNotEmpty()) { "no OpenPGP encryption key was provided" }
            require(recipients.size <= MAX_RECIPIENTS) { "too many OpenPGP encryption keys" }

            val output = ByteArrayOutputStream()
            val encryptor = PGPEncryptedDataGenerator(
                JcePGPDataEncryptorBuilder(SymmetricKeyAlgorithmTags.AES_256)
                    .setWithIntegrityPacket(true)
                    .setSecureRandom(SecureRandom())
                    .setProvider(provider),
            )
            recipients.forEach { key ->
                encryptor.addMethod(JcePublicKeyKeyEncryptionMethodGenerator(key).setProvider(provider))
            }
            encryptor.open(output, ByteArray(BUFFER_BYTES)).use { encrypted ->
                val compressor = PGPCompressedDataGenerator(CompressionAlgorithmTags.ZIP)
                try {
                    compressor.open(encrypted).use { compressed ->
                        PGPLiteralDataGenerator().open(
                            compressed,
                            PGPLiteralData.BINARY,
                            "health.pl",
                            plaintext.size.toLong(),
                            Date(0),
                        ).use { literal -> literal.write(plaintext) }
                    }
                } finally {
                    compressor.close()
                }
            }
            return output.toByteArray()
        } catch (error: IllegalArgumentException) {
            throw error
        } catch (error: Exception) {
            throw HealthOpenPgpException("Health OpenPGP export failed", error)
        }
    }

    fun recipientKeyIds(armoredRecipientKey: ByteArray): Set<Long> =
        encryptionKeys(armoredRecipientKey).mapTo(linkedSetOf(), PGPPublicKey::getKeyID)

    private fun encryptionKeys(armored: ByteArray): List<PGPPublicKey> {
        require(armored.size in 1..MAX_KEY_BYTES) { "OpenPGP public key has invalid size" }
        val collection = PGPPublicKeyRingCollection(
            PGPUtil.getDecoderStream(ByteArrayInputStream(armored)),
            JcaKeyFingerprintCalculator(),
        )
        return buildList {
            val rings = collection.keyRings
            while (rings.hasNext()) {
                val keys = rings.next().publicKeys
                while (keys.hasNext()) {
                    val key = keys.next()
                    if (key.isEncryptionKey) add(key)
                }
            }
        }
    }

    private companion object {
        const val MAX_RECIPIENTS = 32
        const val MAX_KEY_BYTES = 256 * 1024
        const val MAX_PLAINTEXT_BYTES = 16 * 1024 * 1024
        const val BUFFER_BYTES = 64 * 1024
    }
}
