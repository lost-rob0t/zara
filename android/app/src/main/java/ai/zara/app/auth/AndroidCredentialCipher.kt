package ai.zara.app.auth

import android.security.keystore.KeyGenParameterSpec
import android.security.keystore.KeyProperties
import java.security.GeneralSecurityException
import java.security.KeyStore
import javax.crypto.Cipher
import javax.crypto.KeyGenerator
import javax.crypto.SecretKey
import javax.crypto.spec.GCMParameterSpec

class AndroidKeystoreCredentialCipher(
    private val alias: String = "zara.curve.wrap.v1",
) : CredentialCipher {

    override fun seal(plaintext: ByteArray): SealedCredential = crypto("encrypt") {
        val cipher = Cipher.getInstance(TRANSFORMATION)
        cipher.init(Cipher.ENCRYPT_MODE, getOrCreateKey())
        SealedCredential(cipher.iv, cipher.doFinal(plaintext))
    }

    override fun open(sealed: SealedCredential): ByteArray = crypto("decrypt") {
        val key = getExistingKey()
            ?: throw CredentialCipherException("credential wrapping key is missing")
        val cipher = Cipher.getInstance(TRANSFORMATION)
        cipher.init(Cipher.DECRYPT_MODE, key, GCMParameterSpec(GCM_TAG_BITS, sealed.iv))
        cipher.doFinal(sealed.ciphertext)
    }

    fun deleteWrappingKey(): Boolean = crypto("delete wrapping key") {
        val keyStore = loadKeyStore()
        if (keyStore.containsAlias(alias)) keyStore.deleteEntry(alias)
        true
    }

    private fun getOrCreateKey(): SecretKey {
        getExistingKey()?.let { return it }
        val generator = KeyGenerator.getInstance(
            KeyProperties.KEY_ALGORITHM_AES,
            ANDROID_KEYSTORE,
        )
        val specification = KeyGenParameterSpec.Builder(
            alias,
            KeyProperties.PURPOSE_ENCRYPT or KeyProperties.PURPOSE_DECRYPT,
        )
            .setBlockModes(KeyProperties.BLOCK_MODE_GCM)
            .setEncryptionPaddings(KeyProperties.ENCRYPTION_PADDING_NONE)
            .setKeySize(256)
            .setRandomizedEncryptionRequired(true)
            .setUserAuthenticationRequired(false)
            .build()
        generator.init(specification)
        return generator.generateKey()
    }

    private fun getExistingKey(): SecretKey? =
        loadKeyStore().getKey(alias, null) as? SecretKey

    private fun loadKeyStore(): KeyStore =
        KeyStore.getInstance(ANDROID_KEYSTORE).apply { load(null) }

    private fun <T> crypto(operation: String, block: () -> T): T {
        try {
            return block()
        } catch (error: CredentialCipherException) {
            throw error
        } catch (error: GeneralSecurityException) {
            throw CredentialCipherException("failed to $operation CURVE credential", error)
        } catch (error: java.io.IOException) {
            throw CredentialCipherException("failed to access Android Keystore", error)
        }
    }

    private companion object {
        const val ANDROID_KEYSTORE = "AndroidKeyStore"
        const val TRANSFORMATION = "AES/GCM/NoPadding"
        const val GCM_TAG_BITS = 128
    }
}
