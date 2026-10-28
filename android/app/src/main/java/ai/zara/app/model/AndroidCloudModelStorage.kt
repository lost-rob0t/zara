package ai.zara.app.model

import ai.zara.app.auth.AndroidKeystoreCredentialCipher
import java.io.File

object AndroidCloudModelStorage {
    private const val KEY_ALIAS = "zara.cloud-model.wrap.v1"
    private const val CONFIG_FILE = "cloud-model.properties"
    private const val KEY_FILE = "cloud-model.key"

    fun configStore(root: File): CloudModelConfigStore =
        CloudModelConfigStore(File(root, CONFIG_FILE))

    fun apiKeyStore(root: File): CloudApiKeyStore =
        CloudApiKeyStore(
            File(root, KEY_FILE),
            AndroidKeystoreCredentialCipher(alias = KEY_ALIAS),
        )

    fun coordinator(root: File): CloudModelCoordinator =
        CloudModelCoordinator(
            configStore = configStore(root),
            apiKeyStore = apiKeyStore(root),
        )
}
