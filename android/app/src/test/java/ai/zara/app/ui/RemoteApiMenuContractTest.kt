package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class RemoteApiMenuContractTest {
    @Test
    fun remoteApiMenuOwnsProviderConfigurationAndSecretBoundary() {
        val navigation = File("src/main/java/ai/zara/app/ui/AppNavigation.kt").readText()
        val ui = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val storage = File("src/main/java/ai/zara/app/model/AndroidCloudModelStorage.kt").readText()
        val keyStore = File("src/main/java/ai/zara/app/model/CloudApiKeyStore.kt").readText()

        assertTrue(navigation.contains("RemoteApis(AppMenu.Settings, \"Remote APIs\")"))
        assertTrue(ui.contains("AppRoute.RemoteApis ->"))
        assertTrue(ui.contains("\"REMOTE API STATUS\""))
        assertTrue(ui.contains("\"OpenRouter\""))
        assertTrue(ui.contains("\"Generic OpenAI-compatible\""))
        assertTrue(ui.contains("\"StarIntel\""))
        assertTrue(ui.contains("\"StatIntel\""))
        assertTrue(ui.contains("\"Z.AI Coding Plan\""))
        assertTrue(ui.contains("RemoteProviderCard("))
        assertTrue(ui.contains("Switch("))
        assertTrue(activity.contains("AndroidCloudModelStorage.coordinator"))
        assertTrue(storage.contains("AndroidKeystoreCredentialCipher"))
        assertTrue(keyStore.contains("cipher.seal(plaintext)"))
        assertFalse(keyStore.contains("Properties()"))
    }

    @Test
    fun switchingProviderOrEndpointFencesStoredCredential() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val save = activity
            .substringAfter("onSaveRemoteApi =")
            .substringBefore("onClearRemoteApiKey =")

        assertTrue(save.contains("previous.provider != config.provider"))
        assertTrue(save.contains("previous.endpoint != config.endpoint"))
        assertTrue(save.contains("cloudModel.clearApiKey()"))
        assertTrue(save.contains("cloudModel.setApiKey(apiKey)"))
    }
}
