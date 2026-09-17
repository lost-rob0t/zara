package ai.zara.app.localai

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class EmbeddedLocalAiContractTest {
    @Test
    fun androidBuildPinsLiteRtLmAndDeclaresAcceleratorLibraries() {
        val catalog = File("../gradle/libs.versions.toml").readText()
        val build = File("build.gradle.kts").readText()
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(catalog.contains("litertLm = \"0.17.0\""))
        assertTrue(catalog.contains("litert-lm-android"))
        assertTrue(build.contains("libs.litert.lm.android"))
        assertTrue(manifest.contains("libOpenCL.so"))
        assertTrue(manifest.contains("libvndksupport.so"))
    }

    @Test
    fun modelAndTtsAreOwnedByNonExportedVoiceProcessService() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val service = File("src/main/java/ai/zara/app/localai/LocalAiService.kt").readText()
        val tts = File("src/main/java/ai/zara/app/localai/AndroidOfflineTtsBackend.kt").readText()

        assertTrue(manifest.contains(".localai.LocalAiService"))
        assertTrue(manifest.contains("android:exported=\"false\""))
        assertTrue(manifest.contains("android:process=\":voice\""))
        assertTrue(service.contains("LocalAiRuntime"))
        assertTrue(service.contains("LiteRtLocalLlmBackend"))
        assertTrue(service.contains("AndroidOfflineTtsBackend"))
        assertTrue(tts.contains("isNetworkConnectionRequired"))
        assertTrue(tts.contains("engine.stop()"))
    }

    @Test
    fun wasmBoundaryIsExplicitAndDoesNotReplaceNativeAndroidAcceleration() {
        val abi = File("src/main/java/ai/zara/app/localai/LocalAiAbi.kt").readText()
        val docs = File("../../wiki/android-local-ai.org").readText()

        assertTrue(abi.contains("ABI_VERSION"))
        assertTrue(abi.contains("local-ai-v1"))
        assertTrue(docs.contains("WebAssembly"))
        assertTrue(docs.contains("WebGPU"))
        assertTrue(docs.contains("native Kotlin LiteRT-LM"))
    }
}
