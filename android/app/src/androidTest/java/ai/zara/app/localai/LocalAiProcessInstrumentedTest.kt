package ai.zara.app.localai

import android.app.ActivityManager
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.util.concurrent.TimeUnit
import org.junit.Assert.*
import org.junit.Test
import org.junit.Assume.assumeTrue

class LocalAiProcessInstrumentedTest {
    @Test fun stopBeforeServiceBindingCannotPlayQueuedSpeechLater() {
        val context = InstrumentationRegistry.getInstrumentation().targetContext
        LocalAiServiceClient(context).use { client ->
            val queued = client.speak("This superseded request must stop.")
            client.stopSpeech()
            assertThrows(Exception::class.java) { queued.get(20, TimeUnit.SECONDS) }
            assertNotEquals(LocalTtsPhase.SPEAKING, client.ttsState().get(20, TimeUnit.SECONDS).phase)
        }
    }

    @Test fun realModelCanImportGenerateUnloadReloadAndGenerateAgain() {
        assumeTrue("Requires the pinned external model fixture; fake models do not satisfy this gate",
            InstrumentationRegistry.getArguments().getString("realModel") == "true")
        val context = InstrumentationRegistry.getInstrumentation().targetContext
        val source = java.io.File(context.cacheDir, "Qwen2_0.5B_Instruct.litertlm")
        assertTrue("Copy the pinned model into the target app cache before running", source.isFile)
        val metadata = LocalModelMetadata("qwen2-0.5b-fixture", "13aab3e5", LocalModelQuantization.INT8,
            "0f01cc004b8eb62b92ba6be85ed05a248ba0d2f78af94c4949b313eccfb4c157", 1024, LocalModelBackend.CPU)
        LocalAiServiceClient(context).use { client ->
            client.installModel(android.net.Uri.fromFile(source), metadata).get(180, TimeUnit.SECONDS)
            for (iteration in 0..1) {
                if (iteration == 1) client.selectModel(metadata.id, metadata.version).get(180, TimeUnit.SECONDS)
                val chunks = mutableListOf<String>()
                val answer = client.generate(LocalGenerationRequest("Write a short greeting.", 32), chunks::add)
                    .get(180, TimeUnit.SECONDS)
                assertTrue(answer.text.isNotBlank())
                assertEquals(metadata.id, answer.modelId)
                assertEquals(metadata.version, answer.modelVersion)
                assertEquals(answer.text, chunks.joinToString(""))
                client.unloadModel().get(60, TimeUnit.SECONDS)
            }
        }
    }

    @Test fun mainAppReachesModelAndSpeechOwnerInVoiceProcess() {
        val context = InstrumentationRegistry.getInstrumentation().targetContext
        val client = LocalAiServiceClient(context)
        try {
            val state = client.state().get(20, TimeUnit.SECONDS)
            assertNotNull(state)
            client.models().get(20, TimeUnit.SECONDS)
            val speech = client.refreshSpeech().get(20, TimeUnit.SECONDS)
            assertTrue(speech.phase in setOf(LocalTtsPhase.READY, LocalTtsPhase.UNAVAILABLE))
            assertEquals("android-offline", client.ttsProviders().get(20, TimeUnit.SECONDS).single().id)
            val processes = (context.getSystemService(Context.ACTIVITY_SERVICE) as ActivityManager).runningAppProcesses
            val host = processes.first { it.processName == "${context.packageName}:voice" }
            assertNotEquals(android.os.Process.myPid(), host.pid)
            client.stopSpeech()
        } finally { client.close() }
    }

    @Test fun stoppedSpeechRejectsNetworkVoiceAndCanRefreshWithoutModel() {
        val context = InstrumentationRegistry.getInstrumentation().targetContext
        LocalAiServiceClient(context).use { client ->
            val speech = client.refreshSpeech().get(20, TimeUnit.SECONDS)
            if (speech.phase == LocalTtsPhase.READY) {
                client.speak("Zara offline speech is working.").get(30, TimeUnit.SECONDS)
                assertEquals(LocalTtsPhase.READY, client.ttsState().get(10, TimeUnit.SECONDS).phase)
            } else {
                assertEquals(LocalTtsPhase.UNAVAILABLE, speech.phase)
                assertNotNull(speech.failure)
                assertThrows(Exception::class.java) { client.speak("hello").get(10, TimeUnit.SECONDS) }
            }
            client.refreshSpeech().get(20, TimeUnit.SECONDS)
        }
    }
}
