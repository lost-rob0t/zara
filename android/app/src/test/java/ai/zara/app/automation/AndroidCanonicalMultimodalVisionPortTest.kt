package ai.zara.app.automation

import ai.zara.app.localai.LocalGenerationRequest
import ai.zara.app.localai.LocalGenerationResult
import ai.zara.app.localai.LocalModelQuantization
import ai.zara.app.prolog.AndroidAutomationAction
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit

class AndroidCanonicalMultimodalVisionPortTest {
    @Test
    fun `interpretation sends PNG through existing generation request and parses one typed action`() {
        val image = png(7)
        var request: LocalGenerationRequest? = null
        val port = port { value ->
            request = value
            completed("actions([adb_tap(120,240)])")
        }

        val decision = port.interpret(AndroidVisionObservation("tap Continue", image, 0))
            .get(2, TimeUnit.SECONDS)

        assertEquals(AndroidVisionDecision.Act(AndroidAutomationAction.AdbTap(120, 240)), decision)
        assertArrayEquals(image, request!!.imagePng)
        assertTrue(request!!.prompt.contains("Never emit shell"))
    }

    @Test
    fun `raw shell or malformed model output is unavailable instead of executable`() {
        val port = port { completed("adb shell input tap 1 2") }

        val decision = port.interpret(AndroidVisionObservation("tap", png(1), 0))
            .get(2, TimeUnit.SECONDS)

        assertTrue(decision is AndroidVisionDecision.Unavailable)
    }

    @Test
    fun `verification sends the fresh PNG and requires exact verified token`() {
        val requests = mutableListOf<LocalGenerationRequest>()
        val port = port { request ->
            requests += request
            completed("verified")
        }
        val fresh = png(9)

        val verified = port.verify(
            AndroidVisionVerification(
                goal = "go home",
                action = AndroidAutomationAction.AdbKey(AdbAutomationKey.Home),
                png = fresh,
                sequence = 1,
            )
        ).get(2, TimeUnit.SECONDS)

        assertTrue(verified)
        assertArrayEquals(fresh, requests.single().imagePng)
    }

    @Test
    fun `provider failure becomes typed unavailable and does not select a fallback`() {
        val port = port {
            CompletableFuture<LocalGenerationResult>().also { future ->
                future.completeExceptionally(IllegalStateException("selected model has no image input"))
            }
        }

        val decision = port.interpret(AndroidVisionObservation("inspect", png(2), 0))
            .get(2, TimeUnit.SECONDS)

        assertEquals(
            AndroidVisionDecision.Unavailable("selected model has no image input"),
            decision,
        )
    }

    @Test(expected = IllegalArgumentException::class)
    fun `local generation request rejects non PNG image bytes`() {
        LocalGenerationRequest(
            prompt = "vision",
            imagePng = byteArrayOf(1, 2, 3, 4, 5, 6, 7, 8),
        )
    }

    private fun port(
        generate: (LocalGenerationRequest) -> CompletableFuture<LocalGenerationResult>,
    ): AndroidCanonicalMultimodalVisionPort = AndroidCanonicalMultimodalVisionPort(
        generate = generate,
        cancelGeneration = {},
    )

    private fun completed(text: String): CompletableFuture<LocalGenerationResult> =
        CompletableFuture.completedFuture(
            LocalGenerationResult(
                text = text,
                modelId = "vision-model",
                modelVersion = "1",
                quantization = LocalModelQuantization.INT4,
                generation = 3,
            )
        )

    private fun png(marker: Int): ByteArray = byteArrayOf(
        0x89.toByte(), 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
        marker.toByte(),
    )
}
