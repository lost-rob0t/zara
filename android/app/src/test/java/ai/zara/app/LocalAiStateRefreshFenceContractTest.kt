package ai.zara.app

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalAiStateRefreshFenceContractTest {
    @Test
    fun `local ai state refresh rejects stale completions and destroyed activity callbacks`() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(source.contains("private var localAiRefreshGeneration = 0L"))
        assertTrue(source.contains("val generation = ++localAiRefreshGeneration"))
        assertTrue(
            source.contains(
                "if (generation != localAiRefreshGeneration || isDestroyed) return@runOnUiThread"
            )
        )

        val onDestroy = source.substringAfter("override fun onDestroy()")
            .substringBefore("private fun refreshLocalAiState()")
        assertTrue(onDestroy.contains("localAiRefreshGeneration += 1"))
    }
}
