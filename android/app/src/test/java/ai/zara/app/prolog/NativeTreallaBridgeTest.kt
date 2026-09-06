package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class NativeTreallaBridgeTest {

    @Test
    fun initializeLoadsPinnedLibraryBeforeNativeCore() {
        val events = mutableListOf<String>()
        val bridge = NativeTreallaBridge(
            libraryLoader = NativeLibraryLoader { name -> events += "load:$name" },
            nativeApi = FakeNativeApi(events = events)
        )

        bridge.initialize("prolog/portable/semantic_core.pl")

        assertEquals(
            listOf("load:zara_trealla", "init:prolog/portable/semantic_core.pl"),
            events
        )
        assertTrue(bridge.isInitialized)
    }

    @Test
    fun nativeInitializationFailureFailsClosed() {
        val bridge = NativeTreallaBridge(
            libraryLoader = NativeLibraryLoader { },
            nativeApi = FakeNativeApi(initializeResult = false)
        )

        try {
            bridge.initialize("prolog/portable/semantic_core.pl")
            throw AssertionError("failed native initialization must be surfaced")
        } catch (_: IllegalStateException) {
        }

        assertFalse(bridge.isInitialized)
    }

    @Test
    fun libraryLoadFailureNeverCallsNativeApi() {
        val api = FakeNativeApi()
        val bridge = NativeTreallaBridge(
            libraryLoader = NativeLibraryLoader { error("missing native library") },
            nativeApi = api
        )

        try {
            bridge.initialize("prolog/portable/semantic_core.pl")
            throw AssertionError("missing native library must fail closed")
        } catch (_: IllegalStateException) {
        }

        assertEquals(0, api.initializeCalls)
        assertFalse(bridge.isInitialized)
    }

    @Test
    fun evaluateRequiresInitializationAndPreservesNativeTerms() {
        val api = FakeNativeApi(results = arrayOf("frame(timer,1200)", "slot(label,'tea')"))
        val bridge = NativeTreallaBridge(
            libraryLoader = NativeLibraryLoader { },
            nativeApi = api
        )

        try {
            bridge.evaluate("semantic(timer)")
            throw AssertionError("evaluation before initialization must fail")
        } catch (_: IllegalStateException) {
        }

        bridge.initialize("prolog/portable/semantic_core.pl")

        assertEquals(
            listOf("frame(timer,1200)", "slot(label,'tea')"),
            bridge.evaluate("semantic(timer)")
        )
    }

    @Test
    fun shutdownIsIdempotentAndClosesBridge() {
        val api = FakeNativeApi()
        val bridge = NativeTreallaBridge(
            libraryLoader = NativeLibraryLoader { },
            nativeApi = api
        )
        bridge.initialize("prolog/portable/semantic_core.pl")

        bridge.shutdown()
        bridge.shutdown()

        assertEquals(1, api.shutdownCalls)
        assertFalse(bridge.isInitialized)
    }

    @Test
    fun repeatedInitEvaluateShutdownCyclesReleaseEveryNativeRuntime() {
        val cycles = 512
        val api = FakeNativeApi(results = arrayOf("frame(timer,1200)"))
        var libraryLoads = 0
        val bridge = NativeTreallaBridge(
            libraryLoader = NativeLibraryLoader { libraryLoads += 1 },
            nativeApi = api,
        )

        repeat(cycles) {
            bridge.initialize("prolog/portable/semantic_core.pl")
            assertEquals(listOf("frame(timer,1200)"), bridge.evaluate("semantic(timer)"))
            bridge.shutdown()
            assertFalse(bridge.isInitialized)
        }

        assertEquals(cycles, libraryLoads)
        assertEquals(cycles, api.initializeCalls)
        assertEquals(cycles, api.evaluateCalls)
        assertEquals(cycles, api.shutdownCalls)
    }

    private class FakeNativeApi(
        private val events: MutableList<String> = mutableListOf(),
        private val initializeResult: Boolean = true,
        private val results: Array<String> = emptyArray()
    ) : TreallaNativeApi {
        var initializeCalls = 0
        var evaluateCalls = 0
        var shutdownCalls = 0

        override fun initialize(coreAssetPath: String): Boolean {
            initializeCalls += 1
            events += "init:$coreAssetPath"
            return initializeResult
        }

        override fun evaluate(query: String): Array<String> {
            evaluateCalls += 1
            return results
        }

        override fun shutdown() {
            shutdownCalls += 1
        }
    }
}
