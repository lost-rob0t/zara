package ai.zara.app.prolog

fun interface NativeLibraryLoader {
    fun load(name: String)
}

interface TreallaNativeApi {
    fun initialize(coreAssetPath: String): Boolean
    fun evaluate(query: String): Array<String>
    fun shutdown()
}

class SystemNativeLibraryLoader : NativeLibraryLoader {
    override fun load(name: String) {
        System.loadLibrary(name)
    }
}

class JniTreallaNativeApi : TreallaNativeApi {
    override external fun initialize(coreAssetPath: String): Boolean
    override external fun evaluate(query: String): Array<String>
    override external fun shutdown()
}

class NativeTreallaBridge(
    private val libraryLoader: NativeLibraryLoader = SystemNativeLibraryLoader(),
    private val nativeApi: TreallaNativeApi = JniTreallaNativeApi(),
    private val libraryName: String = "zara_trealla"
) : TreallaBridge {
    private var initialized = false

    val isInitialized: Boolean
        get() = initialized

    override fun initialize(coreAssetPath: String) {
        check(!initialized) { "Trealla native bridge is already initialized" }
        require(coreAssetPath.isNotBlank()) { "Portable semantic core path is required" }
        require(libraryName.isNotBlank()) { "Trealla native library name is required" }

        try {
            libraryLoader.load(libraryName)
        } catch (error: Throwable) {
            throw IllegalStateException("Trealla native library could not be loaded", error)
        }

        val ready = try {
            nativeApi.initialize(coreAssetPath)
        } catch (error: Throwable) {
            throw IllegalStateException("Trealla native runtime initialization failed", error)
        }
        check(ready) { "Trealla native runtime initialization failed" }
        initialized = true
    }

    override fun evaluate(query: String): List<String> {
        check(initialized) { "Trealla native bridge is not initialized" }
        require(query.isNotBlank()) { "Semantic query is required" }

        return try {
            nativeApi.evaluate(query).toList()
        } catch (error: Throwable) {
            throw IllegalStateException("Trealla native evaluation failed", error)
        }
    }

    override fun shutdown() {
        if (!initialized) return
        try {
            nativeApi.shutdown()
        } finally {
            initialized = false
        }
    }
}
