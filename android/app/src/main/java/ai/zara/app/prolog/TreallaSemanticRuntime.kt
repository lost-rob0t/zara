package ai.zara.app.prolog

interface TreallaBridge {
    fun initialize(coreAssetPath: String)
    fun evaluate(query: String): List<String>
    fun shutdown()
}

class TreallaSemanticRuntime(
    private val bridge: TreallaBridge
) : SemanticRuntime, AutoCloseable {
    private enum class State {
        NEW,
        READY,
        CLOSED
    }

    private var state = State.NEW

    val isReady: Boolean
        get() = state == State.READY

    val isClosed: Boolean
        get() = state == State.CLOSED

    fun initialize(coreAssetPath: String) {
        check(state == State.NEW) { "Trealla runtime is not initializable" }
        require(coreAssetPath.isNotBlank()) { "Portable semantic core path is required" }

        bridge.initialize(coreAssetPath)
        state = State.READY
    }

    override fun evaluate(fixture: SemanticFixture): SemanticResult {
        check(state == State.READY) { "Trealla runtime is not ready" }
        return SemanticResult(
            contractVersion = PortableSemanticCore.contractVersion,
            terms = bridge.evaluate(fixture.query)
        )
    }

    override fun close() {
        if (state == State.CLOSED) return
        if (state == State.READY) bridge.shutdown()
        state = State.CLOSED
    }
}
