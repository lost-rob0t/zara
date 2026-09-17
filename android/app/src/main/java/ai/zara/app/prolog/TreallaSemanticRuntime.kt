package ai.zara.app.prolog

interface TreallaBridge {
    fun initialize(coreAssetPath: String)
    fun consult(sourcePath: String)
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

    fun initialize(coreFilePath: String) {
        check(state == State.NEW) { "Trealla runtime is not initializable" }
        require(coreFilePath.isNotBlank()) { "Portable semantic core path is required" }

        bridge.initialize(coreFilePath)
        state = State.READY
    }

    fun initializeFromAssets(
        stager: PortableSemanticAssetStager,
        source: PortableSemanticAssetSource
    ) {
        check(state == State.NEW) { "Trealla runtime is not initializable" }
        val staged = stager.stageAll(source)
        initialize(staged.coreFile.absolutePath)
    }

    override fun evaluate(fixture: SemanticFixture): SemanticResult {
        check(state == State.READY) { "Trealla runtime is not ready" }
        return SemanticResult(
            contractVersion = PortableSemanticCore.contractVersion,
            terms = bridge.evaluate(fixture.query)
        )
    }

    fun consult(sourcePath: String) {
        check(state == State.READY) { "Trealla runtime is not ready" }
        require(sourcePath.isNotBlank()) { "Prolog source path is required" }
        bridge.consult(sourcePath)
    }

    override fun close() {
        if (state == State.CLOSED) return
        if (state == State.READY) bridge.shutdown()
        state = State.CLOSED
    }
}
