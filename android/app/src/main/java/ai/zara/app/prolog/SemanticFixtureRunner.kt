package ai.zara.app.prolog

data class SemanticFixture(
    val id: String,
    val query: String
)

fun interface SemanticRuntime {
    fun evaluate(fixture: SemanticFixture): SemanticResult
}

data class SemanticParityResult(
    val fixtureId: String,
    val equivalent: Boolean,
    val linux: SemanticResult?,
    val android: SemanticResult?,
    val failure: String? = null
)

class SemanticFixtureRunner(
    private val linuxRuntime: SemanticRuntime,
    private val androidRuntime: SemanticRuntime
) {
    fun run(fixtures: List<SemanticFixture>): List<SemanticParityResult> =
        fixtures.map(::runFixture)

    private fun runFixture(fixture: SemanticFixture): SemanticParityResult {
        val linux = try {
            linuxRuntime.evaluate(fixture)
        } catch (_: Exception) {
            return SemanticParityResult(
                fixtureId = fixture.id,
                equivalent = false,
                linux = null,
                android = null,
                failure = "linux runtime failed"
            )
        }

        val android = try {
            androidRuntime.evaluate(fixture)
        } catch (_: Exception) {
            return SemanticParityResult(
                fixtureId = fixture.id,
                equivalent = false,
                linux = SemanticResultComparator.normalize(linux),
                android = null,
                failure = "android runtime failed"
            )
        }

        val normalizedLinux = SemanticResultComparator.normalize(linux)
        val normalizedAndroid = SemanticResultComparator.normalize(android)
        return SemanticParityResult(
            fixtureId = fixture.id,
            equivalent = SemanticResultComparator.equivalent(normalizedLinux, normalizedAndroid),
            linux = normalizedLinux,
            android = normalizedAndroid
        )
    }
}
