package ai.zara.app.prolog

object PortableSemanticCore {
    const val contractVersion = "ZARA-SEMANTIC/1"
    const val coreAssetPath = "prolog/portable/semantic_core.pl"
    const val fixtureAssetPath = "prolog/portable/semantic_fixtures.json"

    val resources: List<String> = listOf(
        coreAssetPath,
        fixtureAssetPath
    )
}
