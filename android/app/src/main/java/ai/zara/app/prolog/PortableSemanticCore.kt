package ai.zara.app.prolog

object PortableSemanticCore {
    const val contractVersion = "ZARA-SEMANTIC/1"
    const val coreAssetPath = "prolog/portable/semantic_core.pl"
    const val fixtureAssetPath = "prolog/portable/semantic_fixtures.json"

    val resolverDependencies: List<String> = listOf(
        "prolog/shared/modules/intent_frames.pl",
        "prolog/shared/modules/normalizer.pl",
        "prolog/shared/kb/intents.pl"
    )

    val resources: List<String> = listOf(
        coreAssetPath,
        fixtureAssetPath
    ) + resolverDependencies
}
