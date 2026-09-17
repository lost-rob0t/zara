package ai.zara.app.device

object AndroidAppAliases {
    private val packagesByAlias = mapOf(
        "bixby" to listOf("com.samsung.android.bixby.agent"),
        "browser" to listOf(
            "com.sec.android.app.sbrowser",
            "com.android.chrome",
            "com.brave.browser",
            "org.mozilla.firefox",
        ),
        "youtube" to listOf("com.google.android.youtube"),
        "youtube_revanced" to listOf(
            "app.revanced.android.youtube",
            "app.rvx.android.youtube",
        ),
    )

    val reviewed: Set<String> = packagesByAlias.keys

    fun packageCandidates(alias: String): List<String> =
        packagesByAlias[alias.trim().lowercase()].orEmpty()
}
