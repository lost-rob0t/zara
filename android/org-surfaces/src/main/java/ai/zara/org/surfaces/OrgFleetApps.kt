package ai.zara.org.surfaces

object OrgFleetApps {
    val flagship = "ai.zara.org.app"
    val siblings: List<String> = listOf(
        flagship,
        "ai.zara.org.editor",
        "ai.zara.org.todo",
        "ai.zara.org.reminder",
        "ai.zara.org.timer",
        "ai.zara.org.roam",
        "ai.zara.org.graph",
        "ai.zara.org.home",
    )

    fun installed(installedPackages: Set<String>, exclude: String? = null): List<String> =
        siblings.filter { it != exclude && it in installedPackages }

    fun labels(): Map<String, String> = mapOf(
        flagship to "Org",
        "ai.zara.org.editor" to "Editor",
        "ai.zara.org.todo" to "Todo",
        "ai.zara.org.reminder" to "Reminder",
        "ai.zara.org.timer" to "Timer",
        "ai.zara.org.roam" to "Roam",
        "ai.zara.org.graph" to "Graph",
        "ai.zara.org.home" to "Home",
    )
}
