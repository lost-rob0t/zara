package ai.zara.app.launcher

import ai.zara.app.prolog.AndroidActionMemory
import ai.zara.app.prolog.AndroidAppMemory
import ai.zara.app.prolog.AndroidKnowledgeBase

data class LauncherAppRecord(
    val packageName: String,
    val activityName: String,
    val label: String,
    val profile: String,
)

interface LauncherAppSource {
    fun listLaunchableApps(): List<LauncherAppRecord>
}

interface LauncherAppStarter {
    fun start(app: LauncherAppRecord)
}

data class LauncherLaunchResult(
    val success: Boolean,
    val error: String? = null,
)

class LauncherCatalog(
    private val source: LauncherAppSource,
    private val starter: LauncherAppStarter,
    private val knowledgeBase: AndroidKnowledgeBase,
) {
    fun refresh(): List<LauncherAppRecord> {
        val apps = source.listLaunchableApps()
            .distinctBy { Triple(it.packageName, it.activityName, it.profile) }
            .sortedWith(
                compareBy<LauncherAppRecord>(
                    { it.label.lowercase() },
                    { it.packageName },
                    { it.activityName },
                    { it.profile },
                )
            )
        apps.forEach { app ->
            knowledgeBase.rememberApp(
                AndroidAppMemory(
                    packageName = app.packageName,
                    activityName = app.activityName,
                    label = app.label,
                    profile = app.profile,
                )
            )
        }
        return apps
    }

    fun launch(app: LauncherAppRecord): LauncherLaunchResult {
        val target = "${app.packageName}/${app.activityName}"
        val actionId = "launch:$target"
        knowledgeBase.rememberAction(
            AndroidActionMemory(
                id = actionId,
                kind = "launch_app",
                backend = "launcher_apps",
                target = target,
                source = "launcher_discovery",
            )
        )
        return try {
            starter.start(app)
            knowledgeBase.rememberObservation(actionId, "success", "launcher_apps", "started")
            LauncherLaunchResult(success = true)
        } catch (error: Throwable) {
            val detail = error.message?.take(256) ?: error::class.java.simpleName
            knowledgeBase.rememberObservation(actionId, "failure", "launcher_apps", detail)
            LauncherLaunchResult(success = false, error = detail)
        }
    }
}
