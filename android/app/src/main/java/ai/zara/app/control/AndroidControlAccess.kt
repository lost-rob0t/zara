package ai.zara.app.control

import ai.zara.app.accessibility.ZaraAccessibilityService
import android.app.AppOpsManager
import android.app.role.RoleManager
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Process
import android.provider.Settings

/** Android privileges that cannot be silently granted by Zara. */
enum class AndroidControlAccess {
    Accessibility,
    AssistantRole,
    UsageAccess,
    WriteSystemSettings,
    DrawOverlays,
}

data class AndroidControlAccessState(
    val access: AndroidControlAccess,
    val granted: Boolean,
)

class AndroidControlAccessBroker(context: Context) {
    private val appContext = context.applicationContext

    fun snapshot(): List<AndroidControlAccessState> =
        AndroidControlAccess.entries.map { AndroidControlAccessState(it, isGranted(it)) }

    fun isGranted(access: AndroidControlAccess): Boolean = when (access) {
        AndroidControlAccess.Accessibility -> accessibilityEnabled()
        AndroidControlAccess.AssistantRole -> {
            val manager = appContext.getSystemService(RoleManager::class.java)
            manager != null && manager.isRoleAvailable(RoleManager.ROLE_ASSISTANT) &&
                manager.isRoleHeld(RoleManager.ROLE_ASSISTANT)
        }
        AndroidControlAccess.UsageAccess -> {
            val manager = appContext.getSystemService(AppOpsManager::class.java) ?: return false
            manager.checkOpNoThrow(
                AppOpsManager.OPSTR_GET_USAGE_STATS,
                Process.myUid(),
                appContext.packageName,
            ) == AppOpsManager.MODE_ALLOWED
        }
        AndroidControlAccess.WriteSystemSettings -> Settings.System.canWrite(appContext)
        AndroidControlAccess.DrawOverlays -> Settings.canDrawOverlays(appContext)
    }

    /** Returns the platform-owned screen/request UI; Zara never fabricates a grant. */
    fun requestIntent(access: AndroidControlAccess): Intent? = when (access) {
        AndroidControlAccess.Accessibility -> Intent(Settings.ACTION_ACCESSIBILITY_SETTINGS)
        AndroidControlAccess.AssistantRole -> {
            val manager = appContext.getSystemService(RoleManager::class.java) ?: return null
            if (!manager.isRoleAvailable(RoleManager.ROLE_ASSISTANT)) null
            else manager.createRequestRoleIntent(RoleManager.ROLE_ASSISTANT)
        }
        AndroidControlAccess.UsageAccess -> Intent(
            Settings.ACTION_USAGE_ACCESS_SETTINGS,
            Uri.parse("package:${appContext.packageName}"),
        )
        AndroidControlAccess.WriteSystemSettings -> Intent(
            Settings.ACTION_MANAGE_WRITE_SETTINGS,
            Uri.parse("package:${appContext.packageName}"),
        )
        AndroidControlAccess.DrawOverlays -> Intent(
            Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
            Uri.parse("package:${appContext.packageName}"),
        )
    }?.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)

    private fun accessibilityEnabled(): Boolean {
        if (Settings.Secure.getInt(
                appContext.contentResolver,
                Settings.Secure.ACCESSIBILITY_ENABLED,
                0,
            ) != 1
        ) return false
        val expected = ComponentName(appContext, ZaraAccessibilityService::class.java)
            .flattenToString()
        return Settings.Secure.getString(
            appContext.contentResolver,
            Settings.Secure.ENABLED_ACCESSIBILITY_SERVICES,
        ).orEmpty().split(':').any { it.equals(expected, ignoreCase = true) }
    }
}
