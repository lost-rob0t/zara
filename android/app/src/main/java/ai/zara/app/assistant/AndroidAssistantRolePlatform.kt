package ai.zara.app.assistant

import android.app.role.RoleManager
import android.content.Context
import android.content.Intent

class AndroidAssistantRolePlatform(context: Context) : AssistantRolePlatform {
    private val roleManager: RoleManager = context.getSystemService(RoleManager::class.java)

    override fun isAvailable(): Boolean =
        roleManager.isRoleAvailable(RoleManager.ROLE_ASSISTANT)

    override fun isHeld(): Boolean =
        isAvailable() && roleManager.isRoleHeld(RoleManager.ROLE_ASSISTANT)

    fun createUserRequestIntent(): Intent? {
        if (!isAvailable() || isHeld()) return null
        return roleManager.createRequestRoleIntent(RoleManager.ROLE_ASSISTANT)
    }
}
