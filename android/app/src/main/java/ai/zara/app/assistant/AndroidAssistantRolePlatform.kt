package ai.zara.app.assistant

import android.app.role.RoleManager
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.provider.Settings
import android.service.voice.VoiceInteractionService

class AndroidAssistantRolePlatform(
    private val context: Context,
) : AssistantRolePlatform {
    private val roleManager: RoleManager = context.getSystemService(RoleManager::class.java)

    override fun isAvailable(): Boolean =
        roleManager.isRoleAvailable(RoleManager.ROLE_ASSISTANT)

    override fun isHeld(): Boolean =
        isAvailable() &&
            roleManager.isRoleHeld(RoleManager.ROLE_ASSISTANT) &&
            VoiceInteractionService.isActiveService(
                context,
                ComponentName(context, ZaraVoiceInteractionService::class.java),
            )

    fun createUserRequestIntent(): Intent? {
        if (!isAvailable() || isHeld()) return null

        return listOf(
            Settings.ACTION_VOICE_INPUT_SETTINGS,
            Settings.ACTION_MANAGE_DEFAULT_APPS_SETTINGS,
            Settings.ACTION_SETTINGS,
        ).asSequence()
            .map(::Intent)
            .firstOrNull { it.resolveActivity(context.packageManager) != null }
    }
}
