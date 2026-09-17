package ai.zara.app.integration

import ai.zara.app.integration.accessibility.AccessibilityBackend
import ai.zara.app.integration.accessibility.ZaraAccessibilityService
import ai.zara.app.integration.assist.AssistantContextBackend
import ai.zara.app.integration.ime.ImeBackend
import ai.zara.app.integration.ime.ZaraInputMethodService
import ai.zara.app.integration.notification.NotificationBackend
import ai.zara.app.integration.notification.ZaraNotificationListenerService
import android.app.assist.AssistContent
import android.app.assist.AssistStructure
import android.content.Context
import android.graphics.Bitmap
import android.os.Bundle
import java.io.File
import java.util.concurrent.CompletableFuture

class AndroidIntegrationRuntime(context: Context) : AutoCloseable {
    private val appContext = context.applicationContext
    private val authorityPolicy = AndroidAuthorityPolicy.fromWorkspace(
        File(appContext.filesDir, "prolog-workspace"),
    )
    private val accessibility = AccessibilityBackend()
    private val notifications = NotificationBackend()
    private val ime = ImeBackend()
    private val assist = AssistantContextBackend()

    private val actor = AndroidIntegrationActor(
        policy = authorityPolicy,
        backends = listOf(
            RootShellBackend(),
            DeviceOwnerBackend(appContext),
            accessibility,
            notifications,
            ime,
            assist,
            RawIntentBackend(appContext),
            AppShellBackend(),
        ),
    )

    fun authority(): AndroidAuthoritySnapshot = actor.authority()

    fun backendStatus(): List<AndroidBackendStatus> = actor.backendStatus()

    fun execute(request: AndroidOperationRequest): CompletableFuture<AndroidOperationResult> =
        actor.execute(request)

    fun attachAccessibility(service: ZaraAccessibilityService) = accessibility.attach(service)

    fun detachAccessibility(service: ZaraAccessibilityService) = accessibility.detach(service)

    fun attachNotifications(service: ZaraNotificationListenerService) = notifications.attach(service)

    fun detachNotifications(service: ZaraNotificationListenerService) = notifications.detach(service)

    fun attachIme(service: ZaraInputMethodService) = ime.attach(service)

    fun detachIme(service: ZaraInputMethodService) = ime.detach(service)

    fun captureAssistantContext(
        data: Bundle?,
        structure: AssistStructure?,
        content: AssistContent?,
    ) = assist.capture(data, structure, content)

    fun captureAssistantScreenshot(bitmap: Bitmap?) = assist.captureScreenshot(bitmap)

    fun clearAssistantContext() = assist.clear()

    override fun close() = actor.close()
}
