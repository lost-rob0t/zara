package ai.zara.app.ui

import ai.zara.app.MainActivity
import ai.zara.app.conversations.ConversationStore
import android.content.ClipboardManager
import android.content.ClipData
import android.content.Intent
import android.os.SystemClock
import android.view.accessibility.AccessibilityNodeInfo
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Test

class ChatErrorClipboardInstrumentedTest {
    @Test
    fun restoredFailedBubbleCopiesItsExactSavedDetailsAfterLaterSuccess() {
        val instrumentation = InstrumentationRegistry.getInstrumentation()
        val context = instrumentation.targetContext
        val conversation = ConversationStore(File(context.filesDir, "conversations.bin"))
            .state().selectedConversation
        assertNotNull("Installed acceptance must first create its real failed local turn", conversation)
        val turns = conversation!!.turns
        assertTrue("Acceptance must complete a later successful turn", turns.last().success == true)
        val expected = turns.single { it.success == false }.errorDetails
        assertNotNull("Failed turn must retain its snapshot", expected)
        assertTrue(expected!!.startsWith("ZARA-CHAT-ERROR/1\n"))
        assertTrue(expected.toByteArray(Charsets.UTF_8).size <= 8192)
        assertTrue(!expected.contains("zara_diagnostic_private_fixture_1523"))

        context.startActivity(Intent(context, MainActivity::class.java).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK))
        val automation = instrumentation.uiAutomation
        val deadline = SystemClock.uptimeMillis() + 30_000
        val clipboard = context.getSystemService(ClipboardManager::class.java)
        var clicked = false
        while (SystemClock.uptimeMillis() < deadline && !clicked) {
            val root = automation.rootInActiveWindow
            val label = find(root, "Copy error details")
            if (label != null) {
                var action: AccessibilityNodeInfo? = label
                repeat(8) {
                    if (action?.isClickable != true) action = action?.parent
                }
                clipboard.setPrimaryClip(ClipData.newPlainText("Zara acceptance", "zara_clipboard_sentinel_1523"))
                assertEquals("zara_clipboard_sentinel_1523", clipboard.primaryClip?.getItemAt(0)?.text?.toString())
                clicked = action?.performAction(AccessibilityNodeInfo.ACTION_CLICK) == true
            } else {
                findScrollable(root)?.performAction(AccessibilityNodeInfo.ACTION_SCROLL_BACKWARD)
            }
            if (!clicked) SystemClock.sleep(200)
        }
        assertTrue("Restored failed bubble did not expose an operable Copy error details action", clicked)
        var actual: String? = null
        while (SystemClock.uptimeMillis() < deadline && actual != expected) {
            actual = clipboard.primaryClip?.let { clip ->
                if (clip.itemCount == 1) clip.getItemAt(0).coerceToText(context).toString() else null
            }
            if (actual != expected) SystemClock.sleep(100)
        }
        assertEquals("Android clipboard must equal the failed message's saved snapshot", expected, actual)
        var feedback = find(automation.rootInActiveWindow, "Error details copied")
        while (feedback == null && SystemClock.uptimeMillis() < deadline) {
            SystemClock.sleep(100)
            feedback = find(automation.rootInActiveWindow, "Error details copied")
        }
        assertNotNull("Copy success must be visible to accessibility", feedback)
    }

    private fun find(node: AccessibilityNodeInfo?, text: String): AccessibilityNodeInfo? {
        if (node == null) return null
        if (node.text?.toString() == text || node.contentDescription?.toString() == text) return node
        for (index in 0 until node.childCount) find(node.getChild(index), text)?.let { return it }
        return null
    }

    private fun findScrollable(node: AccessibilityNodeInfo?): AccessibilityNodeInfo? {
        if (node == null) return null
        if (node.isScrollable) return node
        for (index in 0 until node.childCount) findScrollable(node.getChild(index))?.let { return it }
        return null
    }
}
