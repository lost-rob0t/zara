package ai.zara.app.accessibility

import android.accessibilityservice.AccessibilityService
import android.os.Bundle
import android.view.accessibility.AccessibilityEvent
import android.view.accessibility.AccessibilityNodeInfo

sealed interface AccessibilitySelector {
    data class Text(val value: String) : AccessibilitySelector
    data class ViewId(val value: String) : AccessibilitySelector
    data class Description(val value: String) : AccessibilitySelector
}

enum class AccessibilityGlobalAction {
    Back,
    Home,
    Recents,
    Notifications,
}

class ZaraAccessibilityService : AccessibilityService() {
    override fun onServiceConnected() {
        active = this
    }

    override fun onDestroy() {
        if (active === this) active = null
        super.onDestroy()
    }

    override fun onInterrupt() = Unit

    override fun onAccessibilityEvent(event: AccessibilityEvent?) = Unit

    fun click(selector: AccessibilitySelector): Boolean =
        find(selector).firstOrNull { node -> node.isVisibleToUser && node.isClickable }
            ?.performAction(AccessibilityNodeInfo.ACTION_CLICK) == true

    fun setText(selector: AccessibilitySelector, text: String): Boolean {
        require(text.encodeToByteArray().size <= MAX_TEXT_BYTES) { "accessibility text exceeds byte limit" }
        val node = find(selector).firstOrNull { it.isVisibleToUser && it.isEditable } ?: return false
        val arguments = Bundle().apply {
            putCharSequence(AccessibilityNodeInfo.ACTION_ARGUMENT_SET_TEXT_CHARSEQUENCE, text)
        }
        return node.performAction(AccessibilityNodeInfo.ACTION_SET_TEXT, arguments)
    }

    fun scrollForward(selector: AccessibilitySelector): Boolean =
        find(selector).firstOrNull { it.isVisibleToUser && it.isScrollable }
            ?.performAction(AccessibilityNodeInfo.ACTION_SCROLL_FORWARD) == true

    fun global(action: AccessibilityGlobalAction): Boolean = performGlobalAction(
        when (action) {
            AccessibilityGlobalAction.Back -> GLOBAL_ACTION_BACK
            AccessibilityGlobalAction.Home -> GLOBAL_ACTION_HOME
            AccessibilityGlobalAction.Recents -> GLOBAL_ACTION_RECENTS
            AccessibilityGlobalAction.Notifications -> GLOBAL_ACTION_NOTIFICATIONS
        }
    )

    private fun find(selector: AccessibilitySelector): List<AccessibilityNodeInfo> {
        val root = rootInActiveWindow ?: return emptyList()
        return when (selector) {
            is AccessibilitySelector.Text -> root.findAccessibilityNodeInfosByText(selector.value).orEmpty()
            is AccessibilitySelector.ViewId -> root.findAccessibilityNodeInfosByViewId(selector.value).orEmpty()
            is AccessibilitySelector.Description -> {
                val matches = mutableListOf<AccessibilityNodeInfo>()
                collectByDescription(root, selector.value, matches, 0)
                matches
            }
        }
    }

    private fun collectByDescription(
        node: AccessibilityNodeInfo,
        description: String,
        result: MutableList<AccessibilityNodeInfo>,
        depth: Int,
    ) {
        if (depth > MAX_TREE_DEPTH || result.size >= MAX_MATCHES) return
        if (node.contentDescription?.toString() == description) result += node
        for (index in 0 until node.childCount) {
            val child = node.getChild(index) ?: continue
            collectByDescription(child, description, result, depth + 1)
            if (result.size >= MAX_MATCHES) return
        }
    }

    companion object {
        @Volatile
        private var active: ZaraAccessibilityService? = null

        fun connected(): Boolean = active != null

        fun current(): ZaraAccessibilityService? = active

        private const val MAX_TEXT_BYTES = 4 * 1024
        private const val MAX_TREE_DEPTH = 64
        private const val MAX_MATCHES = 64
    }
}
