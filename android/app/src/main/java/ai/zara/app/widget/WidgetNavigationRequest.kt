package ai.zara.app.widget

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import ai.zara.app.MainActivity
import ai.zara.app.ui.AppRoute
import java.util.concurrent.atomic.AtomicReference

/** One-shot, bounded ingress from launcher widgets into the canonical AppNavigation owner. */
object WidgetNavigationRequest {
    private val pending = AtomicReference<AppRoute?>(null)

    fun request(route: AppRoute) {
        pending.set(route)
    }

    fun peek(): AppRoute? = pending.get()

    fun consume(expected: AppRoute? = null): AppRoute? {
        if (expected == null) return pending.getAndSet(null)
        return if (pending.compareAndSet(expected, null)) expected else null
    }
}

private fun WidgetRoute.toAppRoute(): AppRoute = when (this) {
    WidgetRoute.CHAT -> AppRoute.Chat
    WidgetRoute.VOICE -> AppRoute.Voice
    WidgetRoute.LOGIC -> AppRoute.Logic
    WidgetRoute.REMOTE -> AppRoute.Runtime
    WidgetRoute.DIAGNOSTICS -> AppRoute.Diagnostics
    WidgetRoute.THEMES -> AppRoute.Appearance
}

class WidgetRouteReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        val requested = intent.getStringExtra(EXTRA_ROUTE)?.trim()?.lowercase() ?: return
        val route = WidgetRoute.entries.firstOrNull { it.atom == requested } ?: return
        WidgetNavigationRequest.request(route.toAppRoute())
        context.startActivity(
            Intent(context, MainActivity::class.java).apply {
                addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TASK)
            },
        )
    }

    companion object {
        const val EXTRA_ROUTE = "ai.zara.app.widget.ROUTE"
    }
}
