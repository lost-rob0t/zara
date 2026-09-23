package ai.zara.app.widget

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import ai.zara.app.MainActivity
import ai.zara.app.ui.AppRoute

internal fun consumeWidgetRoute(intent: Intent?): AppRoute? {
    val requested = intent?.getStringExtra(WidgetRouteReceiver.EXTRA_ROUTE)?.trim()?.lowercase() ?: return null
    val route = WidgetRoute.entries.firstOrNull { it.atom == requested }
        ?: WidgetRoute.fromAtom(requested)
        ?: return null
    intent.removeExtra(WidgetRouteReceiver.EXTRA_ROUTE)
    return route.appRoute
}

class WidgetRouteReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        val requested = intent.getStringExtra(EXTRA_ROUTE)?.trim()?.lowercase() ?: return
        val route = WidgetRoute.entries.firstOrNull { it.atom == requested }
            ?: WidgetRoute.fromAtom(requested)
            ?: return
        context.startActivity(
            Intent(context, MainActivity::class.java).apply {
                addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_SINGLE_TOP)
                putExtra(EXTRA_ROUTE, route.atom)
            },
        )
    }

    companion object {
        const val EXTRA_ROUTE = "ai.zara.app.widget.ROUTE"
    }
}
