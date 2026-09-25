package ai.zara.org.app

import ai.zara.org.surfaces.DailySurface
import ai.zara.org.surfaces.EditorSurface
import ai.zara.org.surfaces.GraphSurface
import ai.zara.org.surfaces.HomeSurface
import ai.zara.org.surfaces.OrgSurfaceTab
import ai.zara.org.surfaces.OrgTheme
import ai.zara.org.surfaces.OrgWorkspaceScreen
import ai.zara.org.surfaces.RemindersSurface
import ai.zara.org.surfaces.RoamSurface
import ai.zara.org.surfaces.TimersSurface
import ai.zara.org.surfaces.TodoSurface
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            OrgTheme {
                OrgWorkspaceScreen(
                    title = "Org",
                    tabs = listOf(
                        OrgSurfaceTab("Todo") { model -> TodoSurface(model) },
                        OrgSurfaceTab("Roam") { model -> RoamSurface(model) },
                        OrgSurfaceTab("Daily") { model -> DailySurface(model) },
                        OrgSurfaceTab("Reminders") { model -> RemindersSurface(model) },
                        OrgSurfaceTab("Timers") { model -> TimersSurface(model) },
                        OrgSurfaceTab("Graph") { model -> GraphSurface(model) },
                        OrgSurfaceTab("Editor") { model -> EditorSurface(model) },
                        OrgSurfaceTab("Home") { model -> HomeSurface(model) },
                    ),
                )
            }
        }
    }
}
