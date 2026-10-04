package ai.zara.org.graph

import ai.zara.org.surfaces.OrgSurfaceTab
import ai.zara.org.surfaces.OrgTheme
import ai.zara.org.surfaces.OrgWorkspaceScreen
import ai.zara.org.surfaces.GraphSurface
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            OrgTheme {
                OrgWorkspaceScreen(
                    title = "Org Graph",
                    tabs = listOf(
                        OrgSurfaceTab("Graph") { model -> GraphSurface(model) },
                    ),
                )
            }
        }
    }
}
