package ai.zara.org.editor

import ai.zara.org.surfaces.OrgSurfaceTab
import ai.zara.org.surfaces.OrgTheme
import ai.zara.org.surfaces.OrgWorkspaceScreen
import ai.zara.org.surfaces.EditorSurface
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            OrgTheme {
                OrgWorkspaceScreen(
                    title = "Org Editor",
                    tabs = listOf(
                        OrgSurfaceTab("Editor") { model -> EditorSurface(model) },
                    ),
                )
            }
        }
    }
}
