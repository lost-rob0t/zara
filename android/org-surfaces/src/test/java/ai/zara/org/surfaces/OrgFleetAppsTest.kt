package ai.zara.org.surfaces

import org.junit.Assert.assertEquals
import org.junit.Test

class OrgFleetAppsTest {
    @Test fun `installed siblings are filtered by the real package set`() {
        val installed = OrgFleetApps.installed(
            installedPackages = setOf("ai.zara.org.todo", "ai.zara.org.roam", "ai.zara.unrelated"),
        )

        assertEquals(listOf("ai.zara.org.todo", "ai.zara.org.roam"), installed)
    }

    @Test fun `an app never links to itself`() {
        val installed = OrgFleetApps.installed(
            installedPackages = OrgFleetApps.siblings.toSet(),
            exclude = "ai.zara.org.todo",
        )

        assertEquals(OrgFleetApps.siblings.size - 1, installed.size)
        assertEquals(false, "ai.zara.org.todo" in installed)
    }

    @Test fun `every fleet app has a label`() {
        OrgFleetApps.siblings.forEach { packageName ->
            assertEquals(true, OrgFleetApps.labels().containsKey(packageName))
        }
    }
}
