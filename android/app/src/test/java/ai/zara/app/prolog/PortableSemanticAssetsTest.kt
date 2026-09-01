package ai.zara.app.prolog

import java.nio.file.Files
import java.nio.file.Path
import org.junit.Assert.assertTrue
import org.junit.Test

class PortableSemanticAssetsTest {

    @Test fun `declared portable semantic assets are packaged and nonempty`() {
        PortableSemanticCore.resources.forEach { resource ->
            val path = Path.of("app", "src", "main", "assets", resource)
            assertTrue("missing asset: $resource", Files.isRegularFile(path))
            assertTrue("empty asset: $resource", Files.size(path) > 0L)
        }
    }
}
