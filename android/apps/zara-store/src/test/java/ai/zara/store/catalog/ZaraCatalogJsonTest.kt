package ai.zara.store.catalog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraCatalogJsonTest {
    @Test
    fun parsesValidCatalogAndFeedsExactIdentityOverlay() {
        val document = ZaraCatalogJson.parseUntrusted(validCatalog())

        assertEquals("ZARA-CATALOG/1", document.schema)
        assertEquals("Zara", document.repository.name)
        assertEquals("https://repo.zara.invalid/fdroid/repo", document.repository.fdroidRepo)
        assertEquals(1, document.packages.size)

        val metadata = document.packages.single()
        assertEquals(ZaraPackageKind.ANDROID_PLUGIN, metadata.kind)
        assertEquals(setOf("ZARA-ANDROID-PLUGIN/1"), metadata.protocols)

        val identity = FdroidPackageIdentity(
            packageName = metadata.packageName,
            versionCode = metadata.versionCode,
            apkSha256 = metadata.apkSha256,
            signerSha256 = metadata.acceptedSignerSha256.single(),
        )
        assertTrue(ZaraCatalogOverlay.merge(identity, metadata) is CatalogOverlayResult.Enriched)
    }

    @Test
    fun rejectsUnknownCatalogMajor() {
        assertThrows(IllegalArgumentException::class.java) {
            ZaraCatalogJson.parseUntrusted(validCatalog().replace("ZARA-CATALOG/1", "ZARA-CATALOG/2"))
        }
    }

    @Test
    fun rejectsDuplicatePackageVersionIdentity() {
        val packageJson = validPackage()
        assertThrows(IllegalArgumentException::class.java) {
            ZaraCatalogJson.parseUntrusted(
                catalogWithPackages("$packageJson,$packageJson"),
            )
        }
    }

    @Test
    fun rejectsFractionalVersionCodeInsteadOfTruncatingIt() {
        assertThrows(IllegalArgumentException::class.java) {
            ZaraCatalogJson.parseUntrusted(validCatalog().replace("\"version_code\": 7", "\"version_code\": 7.5"))
        }
    }

    @Test
    fun rejectsHttpRepositoryUrls() {
        assertThrows(IllegalArgumentException::class.java) {
            ZaraCatalogJson.parseUntrusted(
                validCatalog().replace(
                    "https://repo.zara.invalid/fdroid/repo",
                    "http://repo.zara.invalid/fdroid/repo",
                ),
            )
        }
    }

    @Test
    fun rejectsSignerThatIsNotSha256() {
        assertThrows(IllegalArgumentException::class.java) {
            ZaraCatalogJson.parseUntrusted(validCatalog().replace(sha('b'), "not-a-digest"))
        }
    }

    @Test
    fun rejectsAndroidPluginWithoutProtocolDeclaration() {
        assertThrows(IllegalArgumentException::class.java) {
            ZaraCatalogJson.parseUntrusted(
                validCatalog().replace(
                    "\"protocols\": [\"ZARA-ANDROID-PLUGIN/1\"],",
                    "\"protocols\": [],",
                ),
            )
        }
    }

    @Test
    fun rejectsDuplicateCapabilities() {
        assertThrows(IllegalArgumentException::class.java) {
            ZaraCatalogJson.parseUntrusted(
                validCatalog().replace(
                    "\"capabilities\": [\"notes.read\", \"notes.write\"]",
                    "\"capabilities\": [\"notes.read\", \"notes.read\"]",
                ),
            )
        }
    }

    @Test
    fun rejectsCatalogLargerThanBoundBeforeParsing() {
        val oversized = " ".repeat(4 * 1024 * 1024 + 1)
        assertThrows(IllegalArgumentException::class.java) {
            ZaraCatalogJson.parseUntrusted(oversized)
        }
    }

    private fun validCatalog(): String = catalogWithPackages(validPackage())

    private fun catalogWithPackages(packages: String): String =
        """
        {
          "schema": "ZARA-CATALOG/1",
          "generated_at": 1726531200000,
          "repo": {
            "name": "Zara",
            "fdroid_repo": "https://repo.zara.invalid/fdroid/repo",
            "revision": "fixture-1"
          },
          "packages": [$packages]
        }
        """.trimIndent()

    private fun validPackage(): String =
        """
        {
          "package_name": "ai.zara.notes",
          "version_code": 7,
          "apk_sha256": "${sha('a')}",
          "accepted_signer_sha256": ["${sha('b')}"],
          "kind": "android_plugin",
          "source_repo": "https://github.com/lost-rob0t/zara",
          "source_sha": "${"c".repeat(40)}",
          "protocols": ["ZARA-ANDROID-PLUGIN/1"],
          "capabilities": ["notes.read", "notes.write"]
        }
        """.trimIndent()

    private fun sha(char: Char): String = char.toString().repeat(64)
}
