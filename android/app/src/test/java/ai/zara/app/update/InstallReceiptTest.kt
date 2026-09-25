package ai.zara.app.update

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class InstallReceiptTest {
    @get:Rule
    val temporary = TemporaryFolder()

    private fun provenance(
        sourceSha: String = "a".repeat(40),
        version: String = "0.2.3",
        versionCode: Long = 5,
        sha256: String = "b".repeat(64),
    ): UpdateApkProvenance = object : UpdateApkProvenance {
        override val release = UpdateRelease(
            version = version,
            sourceSha = sourceSha,
            apkUrl = "https://github.com/lost-rob0t/zara/releases/download/v$version/zara-android-$version.apk",
            sha256 = sha256,
        )
        override val versionName = version
        override val versionCode = versionCode
    }

    @Test
    fun `receipt round trips exact package installer authority`() {
        val receipt = InstallReceipt.create(
            sessionId = 42,
            provenance = provenance(),
            apkFileName = "zara-0.2.3.apk",
            nonce = "c".repeat(64),
        )
        val store = InstallReceiptStore(temporary.newFolder("receipt"))

        assertTrue(store.write(receipt).isSuccess)
        val restored = store.read().getOrThrow()
        assertEquals(receipt, restored)
        assertTrue(receipt.matches(receipt.identity()))
    }

    @Test
    fun `stale callback identity cannot match active receipt`() {
        val receipt = InstallReceipt.create(
            sessionId = 42,
            provenance = provenance(),
            apkFileName = "zara-0.2.3.apk",
            nonce = "c".repeat(64),
        )
        val identity = receipt.identity()

        val stale = listOf(
            identity.copy(sessionId = 41),
            identity.copy(nonce = "d".repeat(64)),
            identity.copy(sourceSha = "e".repeat(40)),
            identity.copy(versionName = "0.2.4"),
            identity.copy(versionCode = 6),
            identity.copy(apkSha256 = "f".repeat(64)),
        )
        assertTrue(stale.all { !receipt.matches(it) })
    }

    @Test
    fun `corrupt or ambiguous durable receipt fails closed`() {
        val directory = temporary.newFolder("corrupt")
        val store = InstallReceiptStore(directory)
        val receipt = InstallReceipt.create(
            sessionId = 42,
            provenance = provenance(),
            apkFileName = "zara-0.2.3.apk",
            nonce = "c".repeat(64),
        )
        assertTrue(store.write(receipt).isSuccess)

        val file = directory.resolve("active-install.receipt")
        file.appendText("source_sha=dddddddddddddddddddddddddddddddddddddddd\n")
        assertTrue(store.read().isFailure)
    }

    @Test
    fun `receipt refuses path substitution and candidate mismatch`() {
        assertTrue(
            runCatching {
                InstallReceipt.create(
                    sessionId = 42,
                    provenance = provenance(),
                    apkFileName = "../zara.apk",
                    nonce = "c".repeat(64),
                )
            }.isFailure
        )
        assertTrue(
            InstallReceipt.parse(
                InstallReceipt.create(
                    sessionId = 42,
                    provenance = provenance(),
                    apkFileName = "zara-0.2.3.apk",
                    nonce = "c".repeat(64),
                ).encode().replace("version_code=5", "version_code=0")
            ).isFailure
        )
    }

    @Test
    fun `clearing receipt removes restart authority`() {
        val store = InstallReceiptStore(temporary.newFolder("clear"))
        val receipt = InstallReceipt.create(
            sessionId = 42,
            provenance = provenance(),
            apkFileName = "zara-0.2.3.apk",
            nonce = "c".repeat(64),
        )
        assertTrue(store.write(receipt).isSuccess)
        assertTrue(store.clear())
        assertFalse(store.read().getOrThrow() != null)
    }
}
