package ai.zara.app.donations

import java.io.File
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

data class DonationState(
    val ledger: DonationLedger = DonationLedger(),
    val failure: String? = null,
)

class DonationStore(private val file: File) {
    fun state(): DonationState {
        if (!file.isFile) return DonationState()
        return try {
            DonationState(ledger = DonationLedger.parse(file.readText(Charsets.UTF_8)))
        } catch (error: DonationDocumentException) {
            DonationState(failure = error.message ?: "invalid donation document")
        } catch (error: Exception) {
            DonationState(failure = "cannot read donation document: ${error.javaClass.simpleName}")
        }
    }

    fun importDocument(payload: String): DonationState {
        val ledger = DonationLedger.parse(payload)
        val parent = file.parentFile ?: error("donation store requires a parent directory")
        check(parent.exists() || parent.mkdirs()) { "cannot create donation storage directory" }
        val temporary = File.createTempFile(".donations-", ".json", parent)
        try {
            temporary.writeText(payload.trimEnd() + "\n", Charsets.UTF_8)
            try {
                Files.move(
                    temporary.toPath(),
                    file.toPath(),
                    StandardCopyOption.ATOMIC_MOVE,
                    StandardCopyOption.REPLACE_EXISTING,
                )
            } catch (_: AtomicMoveNotSupportedException) {
                Files.move(
                    temporary.toPath(),
                    file.toPath(),
                    StandardCopyOption.REPLACE_EXISTING,
                )
            }
        } finally {
            temporary.delete()
        }
        return DonationState(ledger = ledger)
    }
}
