package ai.zara.store.repository

import org.fdroid.index.IndexParser

/**
 * Small Zara-owned seam around F-Droid's alpha index API.
 *
 * Decoding is deliberately not a trust decision. Production repository refresh must feed bytes
 * through F-Droid's signature/hash verification and stream processors before promoting a snapshot
 * to a trusted repository generation.
 */
data class FdroidIndexSnapshot(
    val address: String,
    val timestamp: Long,
    val packageNames: List<String>,
)

object FdroidIndexDecoder {
    fun decodeV2Untrusted(indexJson: String): FdroidIndexSnapshot {
        val index = IndexParser.parseV2(indexJson)
        require(index.repo.address.isNotBlank()) { "F-Droid repository address is required" }
        require(index.repo.timestamp >= 0) { "F-Droid repository timestamp must be non-negative" }
        return FdroidIndexSnapshot(
            address = index.repo.address,
            timestamp = index.repo.timestamp,
            packageNames = index.packages.keys.sorted(),
        )
    }
}
