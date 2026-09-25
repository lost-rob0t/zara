package ai.zara.ui.continuity

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.IOException
import java.nio.charset.StandardCharsets

/**
 * Phone -> Wear auto-provisioning envelope over the Wear Data Layer.
 *
 * The provision payload is presentation data only: the paired phone's display
 * name plus the canonical pure-symbolic conversation projection. It never
 * carries credentials, endpoints, transcripts, or authority grants.
 */
data class WearPhoneProvision(
    val phoneName: String,
    val snapshot: SymbolicConversationEdgeSnapshot?,
)

object WearCompanionContract {
    const val CAPABILITY_WATCH = "zara_watch"
    const val CAPABILITY_PHONE = "zara_phone"
    const val PATH_WATCH_HELLO = "/zara/watch/hello"
    const val PATH_PHONE_PROVISION = "/zara/phone/provision"
    const val MAX_PHONE_NAME_CHARS = 64
    const val MAX_PROVISION_WIRE_BYTES = 36 * 1024

    private const val MAGIC = "ZARA-WEAR-PROVISION/1"

    fun encodeProvision(provision: WearPhoneProvision): ByteArray {
        requireBoundedPhoneName(provision.phoneName)
        val snapshot = provision.snapshot
        snapshot?.assertPureSymbolic()
        val snapshotWire = snapshot?.let(SymbolicConversationEdgeCodec::encode) ?: ByteArray(0)
        val bytes = ByteArrayOutputStream()
        DataOutputStream(bytes).use { output ->
            output.write(MAGIC.toByteArray(StandardCharsets.US_ASCII))
            output.writeString(provision.phoneName, MAX_PHONE_NAME_CHARS)
            output.writeInt(snapshotWire.size)
            output.write(snapshotWire)
            output.writeBoolean(snapshot != null)
        }
        return bytes.toByteArray().also { encoded ->
            require(encoded.size in 1..MAX_PROVISION_WIRE_BYTES) {
                "wear phone provision exceeds $MAX_PROVISION_WIRE_BYTES wire bytes"
            }
        }
    }

    fun decodeProvision(encoded: ByteArray): WearPhoneProvision {
        require(encoded.size in 1..MAX_PROVISION_WIRE_BYTES) {
            "wear phone provision wire size is invalid"
        }
        try {
            DataInputStream(ByteArrayInputStream(encoded)).use { input ->
                val magic = ByteArray(MAGIC.length)
                input.readFully(magic)
                require(String(magic, StandardCharsets.US_ASCII) == MAGIC) {
                    "wear phone provision magic is invalid"
                }
                val phoneName = input.readString(MAX_PHONE_NAME_CHARS)
                val snapshotSize = input.readInt()
                require(snapshotSize in 0..SymbolicConversationEdgeCodec.MAX_WIRE_BYTES) {
                    "wear phone provision snapshot size is invalid"
                }
                val snapshotWire = ByteArray(snapshotSize)
                input.readFully(snapshotWire)
                val snapshotPresent = input.readBoolean()
                require(input.read() == -1) { "wear phone provision contains trailing bytes" }
                require(snapshotPresent || snapshotSize == 0) {
                    "wear phone provision snapshot presence flag is invalid"
                }
                return WearPhoneProvision(
                    phoneName = phoneName,
                    snapshot = if (snapshotPresent) {
                        SymbolicConversationEdgeCodec.decode(snapshotWire)
                    } else {
                        null
                    },
                )
            }
        } catch (error: IOException) {
            throw IllegalArgumentException(
                "wear phone provision is truncated or malformed",
                error,
            )
        }
    }

    private fun requireBoundedPhoneName(value: String) {
        require(value.isNotBlank()) { "phoneName must not be blank" }
        require(value.length <= MAX_PHONE_NAME_CHARS) {
            "phoneName exceeds $MAX_PHONE_NAME_CHARS characters"
        }
        require(value.none(Char::isISOControl)) { "phoneName contains control characters" }
    }

    private fun DataOutputStream.writeString(value: String, maxChars: Int) {
        require(value.length <= maxChars)
        val encoded = value.toByteArray(StandardCharsets.UTF_8)
        require(encoded.size <= maxChars * 4)
        writeInt(encoded.size)
        write(encoded)
    }

    private fun DataInputStream.readString(maxChars: Int): String {
        val size = readInt()
        require(size in 0..(maxChars * 4)) { "wear phone provision string byte length is invalid" }
        val encoded = ByteArray(size)
        readFully(encoded)
        return String(encoded, StandardCharsets.UTF_8).also { value ->
            require(value.length <= maxChars) { "wear phone provision string exceeds character bound" }
        }
    }
}
