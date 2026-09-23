package ai.zara.app.auth

object PairingCode {
    const val LENGTH = 16
    const val GROUP = 3

    fun normalize(raw: String): String {
        val letters = StringBuilder(LENGTH)
        raw.forEach { character ->
            when {
                character == '-' || character.isWhitespace() -> Unit
                character in 'a'..'z' -> letters.append(character.uppercaseChar())
                character in 'A'..'Z' -> letters.append(character)
                else -> throw IllegalArgumentException("pairing code must contain letters only")
            }
        }
        require(letters.length == LENGTH) { "pairing code must contain exactly 16 letters" }
        return letters.toString()
    }

    fun render(raw: String): String =
        normalize(raw)
            .chunked(GROUP)
            .joinToString("-")
}
