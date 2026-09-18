package ai.zara.app.policy

object PolicyWire {
    fun query(text: String): String {
        require(text.length <= 32768) { "Policy text is too large" }
        val codes = text.codePoints().toArray()
        require(codes.all(::isScalar)) { "Policy text contains an invalid Unicode scalar" }
        return "zara_policy:advise_codes([${codes.joinToString(",")}], Result)"
    }

    fun decode(terms: List<String>, generation: Long): PolicyAdvice {
        require(terms.size == 1) { "Policy evaluation did not return one report" }
        val text = terms.single().trim()
        require(text.length in 3..65536 && text.first() == '[' && text.last() == ']') {
            "Malformed policy report"
        }
        val fields = text.substring(1, text.length - 1).split(',')
        require(fields.size in 1..8193) { "Policy report is too large" }
        val codes = fields.map { field ->
            val number = field.trim()
            require(number.isNotEmpty() && number.length <= 7 && number.all { it in '0'..'9' }) {
                "Policy report contains a non-integer value"
            }
            number.toInt()
        }
        val enabled = codes.first()
        require(enabled == 0 || enabled == 1) { "Unknown policy status" }
        require(enabled != 0 || codes.size == 1) { "Disabled policy contains guidance" }
        val guidance = StringBuilder()
        for (code in codes.drop(1)) {
            require(isScalar(code) && (code >= 32 || code == 9 || code == 10)) {
                "Invalid policy guidance character"
            }
            guidance.appendCodePoint(code)
        }
        return PolicyAdvice(guidance.toString(), generation, enabled == 1)
    }

    private fun isScalar(code: Int): Boolean = code in 0..0x10ffff && code !in 0xd800..0xdfff
}
