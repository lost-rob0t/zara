package ai.zara.app.auth

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PairingCodeTest {
    @Test
    fun `normalization matches server case-insensitive three-letter grouping contract`() {
        assertEquals("ABCDEFGHIJKLMNOP", PairingCode.normalize("abc-def-ghi-jkl-mno-p"))
        assertEquals("ABCDEFGHIJKLMNOP", PairingCode.normalize("ABC DEF GHI JKL MNO P"))
        assertEquals("ABC-DEF-GHI-JKL-MNO-P", PairingCode.render("abcdefghijklmnop"))
    }

    @Test
    fun `invalid length digit punctuation and non-ascii input fail closed`() {
        listOf(
            "ABC-DEF-GHI-JKL-MNO",
            "ABC-DEF-GHI-JKL-MNO-PQ",
            "ABC-DEF-GHI-JKL-MN0-P",
            "ABC_DEF_GHI_JKL_MNO_P",
            "ÁBC-DEF-GHI-JKL-MNO-P",
        ).forEach { raw ->
            assertThrows(IllegalArgumentException::class.java) {
                PairingCode.normalize(raw)
            }
        }
    }
}
