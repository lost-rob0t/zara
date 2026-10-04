package ai.zara.app.samsunghealth

import java.io.ByteArrayInputStream
import org.bouncycastle.openpgp.PGPEncryptedDataList
import org.bouncycastle.openpgp.PGPObjectFactory
import org.bouncycastle.openpgp.PGPUtil
import org.bouncycastle.openpgp.operator.jcajce.JcaKeyFingerprintCalculator
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

class OpenPgpHealthExporterTest {
    @Test
    fun encryptsOnePayloadForMultipleGpgRecipients() {
        val plaintext = "health_db_version(1).\nhealth_goal(owner,steps,10000,steps,daily,1).\n"
            .toByteArray()
        val encrypted = OpenPgpHealthExporter().encrypt(
            plaintext,
            listOf(PUBLIC_KEY_ONE.toByteArray(), PUBLIC_KEY_TWO.toByteArray()),
        )

        assertFalse(encrypted.toString(Charsets.ISO_8859_1).contains("health_goal"))
        val objects = PGPObjectFactory(
            PGPUtil.getDecoderStream(ByteArrayInputStream(encrypted)),
            JcaKeyFingerprintCalculator(),
        )
        val recipients = objects.nextObject() as PGPEncryptedDataList
        assertEquals(2, recipients.encryptedDataObjects.asSequence().count())
    }

    @Test(expected = IllegalArgumentException::class)
    fun refusesToExportWithoutRecipients() {
        OpenPgpHealthExporter().encrypt("health_db_version(1).".toByteArray(), emptyList())
    }

    private companion object {
        val PUBLIC_KEY_ONE = """
            -----BEGIN PGP PUBLIC KEY BLOCK-----

            mI0EasGKeAEEALm2lPWH8Jmida4mbAe2z+x3im5qTAUlRT89xRAptqPWr2bx9Fbi
            wIJeHJdbTrkg100nnUvWT+XJ8L82g4TTV53z3dTlJOVLSkwadYZEkdApquO/ohwG
            bYgBHNQaouwCu8yhpnq60+0Bl5VRCKPeqwu0pO1Hca8WilFZooR6TGYdABEBAAG0
            J1phcmEgSGVhbHRoIFRlc3QgT25lIDxvbmVAZXhhbXBsZS50ZXN0PojOBBMBCgA4
            FiEEiLNV9UNyDrbBgueoevalabY6E+cFAmrBingCGw0FCwkIBwIGFQoJCAsCBBYC
            AwECHgECF4AACgkQevalabY6E+c8KAP+OwXeADpU8imyH+0QIiSdSRmrMtY6N5/J
            BY2JglQ89DI1WyvSmTwEi6QEOWKO4yiIqvuru434dWBCkSt8+oJom8BGgs+TMxSQ
            UD7rmNa7xxgHBW8SARgB3xApHpY59UkywnYNQeNssot1batohsgY9j+401UY3qsr
            MRML9+lNwfc=
            =nGS4
            -----END PGP PUBLIC KEY BLOCK-----
        """.trimIndent()

        val PUBLIC_KEY_TWO = """
            -----BEGIN PGP PUBLIC KEY BLOCK-----

            mI0EasGKeAEEALUb9Vz+jiKduIoTOBssnwFCjKni8yYYEIVPrETfpOWqQnsPl2Gm
            4LUleJszekxTzLOy+I0+exJspHb31Ovdz0sCiSbC4r8FD+8SD7pPXLWsHAcvTI5m
            LGKuOTb4t32e4LOdvaQ7XLIwgqMwkhv4G6fDVkL01Iwzg64Uw7oPRvOxABEBAAG0
            J1phcmEgSGVhbHRoIFRlc3QgVHdvIDx0d29AZXhhbXBsZS50ZXN0PojOBBMBCgA4
            FiEEDc/A+33hfI0G8WKymAW0utXK+eMFAmrBingCGw0FCwkIBwIGFQoJCAsCBBYC
            AwECHgECF4AACgkQmAW0utXK+eNaVAP+NlHPwT1yv8baqLlKBUt857kOeXApoxmc
            eYSCQbfHAcb61pQTRreHKNBqD/iE2060amObpVCOk+ubH34N6KDWK9IubNqlQfPx
            YUdn8iMuy59NGbamMOjC4Onl+n1Lx5UxYSGnIG/TTx0NnBL9v5vTBYWpMzcsPFGQ
            kKnbZXKqhY0=
            =HyBO
            -----END PGP PUBLIC KEY BLOCK-----
        """.trimIndent()
    }
}
