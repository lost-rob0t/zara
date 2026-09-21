package ai.zara.app.donations

import java.math.BigDecimal
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class DonationLedgerTest {
    private val fixture = """
        {
          "version":"ZARA-DONATIONS/1",
          "campaigns":[
            {
              "id":"infra",
              "title":"Infrastructure",
              "goal_usd":"1000.00",
              "raised_usd":"125.50",
              "active":true,
              "wallets":[
                {
                  "chain":"bitcoin",
                  "network":"mainnet",
                  "asset":"BTC",
                  "address":"bc1qexample",
                  "label":"BTC"
                }
              ]
            },
            {
              "id":"research",
              "title":"Research",
              "goal_usd":500,
              "raised_usd":25,
              "active":false,
              "wallets":[]
            }
          ]
        }
    """.trimIndent()

    @Test
    fun aggregatesDeclaredUsdAndWallets() {
        val ledger = DonationLedger.parse(fixture)

        assertEquals(BigDecimal("1500.00"), ledger.totalGoalUsd)
        assertEquals(BigDecimal("150.50"), ledger.totalRaisedUsd)
        assertEquals(BigDecimal("1349.50"), ledger.totalRemainingUsd)
        assertEquals(1, ledger.activeCampaignCount)
        assertEquals("bc1qexample", ledger.campaigns.first().wallets.first().address)
    }

    @Test
    fun missingAmountsDefaultToZero() {
        val ledger = DonationLedger.parse(
            """{"version":"ZARA-DONATIONS/1","campaigns":[{"id":"x","title":"X"}]}""",
        )

        assertEquals(BigDecimal("0.00"), ledger.totalRaisedUsd)
        assertEquals(BigDecimal("0.00"), ledger.totalGoalUsd)
    }

    @Test
    fun rejectsNegativeOrOverPreciseUsdAndDuplicateIds() {
        assertThrows(DonationDocumentException::class.java) {
            DonationLedger.parse(
                """{"version":"ZARA-DONATIONS/1","campaigns":[{"id":"x","title":"X","raised_usd":"-1"}]}""",
            )
        }
        assertThrows(DonationDocumentException::class.java) {
            DonationLedger.parse(
                """{"version":"ZARA-DONATIONS/1","campaigns":[{"id":"x","title":"X","goal_usd":"1.001"}]}""",
            )
        }
        assertThrows(DonationDocumentException::class.java) {
            DonationLedger.parse(
                """{"version":"ZARA-DONATIONS/1","campaigns":[{"id":"x","title":"X"},{"id":"x","title":"Again"}]}""",
            )
        }
    }

    @Test
    fun rejectsPrivateKeyShapedFieldsByIgnoringNoUnknownSecretsContract() {
        val ledger = DonationLedger.parse(
            """
            {
              "version":"ZARA-DONATIONS/1",
              "campaigns":[
                {
                  "id":"safe",
                  "title":"Safe",
                  "wallets":[
                    {
                      "chain":"bitcoin",
                      "network":"mainnet",
                      "asset":"BTC",
                      "address":"bc1qexample"
                    }
                  ]
                }
              ]
            }
            """.trimIndent(),
        )

        assertEquals(1, ledger.campaigns.single().wallets.size)
    }
}
