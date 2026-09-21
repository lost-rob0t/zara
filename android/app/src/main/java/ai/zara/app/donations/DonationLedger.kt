package ai.zara.app.donations

import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.JsonParseException
import com.google.gson.JsonParser
import java.math.BigDecimal

class DonationDocumentException(message: String, cause: Throwable? = null) :
    IllegalArgumentException(message, cause)

data class DonationWallet(
    val chain: String,
    val network: String,
    val asset: String,
    val address: String,
    val label: String?,
)

data class DonationCampaign(
    val id: String,
    val title: String,
    val goalUsd: BigDecimal,
    val raisedUsd: BigDecimal,
    val active: Boolean,
    val wallets: List<DonationWallet>,
) {
    val remainingUsd: BigDecimal
        get() = (goalUsd - raisedUsd).max(BigDecimal.ZERO).setScale(2)
}

data class DonationLedger(
    val campaigns: List<DonationCampaign> = emptyList(),
) {
    val totalGoalUsd: BigDecimal
        get() = campaigns.fold(BigDecimal.ZERO) { total, campaign -> total + campaign.goalUsd }
            .setScale(2)

    val totalRaisedUsd: BigDecimal
        get() = campaigns.fold(BigDecimal.ZERO) { total, campaign -> total + campaign.raisedUsd }
            .setScale(2)

    val totalRemainingUsd: BigDecimal
        get() = campaigns.fold(BigDecimal.ZERO) { total, campaign -> total + campaign.remainingUsd }
            .setScale(2)

    val activeCampaignCount: Int
        get() = campaigns.count { it.active }

    companion object {
        const val DOCUMENT_VERSION = "ZARA-DONATIONS/1"
        private val maxUsd = BigDecimal("1000000000000.00")
        private val campaignId = Regex("^[a-z0-9][a-z0-9_-]{0,63}$")

        fun parse(payload: String): DonationLedger {
            val root = try {
                JsonParser.parseString(payload)
            } catch (error: JsonParseException) {
                throw DonationDocumentException("donation document is not valid JSON", error)
            }
            if (!root.isJsonObject) {
                throw DonationDocumentException("donation document must be an object")
            }
            val document = root.asJsonObject
            rejectUnknown(
                document,
                setOf("version", "campaigns", "summary"),
                "donation document",
            )
            if (requiredString(document, "version", "version", 64) != DOCUMENT_VERSION) {
                throw DonationDocumentException("version must be $DOCUMENT_VERSION")
            }
            val campaigns = array(document, "campaigns").mapIndexed { index, element ->
                campaign(element, index)
            }
            val ids = campaigns.map { it.id }
            if (ids.distinct().size != ids.size) {
                throw DonationDocumentException("campaign ids must be unique")
            }
            return DonationLedger(campaigns)
        }

        private fun campaign(element: JsonElement, index: Int): DonationCampaign {
            if (!element.isJsonObject) {
                throw DonationDocumentException("campaign[$index] must be an object")
            }
            val value = element.asJsonObject
            rejectUnknown(
                value,
                setOf(
                    "id",
                    "title",
                    "goal_usd",
                    "raised_usd",
                    "remaining_usd",
                    "active",
                    "wallets",
                ),
                "campaign",
            )
            val id = requiredString(value, "id", "campaign id", 64)
            if (!campaignId.matches(id)) {
                throw DonationDocumentException(
                    "campaign id must match [a-z0-9][a-z0-9_-]{0,63}",
                )
            }
            val active = optionalBoolean(value, "active", true)
            val wallets = array(value, "wallets").mapIndexed { walletIndex, wallet ->
                wallet(wallet, index, walletIndex)
            }
            return DonationCampaign(
                id = id,
                title = requiredString(value, "title", "campaign title", 120),
                goalUsd = usd(value.get("goal_usd"), "campaign goal_usd"),
                raisedUsd = usd(value.get("raised_usd"), "campaign raised_usd"),
                active = active,
                wallets = wallets,
            )
        }

        private fun wallet(
            element: JsonElement,
            campaignIndex: Int,
            walletIndex: Int,
        ): DonationWallet {
            if (!element.isJsonObject) {
                throw DonationDocumentException(
                    "campaign[$campaignIndex].wallets[$walletIndex] must be an object",
                )
            }
            val value = element.asJsonObject
            rejectUnknown(
                value,
                setOf("chain", "network", "asset", "address", "label"),
                "wallet",
            )
            return DonationWallet(
                chain = requiredString(value, "chain", "wallet chain", 40),
                network = requiredString(value, "network", "wallet network", 40),
                asset = requiredString(value, "asset", "wallet asset", 24),
                address = requiredString(value, "address", "wallet address", 256),
                label = optionalString(value, "label", "wallet label", 80),
            )
        }

        private fun rejectUnknown(
            value: JsonObject,
            allowed: Set<String>,
            label: String,
        ) {
            val unknown = value.keySet().filterNot(allowed::contains).sorted()
            if (unknown.isNotEmpty()) {
                throw DonationDocumentException(
                    "$label contains unsupported fields: ${unknown.joinToString(", ")}",
                )
            }
        }

        private fun array(value: JsonObject, key: String): List<JsonElement> {
            val element = value.get(key) ?: return emptyList()
            if (!element.isJsonArray) {
                throw DonationDocumentException("$key must be an array")
            }
            return element.asJsonArray.toList()
        }

        private fun optionalBoolean(value: JsonObject, key: String, fallback: Boolean): Boolean {
            val element = value.get(key) ?: return fallback
            if (!element.isJsonPrimitive || !element.asJsonPrimitive.isBoolean) {
                throw DonationDocumentException("campaign active must be boolean")
            }
            return element.asBoolean
        }

        private fun optionalString(
            value: JsonObject,
            key: String,
            label: String,
            maximum: Int,
        ): String? {
            val element = value.get(key) ?: return null
            if (element.isJsonNull) return null
            return text(element, label, maximum)
        }

        private fun requiredString(
            value: JsonObject,
            key: String,
            label: String,
            maximum: Int,
        ): String {
            val element = value.get(key)
                ?: throw DonationDocumentException("$label must be a string")
            return text(element, label, maximum)
        }

        private fun text(element: JsonElement, label: String, maximum: Int): String {
            if (!element.isJsonPrimitive || !element.asJsonPrimitive.isString) {
                throw DonationDocumentException("$label must be a string")
            }
            val text = element.asString.trim()
            if (text.isEmpty()) {
                throw DonationDocumentException("$label must not be empty")
            }
            if ('\u0000' in text) {
                throw DonationDocumentException("$label must not contain NUL")
            }
            if (text.length > maximum) {
                throw DonationDocumentException("$label exceeds $maximum characters")
            }
            return text
        }

        private fun usd(element: JsonElement?, label: String): BigDecimal {
            if (element == null) return BigDecimal("0.00")
            if (!element.isJsonPrimitive || element.asJsonPrimitive.isBoolean) {
                throw DonationDocumentException("$label must be a USD amount")
            }
            val amount = try {
                BigDecimal(element.asString)
            } catch (error: NumberFormatException) {
                throw DonationDocumentException("$label must be a USD amount", error)
            }
            if (amount.signum() < 0) {
                throw DonationDocumentException("$label must not be negative")
            }
            if (amount > maxUsd) {
                throw DonationDocumentException("$label exceeds supported maximum")
            }
            if (amount.stripTrailingZeros().scale() > 2) {
                throw DonationDocumentException("$label must use at most two decimal places")
            }
            return amount.setScale(2)
        }
    }
}
