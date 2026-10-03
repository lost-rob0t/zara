package ai.zara.app.samsunghealth

import ai.zara.ui.health.HealthGoalTarget
import java.math.BigDecimal
import java.util.Base64

object HealthPrologExport {
    private val decimal = Regex("-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?")

    fun render(
        readings: Map<SamsungHealthMetric, SamsungHealthReading>,
        goals: List<HealthGoalTarget>,
        epochMs: Long = System.currentTimeMillis(),
    ): ByteArray {
        require(epochMs >= 0) { "health export time must be non-negative" }
        val facts = mutableListOf("health_db_version(1).")
        readings.toSortedMap(compareBy(SamsungHealthMetric::atom)).forEach { (metric, reading) ->
            val values = reading.values.toSortedMap().mapNotNull { (name, rawValue) ->
                val value = canonicalDecimal(rawValue) ?: return@mapNotNull null
                "health_value(${token(name)},$value,${token(unit(name))})"
            }
            if (values.isNotEmpty()) {
                facts += "health_observation(" +
                    "${token("android-${metric.atom}-$epochMs")},${token("local:owner")},${metric.atom}," +
                    "$epochMs,$epochMs,${token("samsung_health_data_sdk")}," +
                    "${metric.privacy.name.lowercase()},[${values.joinToString(",")}])."
            }
        }
        goals.sortedBy { it.metric.atom }.forEach { goal ->
            facts += "health_goal(${token("local:owner")},${goal.metric.atom},${goal.target}," +
                "${token(goal.metric.unit)},daily,$epochMs)."
        }
        return (facts.joinToString("\n") + "\n").toByteArray(Charsets.UTF_8)
    }

    private fun canonicalDecimal(value: String): String? {
        if (!decimal.matches(value)) return null
        return try {
            BigDecimal(value).stripTrailingZeros().toPlainString()
        } catch (_: NumberFormatException) {
            null
        }
    }

    private fun token(value: String): String = "b64_" + Base64.getUrlEncoder().withoutPadding()
        .encodeToString(value.toByteArray(Charsets.UTF_8))

    private fun unit(name: String): String = when {
        name == "steps" || name == "count" -> "steps"
        name.endsWith("_minutes") -> "minutes"
        name.endsWith("_bpm") -> "bpm"
        name.endsWith("_kcal") -> "kcal"
        else -> "native"
    }
}
