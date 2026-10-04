package ai.zara.wear.health

import ai.zara.ui.health.HealthGoalMetric
import ai.zara.ui.health.HealthGoalTarget
import ai.zara.wear.EXTRA_OPEN_HEALTH
import ai.zara.wear.WearCompanionLinkState
import ai.zara.wear.WearCompanionRuntime
import ai.zara.wear.WearMainActivity
import android.app.PendingIntent
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
import androidx.wear.watchface.complications.data.NoDataComplicationData
import androidx.wear.watchface.complications.data.PlainComplicationText
import androidx.wear.watchface.complications.data.ShortTextComplicationData
import androidx.wear.watchface.complications.datasource.ComplicationDataSourceUpdateRequester
import androidx.wear.watchface.complications.datasource.ComplicationRequest
import androidx.wear.watchface.complications.datasource.SuspendingComplicationDataSourceService
import java.text.NumberFormat
import java.util.Locale

object HealthGoalComplicationText {
    fun short(goal: HealthGoalTarget): String =
        when (goal.metric) {
            HealthGoalMetric.STEPS -> if (goal.target >= 1_000) {
                val thousands = goal.target / 1_000
                val remainder = goal.target % 1_000
                if (remainder == 0) "${thousands}K" else String.format(Locale.US, "%.1fK", goal.target / 1_000.0)
            } else {
                goal.target.toString()
            }
            HealthGoalMetric.SLEEP -> {
                val hours = goal.target / 60
                val minutes = goal.target % 60
                if (minutes == 0) "${hours}H" else "${hours}H${minutes.toString().padStart(2, '0')}"
            }
            else -> goal.target.toString()
        }

    fun description(goal: HealthGoalTarget): String =
        when (goal.metric) {
            HealthGoalMetric.STEPS ->
                "STEPS ${NumberFormat.getIntegerInstance(Locale.US).format(goal.target)} steps"
            HealthGoalMetric.SLEEP -> {
                val hours = goal.target / 60
                val minutes = goal.target % 60
                buildString {
                    append("SLEEP ")
                    append(hours)
                    append(if (hours == 1) " hour" else " hours")
                    if (minutes > 0) {
                        append(' ')
                        append(minutes)
                        append(if (minutes == 1) " minute" else " minutes")
                    }
                }
            }
            else -> "${goal.metric.label.uppercase(Locale.US)} ${goal.target} ${goal.metric.unit}"
        }
}

abstract class ZaraHealthGoalComplicationService : SuspendingComplicationDataSourceService() {
    protected abstract val metric: HealthGoalMetric

    override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData {
        if (request.complicationType != ComplicationType.SHORT_TEXT) return NoDataComplicationData()
        val runtime = WearCompanionRuntime.get(this)
        runtime.start()
        val goal = (runtime.state() as? WearCompanionLinkState.Paired)
            ?.healthGoals
            ?.firstOrNull { it.metric == metric }
            ?: return NoDataComplicationData()
        return complication(goal)
    }

    override fun getPreviewData(type: ComplicationType): ComplicationData? =
        if (type == ComplicationType.SHORT_TEXT) {
            complication(HealthGoalTarget(metric, metric.defaultTarget))
        } else {
            null
        }

    private fun complication(goal: HealthGoalTarget): ComplicationData {
        val description = HealthGoalComplicationText.description(goal)
        return ShortTextComplicationData.Builder(
            text = PlainComplicationText.Builder(HealthGoalComplicationText.short(goal)).build(),
            contentDescription = PlainComplicationText.Builder(description).build(),
        )
            .setTitle(PlainComplicationText.Builder(metric.label).build())
            .setTapAction(openHealthAction())
            .build()
    }

    private fun openHealthAction(): PendingIntent {
        val intent = Intent(this, WearMainActivity::class.java)
            .setAction("ai.zara.wear.action.OPEN_HEALTH.${metric.atom}")
            .putExtra(EXTRA_OPEN_HEALTH, true)
        return PendingIntent.getActivity(
            this,
            metric.ordinal,
            intent,
            PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
        )
    }
}

class ZaraStepGoalComplicationService : ZaraHealthGoalComplicationService() {
    override val metric = HealthGoalMetric.STEPS
}

class ZaraSleepGoalComplicationService : ZaraHealthGoalComplicationService() {
    override val metric = HealthGoalMetric.SLEEP
}

object ZaraHealthComplicationUpdater {
    fun requestAll(context: Context) {
        listOf(
            ZaraStepGoalComplicationService::class.java,
            ZaraSleepGoalComplicationService::class.java,
        ).forEach { service ->
            ComplicationDataSourceUpdateRequester.create(
                context,
                ComponentName(context, service),
            ).requestUpdateAll()
        }
    }
}
