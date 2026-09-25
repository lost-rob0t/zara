package ai.zara.org.surfaces

import java.time.Duration

data class OrgTimerRun(
    val key: String,
    val name: String,
    val total: Duration,
    val remaining: Duration = total,
    val running: Boolean = false,
) {
    val finished: Boolean get() = remaining.isZero && !running

    fun tick(): OrgTimerRun {
        if (!running) return this
        if (remaining.isZero) return copy(remaining = Duration.ZERO, running = false)
        val next = remaining.minusSeconds(1)
        return if (next.isZero) copy(remaining = Duration.ZERO, running = false) else copy(remaining = next)
    }

    fun start(): OrgTimerRun = copy(running = remaining > Duration.ZERO)

    fun pause(): OrgTimerRun = copy(running = false)

    fun reset(): OrgTimerRun = copy(remaining = total, running = false)
}

object OrgTimerFormat {
    fun format(duration: Duration): String {
        val totalSeconds = duration.seconds
        val hours = totalSeconds / 3600
        val minutes = (totalSeconds % 3600) / 60
        val seconds = totalSeconds % 60
        return if (hours > 0) {
            "%d:%02d:%02d".format(hours, minutes, seconds)
        } else {
            "%02d:%02d".format(minutes, seconds)
        }
    }
}
