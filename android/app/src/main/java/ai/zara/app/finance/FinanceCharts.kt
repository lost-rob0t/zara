package ai.zara.app.finance

import kotlin.math.abs

enum class FinanceChartType {
    CANDLESTICK,
    LINE,
    AREA,
    BAR,
    RENKO,
    HEIKIN_ASHI,
    KAGI,
    POINT_AND_FIGURE,
    LINE_BREAK,
    DEPTH,
    PROBABILITY,
}

enum class FinanceOverlayType {
    VOLUME,
    VOLUME_PROFILE,
    ORDERBOOK,
    STARINTEL_EVIDENCE,
    EXPERT_DECISION,
    RISK_BAND,
    PAPER_FILL,
    FORECAST_DISTRIBUTION,
    CALIBRATION,
}

data class OhlcBar(
    val timestampMillis: Long,
    val open: Double,
    val high: Double,
    val low: Double,
    val close: Double,
    val volume: Double = 0.0,
) {
    init {
        require(high >= low)
        require(open in low..high)
        require(close in low..high)
        require(volume >= 0.0)
    }
}

enum class RenkoDirection {
    UP,
    DOWN,
}

data class RenkoBrick(
    val timestampMillis: Long,
    val open: Double,
    val close: Double,
    val direction: RenkoDirection,
    val sourceIndex: Int,
)

object FinanceChartTransforms {
    fun renko(
        bars: List<OhlcBar>,
        brickSize: Double,
    ): List<RenkoBrick> {
        require(brickSize > 0.0)
        if (bars.isEmpty()) return emptyList()

        val bricks = mutableListOf<RenkoBrick>()
        var reference = bars.first().close

        bars.forEachIndexed { index, bar ->
            while (abs(bar.close - reference) + 1e-12 >= brickSize) {
                val direction =
                    if (bar.close > reference) RenkoDirection.UP else RenkoDirection.DOWN
                val next =
                    if (direction == RenkoDirection.UP) {
                        reference + brickSize
                    } else {
                        reference - brickSize
                    }
                bricks +=
                    RenkoBrick(
                        timestampMillis = bar.timestampMillis,
                        open = reference,
                        close = next,
                        direction = direction,
                        sourceIndex = index,
                    )
                reference = next
            }
        }

        return bricks
    }

    fun heikinAshi(bars: List<OhlcBar>): List<OhlcBar> {
        if (bars.isEmpty()) return emptyList()

        val result = ArrayList<OhlcBar>(bars.size)
        bars.forEachIndexed { index, bar ->
            val close = (bar.open + bar.high + bar.low + bar.close) / 4.0
            val open =
                if (index == 0) {
                    (bar.open + bar.close) / 2.0
                } else {
                    val previous = result.last()
                    (previous.open + previous.close) / 2.0
                }
            val high = maxOf(bar.high, open, close)
            val low = minOf(bar.low, open, close)
            result +=
                OhlcBar(
                    timestampMillis = bar.timestampMillis,
                    open = open,
                    high = high,
                    low = low,
                    close = close,
                    volume = bar.volume,
                )
        }
        return result
    }
}

data class FinanceAutomationScript(
    val id: String,
    val language: Language,
    val source: String,
    val generatedByModel: Boolean,
    val paperOnly: Boolean = true,
) {
    enum class Language {
        STAR_FUNDS_DSL,
        COMMON_LISP,
        PROLOG,
        LISA,
    }

    init {
        require(id.isNotBlank())
        require(source.isNotBlank())
        require(paperOnly)
    }
}
