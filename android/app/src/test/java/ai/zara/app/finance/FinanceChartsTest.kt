package ai.zara.app.finance

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class FinanceChartsTest {
    @Test
    fun renkoBuildsMultipleBricksAcrossLargeMove() {
        val bars =
            listOf(
                OhlcBar(1L, 100.0, 100.0, 100.0, 100.0),
                OhlcBar(2L, 100.0, 103.2, 100.0, 103.2),
            )

        val bricks = FinanceChartTransforms.renko(bars, brickSize = 1.0)

        assertEquals(3, bricks.size)
        assertEquals(RenkoDirection.UP, bricks[0].direction)
        assertEquals(100.0, bricks[0].open, 0.0)
        assertEquals(103.0, bricks.last().close, 0.0)
        assertEquals(1, bricks.last().sourceIndex)
    }

    @Test
    fun renkoReversesWithoutInventingFuturePrices() {
        val bars =
            listOf(
                OhlcBar(1L, 10.0, 10.0, 10.0, 10.0),
                OhlcBar(2L, 10.0, 12.0, 10.0, 12.0),
                OhlcBar(3L, 8.0, 12.0, 8.0, 8.0),
            )

        val bricks = FinanceChartTransforms.renko(bars, brickSize = 1.0)

        assertEquals(
            listOf(
                RenkoDirection.UP,
                RenkoDirection.UP,
                RenkoDirection.DOWN,
                RenkoDirection.DOWN,
                RenkoDirection.DOWN,
                RenkoDirection.DOWN,
            ),
            bricks.map { it.direction },
        )
        assertEquals(8.0, bricks.last().close, 0.0)
    }

    @Test
    fun chartInventoryIncludesOperatorPreferredAndMarketSpecificViews() {
        assertTrue(FinanceChartType.entries.contains(FinanceChartType.RENKO))
        assertTrue(FinanceChartType.entries.contains(FinanceChartType.POINT_AND_FIGURE))
        assertTrue(FinanceChartType.entries.contains(FinanceChartType.DEPTH))
        assertTrue(FinanceChartType.entries.contains(FinanceChartType.PROBABILITY))
    }

    @Test
    fun generatedAutomationIsPaperOnlyByConstruction() {
        val script =
            FinanceAutomationScript(
                id = "model-proposal-1",
                language = FinanceAutomationScript.Language.COMMON_LISP,
                source = "(strategy ...)",
                generatedByModel = true,
            )

        assertTrue(script.paperOnly)
    }
}
