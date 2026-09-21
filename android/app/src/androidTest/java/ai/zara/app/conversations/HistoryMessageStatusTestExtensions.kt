package ai.zara.app.conversations

import ai.zara.app.history.HistoryMessageStatus

/**
 * Test-only mirror of the canonical history owner's running-state predicate.
 *
 * Keep instrumentation assertions explicit about the two non-terminal assistant states without
 * adding UI/runtime ownership or changing production conversation semantics.
 */
internal fun HistoryMessageStatus.isRunning(): Boolean =
    this == HistoryMessageStatus.Pending || this == HistoryMessageStatus.Streaming
