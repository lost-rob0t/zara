"""Inspector drawer: the Prolog ADE surface for the one Copilot renderer.

Extracted from the Settings Prolog console semantics (generation fence,
honest cancel, bounded rendering) so the Copilot window can host the same
query lifecycle in a summoned drawer. The desktop never imports pyswip:
every engine view is a ``RunPrologQuery`` receipt or a labeled saved-file
read, and facts browsing issues only allowlisted ``desktop_ade:*`` goals.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Optional

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import (
    QComboBox,
    QFrame,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QPlainTextEdit,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from zara.runtime.commands import PrologQueryReceipt

MAX_QUERY_BYTES = 4096
UI_REQUESTED_SOLUTIONS = 50
QUERY_HISTORY_LIMIT = 20
RENDER_LINE_CHARS = 2_000
RENDER_TOTAL_CHARS = 64_000

ALLOWLISTED_ADE_PREFIX = "desktop_ade:"
ALLOWLISTED_ADE_GOALS: tuple[str, ...] = (
    "desktop_ade:kb_module_summary(Module, Count)",
    "desktop_ade:intent_catalog(Phrase, Intent, Arity)",
    "desktop_ade:wake_words(Words)",
    "desktop_ade:fact_sample(Module, Term, Limit)",
)

CANCEL_STATUS = "Cancelled · late results will be discarded"
RESTART_STATUS = "Runtime restarting — results discarded"
UNAVAILABLE_STATUS = "Runtime query unavailable"


@dataclass(frozen=True)
class ConsoleRender:
    """Widget-free render of one settled query outcome."""

    text: str
    status: str


class QueryConsoleController(QObject):
    """Generation-fenced query lifecycle without widget ownership.

    One controller per visible console surface. ``run`` submits through the
    injected runtime callable on the caller thread; results re-enter via the
    ``query_finished`` signal; anything whose generation is no longer current
    can never render.
    """

    query_finished = Signal(int, object)
    status_changed = Signal(str)

    def __init__(
        self,
        prolog_query: Optional[Callable[[str, int], object]] = None,
        parent: Optional[QObject] = None,
    ) -> None:
        super().__init__(parent)
        self.prolog_query = prolog_query
        self.generation = 0
        self.busy = False
        self.status = ""
        self._history: list[str] = []

    @property
    def history(self) -> tuple[str, ...]:
        return tuple(self._history)

    def _emit_status(self, status: str) -> None:
        self.status = status
        self.status_changed.emit(status)

    def run(self, goal: str) -> str:
        """Validate and submit one bounded goal; returns the status text."""
        goal = goal.strip()
        if not goal:
            self._emit_status("Enter a goal")
            return self.status
        if self.prolog_query is None:
            self._emit_status(UNAVAILABLE_STATUS)
            return self.status
        if len(goal.encode("utf-8")) > MAX_QUERY_BYTES:
            self._emit_status(
                f"Goal exceeds the {MAX_QUERY_BYTES}-byte limit "
                "and was not submitted"
            )
            return self.status
        self.generation += 1
        generation = self.generation
        self.busy = True
        self._remember_goal(goal)
        self._emit_status("Running…")
        try:
            future = self.prolog_query(goal, UI_REQUESTED_SOLUTIONS)
        except Exception as error:
            return self.finish(generation, error).status
        future.add_done_callback(
            lambda completed, token=generation: self.query_finished.emit(
                token,
                completed,
            )
        )
        return self.status

    def cancel(self) -> str:
        """Fence the inflight query; no engine-side interrupt is pretended."""
        if not self.busy:
            return self.status
        self.generation += 1
        self.busy = False
        self._emit_status(CANCEL_STATUS)
        return CANCEL_STATUS

    def fence_restart(self) -> str:
        """Drop inflight results when the runtime restarts or disconnects."""
        self.generation += 1
        self.busy = False
        self._emit_status(RESTART_STATUS)
        return RESTART_STATUS

    def finish(self, generation: int, outcome: object) -> ConsoleRender:
        """Render the outcome if its generation is still current."""
        if generation != self.generation:
            return ConsoleRender(text="", status="")
        self.busy = False
        try:
            if isinstance(outcome, BaseException):
                raise outcome
            receipt = outcome.result()
            if not isinstance(receipt, PrologQueryReceipt):
                raise TypeError("runtime returned an invalid Prolog query receipt")
        except BaseException as error:
            self._emit_status("Query failed")
            return ConsoleRender(text=f"ERROR\n{error}", status="Query failed")
        lines: list[str] = []
        rendered_chars = 0
        for index, solution in enumerate(receipt.solutions, start=1):
            rendered = ", ".join(f"{key} = {value}" for key, value in solution.items())
            line = f"{index:>2}  {rendered or 'true'}"
            if len(line) > RENDER_LINE_CHARS:
                line = f"{line[: RENDER_LINE_CHARS - 1]}…"
            if rendered_chars + len(line) > RENDER_TOTAL_CHARS:
                lines.append(f"… output truncated at {RENDER_TOTAL_CHARS:,} characters")
                break
            lines.append(line)
            rendered_chars += len(line) + 1
        count = len(receipt.solutions)
        status = f"{count} solution" if count == 1 else f"{count} solutions"
        self._emit_status(status)
        return ConsoleRender(
            text="\n".join(lines) if lines else "false",
            status=status,
        )

    def _remember_goal(self, goal: str) -> None:
        if goal in self._history:
            self._history.remove(goal)
        self._history.insert(0, goal)
        del self._history[QUERY_HISTORY_LIMIT:]


class InspectorDrawer(QWidget):
    """Right-anchored ADE drawer: Console, KB facts, Live state."""

    def __init__(
        self,
        prolog_query: Optional[Callable[[str, int], object]] = None,
        *,
        fact_store=None,
        live_state_provider: Optional[Callable[[], str]] = None,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.setObjectName("zaraPrologInspector")
        self._fact_store = fact_store
        self._live_state_provider = live_state_provider
        self._facts_generation = 0

        self.controller = QueryConsoleController(prolog_query, self)
        self.controller.query_finished.connect(self._on_query_finished)
        self.controller.status_changed.connect(self._set_query_status)
        self.controller.status_changed.connect(self._sync_busy_buttons)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(12, 12, 12, 12)
        layout.setSpacing(8)

        title = QLabel("Prolog Inspector")
        title.setObjectName("zaraSurfaceName")
        hint = QLabel(
            "Live, read-only view of Zara's Prolog engine. "
            "Queries are bounded and never mutate the knowledge base."
        )
        hint.setObjectName("zaraInspectorHint")
        hint.setWordWrap(True)
        layout.addWidget(title)
        layout.addWidget(hint)

        self._console_section = self._build_console_section()
        self._facts_section = self._build_facts_section()
        self._state_section = self._build_state_section()
        layout.addWidget(self._console_section)
        layout.addWidget(self._facts_section)
        layout.addWidget(self._state_section, 1)
        self.query_status.setText(
            "Ready" if prolog_query is not None else UNAVAILABLE_STATUS
        )
        self._sync_busy_buttons()
        self.refresh_live_state()

    # -- console -----------------------------------------------------------

    def _build_console_section(self) -> QFrame:
        section = QFrame(self)
        section.setObjectName("zaraInspectorConsole")
        section_layout = QVBoxLayout(section)
        section_layout.setContentsMargins(0, 0, 0, 0)
        section_layout.setSpacing(6)

        self.query_input = QLineEdit(section)
        self.query_input.setObjectName("zaraInspectorQueryInput")
        self.query_input.setPlaceholderText("?- goal")
        self.query_input.returnPressed.connect(self.run_query)

        history_row = QHBoxLayout()
        self.query_history = QComboBox(section)
        self.query_history.setObjectName("zaraInspectorQueryHistory")
        self.query_history.setMinimumWidth(120)
        self.query_history.addItem("History")
        self.query_history.activated.connect(self._select_history_goal)
        self.run_button = QPushButton("Run", section)
        self.run_button.setObjectName("zaraInspectorQueryRun")
        self.run_button.clicked.connect(self.run_query)
        self.cancel_button = QPushButton("Cancel", section)
        self.cancel_button.setObjectName("zaraInspectorQueryCancel")
        self.cancel_button.setEnabled(False)
        self.cancel_button.clicked.connect(self.cancel_query)
        history_row.addWidget(self.query_history)
        history_row.addWidget(self.query_input, 1)
        history_row.addWidget(self.run_button)
        history_row.addWidget(self.cancel_button)

        self.results = QPlainTextEdit(section)
        self.results.setObjectName("zaraInspectorResults")
        self.results.setReadOnly(True)
        self.results.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        self.query_status = QLabel(section)
        self.query_status.setObjectName("zaraInspectorQueryStatus")

        section_layout.addLayout(history_row)
        section_layout.addWidget(self.results, 1)
        section_layout.addWidget(self.query_status)
        return section

    def _set_query_status(self, status: str) -> None:
        self.query_status.setText(status)

    def run_query(self) -> None:
        self.controller.run(self.query_input.text())
        # controller.status reflects the settled state even when a resolved
        # future completes synchronously inside run().
        self.query_status.setText(self.controller.status)
        self._sync_history_combo()
        self._sync_busy_buttons()

    def cancel_query(self) -> None:
        self.cancel_button.setEnabled(False)
        self.controller.cancel()

    def _on_query_finished(self, generation: int, outcome: object) -> None:
        render = self.controller.finish(generation, outcome)
        if render.status:
            self.results.setPlainText(render.text)
        self._sync_busy_buttons()

    def _sync_busy_buttons(self) -> None:
        self.run_button.setEnabled(not self.controller.busy)
        self.cancel_button.setEnabled(self.controller.busy)

    def _sync_history_combo(self) -> None:
        self.query_history.blockSignals(True)
        self.query_history.clear()
        self.query_history.addItem("History")
        for goal in self.controller.history:
            self.query_history.addItem(goal)
        self.query_history.setCurrentIndex(0)
        self.query_history.blockSignals(False)

    def _select_history_goal(self, index: int) -> None:
        if index <= 0:
            return
        self.query_input.setText(self.query_history.itemText(index))
        self.query_history.setCurrentIndex(0)

    # -- facts -------------------------------------------------------------

    def _build_facts_section(self) -> QFrame:
        section = QFrame(self)
        section.setObjectName("zaraInspectorFacts")
        section_layout = QVBoxLayout(section)
        section_layout.setContentsMargins(0, 0, 0, 0)
        section_layout.setSpacing(6)

        header = QHBoxLayout()
        facts_title = QLabel("KB facts")
        facts_title.setObjectName("zaraMessageRole")
        self.facts_refresh_button = QPushButton("Refresh", section)
        self.facts_refresh_button.setObjectName("zaraInspectorFactsRefresh")
        self.facts_refresh_button.clicked.connect(self.refresh_engine_facts)
        header.addWidget(facts_title)
        header.addStretch(1)
        header.addWidget(self.facts_refresh_button)

        self.saved_facts_label = QLabel("Saved · applied on reload", section)
        self.saved_facts_label.setObjectName("zaraInspectorSavedFactsLabel")
        self.saved_facts = QListWidget(section)
        self.saved_facts.setObjectName("zaraInspectorSavedFacts")
        self.saved_facts.setMaximumHeight(90)
        self.engine_facts = QListWidget(section)
        self.engine_facts.setObjectName("zaraInspectorEngineFacts")

        section_layout.addLayout(header)
        section_layout.addWidget(self.saved_facts_label)
        section_layout.addWidget(self.saved_facts)
        section_layout.addWidget(self.engine_facts, 1)
        self._refresh_saved_facts()
        return section

    def _refresh_saved_facts(self) -> None:
        self.saved_facts.clear()
        if self._fact_store is None:
            return
        for fact in self._fact_store.list():
            fields = ", ".join(f"{key}={value}" for key, value in fact.fields.items())
            self.saved_facts.addItem(f"{fact.kind}({fields})")

    def refresh_engine_facts(self) -> None:
        """Issue only the allowlisted bounded projection goals."""
        self.engine_facts.clear()
        if self.controller.prolog_query is None:
            return
        self._facts_generation += 1
        generation = self._facts_generation
        for goal in ALLOWLISTED_ADE_GOALS:
            try:
                future = self.controller.prolog_query(goal, UI_REQUESTED_SOLUTIONS)
            except Exception as error:
                self.engine_facts.addItem(f"{goal} — query failed: {error}")
                continue
            future.add_done_callback(
                lambda completed, token=generation, issued=goal: (
                    self._on_fact_receipt(token, issued, completed)
                )
            )

    def _on_fact_receipt(self, generation: int, goal: str, outcome: object) -> None:
        if generation != self._facts_generation:
            return
        try:
            if isinstance(outcome, BaseException):
                raise outcome
            receipt = outcome.result()
            if not isinstance(receipt, PrologQueryReceipt):
                raise TypeError("runtime returned an invalid Prolog query receipt")
        except BaseException as error:
            self.engine_facts.addItem(f"{goal} — query failed: {error}")
            return
        count = len(receipt.solutions)
        self.engine_facts.addItem(f"{goal} — {count} solutions")
        for solution in receipt.solutions[:3]:
            rendered = ", ".join(f"{k} = {v}" for k, v in solution.items())
            self.engine_facts.addItem(f"    {rendered or 'true'}")

    # -- live state --------------------------------------------------------

    def _build_state_section(self) -> QFrame:
        section = QFrame(self)
        section.setObjectName("zaraInspectorLiveState")
        section_layout = QVBoxLayout(section)
        section_layout.setContentsMargins(0, 0, 0, 0)
        section_layout.setSpacing(6)
        state_title = QLabel("Live state")
        state_title.setObjectName("zaraMessageRole")
        self.live_state = QPlainTextEdit(section)
        self.live_state.setObjectName("zaraInspectorLiveStateText")
        self.live_state.setReadOnly(True)
        section_layout.addWidget(state_title)
        section_layout.addWidget(self.live_state, 1)
        return section

    def refresh_live_state(self) -> None:
        if self._live_state_provider is None:
            self.live_state.setPlainText("No symbolic projection recorded")
            return
        self.live_state.setPlainText(self._live_state_provider())
