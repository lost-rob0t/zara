#!/usr/bin/env python3
"""Fail when an idle Zara desktop process tree burns sustained CPU."""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import math
import os
import signal
import subprocess
import sys
import tempfile
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Iterable

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

DEFAULT_MODES = ("visible", "hidden", "expanded")


@dataclass(frozen=True)
class CpuResult:
    mode: str
    average_cpu_percent: float
    p95_interval_cpu_percent: float
    sample_seconds: float
    sample_count: int
    max_average_cpu_percent: float
    max_p95_interval_cpu_percent: float
    passed: bool


def completed(value=None) -> concurrent.futures.Future:
    future: concurrent.futures.Future = concurrent.futures.Future()
    future.set_result(value)
    return future


def percentile(values: Iterable[float], fraction: float) -> float:
    ordered = sorted(values)
    if not ordered:
        return 0.0
    index = max(0, math.ceil(len(ordered) * fraction) - 1)
    return ordered[index]


def read_process_ticks(pid: int) -> int:
    try:
        raw = Path(f"/proc/{pid}/stat").read_text()
    except (FileNotFoundError, ProcessLookupError):
        return 0
    end = raw.rfind(")")
    if end < 0:
        raise RuntimeError(f"malformed /proc/{pid}/stat")
    fields = raw[end + 2 :].split()
    return int(fields[11]) + int(fields[12])


def child_pids(pid: int) -> tuple[int, ...]:
    path = Path(f"/proc/{pid}/task/{pid}/children")
    try:
        text = path.read_text().strip()
    except (FileNotFoundError, ProcessLookupError):
        return ()
    if not text:
        return ()
    return tuple(int(value) for value in text.split())


def process_tree_pids(root_pid: int) -> set[int]:
    pending = [root_pid]
    seen: set[int] = set()
    while pending:
        pid = pending.pop()
        if pid in seen:
            continue
        seen.add(pid)
        pending.extend(child_pids(pid))
    return seen


def process_tree_ticks(root_pid: int) -> int:
    return sum(read_process_ticks(pid) for pid in process_tree_pids(root_pid))


def wait_for_ready(process: subprocess.Popen, ready_path: Path, timeout: float) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if ready_path.exists():
            return
        if process.poll() is not None:
            raise RuntimeError(f"desktop probe exited before ready with code {process.returncode}")
        time.sleep(0.05)
    raise TimeoutError(f"desktop probe did not become ready within {timeout:.1f}s")


def stop_process(process: subprocess.Popen) -> None:
    if process.poll() is not None:
        return
    process.send_signal(signal.SIGTERM)
    try:
        process.wait(timeout=3)
        return
    except subprocess.TimeoutExpired:
        process.kill()
    process.wait(timeout=3)


def sample_cpu(
    pid: int,
    *,
    sample_seconds: float,
    interval_seconds: float,
) -> tuple[float, float, int]:
    clock_ticks = os.sysconf("SC_CLK_TCK")
    started = time.monotonic()
    previous_time = started
    previous_ticks = process_tree_ticks(pid)
    interval_cpu: list[float] = []
    final_ticks = previous_ticks
    final_time = previous_time

    while final_time - started < sample_seconds:
        time.sleep(interval_seconds)
        current_time = time.monotonic()
        current_ticks = process_tree_ticks(pid)
        elapsed = current_time - previous_time
        tick_delta = max(0, current_ticks - previous_ticks)
        interval_cpu.append((tick_delta / clock_ticks) / elapsed * 100.0)
        previous_time = current_time
        previous_ticks = current_ticks
        final_time = current_time
        final_ticks = current_ticks

    total_elapsed = final_time - started
    total_ticks = max(0, final_ticks - process_tree_ticks_at_start)
    average = (total_ticks / clock_ticks) / total_elapsed * 100.0
    return average, percentile(interval_cpu, 0.95), len(interval_cpu)


def run_probe(
    mode: str,
    *,
    warmup_seconds: float,
    sample_seconds: float,
    interval_seconds: float,
    max_average_cpu_percent: float,
    max_p95_interval_cpu_percent: float,
    artifact_dir: Path,
) -> CpuResult:
    with tempfile.TemporaryDirectory(prefix=f"zara-cpu-{mode}-") as temp_root:
        temp_path = Path(temp_root)
        ready_path = temp_path / "ready"
        stdout_path = artifact_dir / f"desktop-idle-cpu-{mode}.stdout.log"
        stderr_path = artifact_dir / f"desktop-idle-cpu-{mode}.stderr.log"
        env = os.environ.copy()
        env.update(
            {
                "HOME": str(temp_path / "home"),
                "XDG_CONFIG_HOME": str(temp_path / "config"),
                "XDG_DATA_HOME": str(temp_path / "share"),
                "XDG_RUNTIME_DIR": str(temp_path / "run"),
                "ZARA_CPU_PROBE_READY": str(ready_path),
                "ZARA_CPU_PROBE_MODE": mode,
                "ZARA_CPU_PROBE_SECONDS": str(warmup_seconds + sample_seconds + 2.0),
            }
        )
        for directory in ("home", "config", "share", "run"):
            (temp_path / directory).mkdir(mode=0o700)

        with stdout_path.open("w") as stdout, stderr_path.open("w") as stderr:
            process = subprocess.Popen(
                [sys.executable, str(Path(__file__).resolve()), "--child"],
                cwd=REPO_ROOT,
                env=env,
                stdout=stdout,
                stderr=stderr,
                start_new_session=True,
            )
            try:
                wait_for_ready(process, ready_path, timeout=20.0)
                time.sleep(warmup_seconds)
                global process_tree_ticks_at_start
                process_tree_ticks_at_start = process_tree_ticks(process.pid)
                average, p95_interval, sample_count = sample_cpu(
                    process.pid,
                    sample_seconds=sample_seconds,
                    interval_seconds=interval_seconds,
                )
            finally:
                stop_process(process)

    passed = (
        average <= max_average_cpu_percent
        and p95_interval <= max_p95_interval_cpu_percent
    )
    return CpuResult(
        mode=mode,
        average_cpu_percent=round(average, 2),
        p95_interval_cpu_percent=round(p95_interval, 2),
        sample_seconds=sample_seconds,
        sample_count=sample_count,
        max_average_cpu_percent=max_average_cpu_percent,
        max_p95_interval_cpu_percent=max_p95_interval_cpu_percent,
        passed=passed,
    )


def child_main() -> int:
    os.environ.setdefault("QT_QPA_PLATFORM", "xcb" if os.environ.get("DISPLAY") else "offscreen")

    from PySide6.QtCore import QTimer

    from zara.client import ZaraClientState
    from zara.desktop.app import create_application
    from zara.desktop.windows import CopilotPresentation
    from zara.runtime import bridge as runtime_bridge
    from zara.runtime.commands import CommandReceipt

    class ProbeClient:
        def __init__(self) -> None:
            self._bus = runtime_bridge.RuntimeEventBus()

        @property
        def state(self):
            return ZaraClientState.READY

        def start(self):
            return completed(None)

        def submit(self, command):
            return completed(CommandReceipt(request_id=command.request_id))

        def subscribe(self, *, maxsize: int = 0):
            return self._bus.subscribe(maxsize=maxsize)

        def shutdown(self, reason: str = ""):
            return completed(None)

        def close(self, timeout=None) -> None:
            return None

    app, controller = create_application(["zara-cpu-probe"], client=ProbeClient())
    mode = os.environ.get("ZARA_CPU_PROBE_MODE", "visible")
    if mode == "expanded":
        controller.window.set_presentation(CopilotPresentation.EXPANDED)
        controller.show_quick_copilot()
    elif mode == "hidden":
        controller.show_quick_copilot()
        app.processEvents()
        controller.hide_quick_copilot()
    elif mode == "visible":
        controller.show_quick_copilot()
    else:
        raise ValueError(f"unsupported probe mode: {mode}")

    app.processEvents()
    ready_path = Path(os.environ["ZARA_CPU_PROBE_READY"])
    ready_path.touch()
    run_seconds = float(os.environ["ZARA_CPU_PROBE_SECONDS"])
    QTimer.singleShot(max(1, int(run_seconds * 1000)), app.quit)
    return int(app.exec())


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--child", action="store_true")
    parser.add_argument("--modes", nargs="+", choices=DEFAULT_MODES, default=list(DEFAULT_MODES))
    parser.add_argument("--warmup-seconds", type=float, default=3.0)
    parser.add_argument("--sample-seconds", type=float, default=6.0)
    parser.add_argument("--interval-seconds", type=float, default=0.25)
    parser.add_argument("--max-average-cpu-percent", type=float, default=35.0)
    parser.add_argument("--max-p95-interval-cpu-percent", type=float, default=85.0)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.child:
        return child_main()
    if sys.platform != "linux":
        raise SystemExit("desktop idle CPU probe requires Linux /proc")

    artifact_dir = Path(os.environ.get("ARTIFACT_DIR", "artifacts"))
    artifact_dir.mkdir(parents=True, exist_ok=True)
    results = [
        run_probe(
            mode,
            warmup_seconds=args.warmup_seconds,
            sample_seconds=args.sample_seconds,
            interval_seconds=args.interval_seconds,
            max_average_cpu_percent=args.max_average_cpu_percent,
            max_p95_interval_cpu_percent=args.max_p95_interval_cpu_percent,
            artifact_dir=artifact_dir,
        )
        for mode in args.modes
    ]
    report_path = artifact_dir / "desktop-idle-cpu.json"
    report_path.write_text(json.dumps([asdict(result) for result in results], indent=2) + "\n")

    for result in results:
        status = "PASS" if result.passed else "FAIL"
        print(
            f"{status} {result.mode}: avg={result.average_cpu_percent:.2f}% "
            f"p95={result.p95_interval_cpu_percent:.2f}% "
            f"budgets={result.max_average_cpu_percent:.2f}%/"
            f"{result.max_p95_interval_cpu_percent:.2f}%"
        )

    if all(result.passed for result in results):
        return 0
    print(f"CPU regression report: {report_path}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
