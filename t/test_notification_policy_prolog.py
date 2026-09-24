from __future__ import annotations

import os
import subprocess
from pathlib import Path


def _run_swipl(tmp_path: Path, goal: str) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env["XDG_CONFIG_HOME"] = str(tmp_path / "xdg")
    config_dir = Path(env["XDG_CONFIG_HOME"]) / "zarathushtra"
    config_dir.mkdir(parents=True, exist_ok=True)
    return subprocess.run(
        ["swipl", "-q", "-g", goal, "-t", "halt"],
        cwd=Path(__file__).resolve().parents[1],
        env=env,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
        timeout=15,
    )


def test_notification_policy_is_user_reloadable_without_rebuild(tmp_path: Path) -> None:
    config_dir = tmp_path / "xdg" / "zarathushtra"
    config_dir.mkdir(parents=True)
    (config_dir / "config.pl").write_text(
        "kb_notification_policy:notification_route_policy(default, desktop_only).\n"
        "kb_notification_policy:notification_content_policy('com.example.chat', full_content).\n"
        "kb_notification_policy:notification_feedback('com.example.chat', always_allow).\n",
        encoding="utf-8",
    )
    goal = (
        "consult('main.pl'),"
        "kb_notification_policy:notification_route_target('com.example.chat', P1),"
        "kb_notification_policy:notification_content_mode('com.example.chat', C1),"
        "kb_notification_policy:notification_spam_decision('com.example.chat', 99, true, none, D1, R1),"
        "config_loader:user_local_config_path(Path),"
        "open(Path, write, S),"
        "write_term(S, kb_notification_policy:notification_route_policy(default, watch_only), [quoted(true)]),"
        "write(S, '.'),nl(S),close(S),"
        "config_loader:reload_user_config,"
        "kb_notification_policy:notification_route_target('com.example.chat', P2),"
        "format('~w|~w|~w|~w|~w~n',[P1,C1,P2,D1,R1])"
    )
    result = _run_swipl(tmp_path, goal)

    assert result.returncode == 0, result.stderr
    assert "desktop_only|full_content|watch_only|allow|explicit_always_allow" in result.stdout


def test_notification_config_rejects_untyped_executable_hook(tmp_path: Path) -> None:
    config_dir = tmp_path / "xdg" / "zarathushtra"
    config_dir.mkdir(parents=True)
    (config_dir / "config.pl").write_text(
        "kb_notification_policy:notification_hook(bad, any, shell('rm -rf /')).\n",
        encoding="utf-8",
    )

    # SWI reports exceptions from load-time initialization but keeps consult/1
    # successful. Invoke the loader directly as the contract under test so an
    # invalid executable-shaped hook is an unhandled, fail-closed error.
    result = _run_swipl(tmp_path, "consult('main.pl'),config_loader:load_user_config")

    assert result.returncode != 0
    assert "zarathushtra_user_config_fact" in result.stderr
