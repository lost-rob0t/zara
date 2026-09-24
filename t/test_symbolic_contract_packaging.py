from __future__ import annotations

import runpy
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]
CONTRACT_ROOT = ROOT / "contracts"


def _setup_kwargs() -> dict[str, object]:
    captured: dict[str, object] = {}

    def capture_setup(**kwargs: object) -> None:
        captured.update(kwargs)

    with patch("setuptools.setup", capture_setup):
        runpy.run_path(str(ROOT / "setup.py"), run_name="__main__")

    return captured


def _installed_data_paths() -> dict[str, str]:
    kwargs = _setup_kwargs()
    installed: dict[str, str] = {}
    for target, paths in kwargs["data_files"]:  # type: ignore[index]
        for path in paths:
            installed[str(path)] = str(target)
    return installed


def test_all_runtime_contracts_are_packaged_for_installed_runtime() -> None:
    installed = _installed_data_paths()
    contract_files = sorted(path for path in CONTRACT_ROOT.rglob("*") if path.is_file())
    assert contract_files, "canonical runtime contract tree must not be empty"

    for source in contract_files:
        relative = source.relative_to(ROOT)
        assert str(relative) in installed, f"runtime install omits {relative}"
        assert installed[str(relative)].endswith(
            f"share/zarathushtra/{relative.parent.as_posix()}"
        )


def test_runtime_contracts_are_packaged_in_source_distribution() -> None:
    manifest_lines = {
        line.strip()
        for line in (ROOT / "MANIFEST.in").read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    }

    assert "recursive-include contracts *" in manifest_lines
