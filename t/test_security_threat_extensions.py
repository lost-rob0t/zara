from __future__ import annotations

import ast
import runpy
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
EXTRA_THREATS = runpy.run_path(
    str(Path(__file__).with_name("security_threat_extensions.py"))
)["EXTRA_THREATS"]


def _top_level_tests(path: Path) -> set[str]:
    module = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    return {
        node.name
        for node in module.body
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        and node.name.startswith("test_")
    }


def test_compound_threat_catalog_is_unique_closed_and_sourced():
    ids = [item["id"] for item in EXTRA_THREATS]
    assert EXTRA_THREATS
    assert len(ids) == len(set(ids))
    for item in EXTRA_THREATS:
        assert set(item) == {"id", "scenario", "sources", "tests"}
        assert isinstance(item["scenario"], str) and item["scenario"].strip()
        assert isinstance(item["sources"], tuple) and item["sources"]
        assert isinstance(item["tests"], tuple) and item["tests"]


def test_every_compound_threat_reference_is_a_real_pytest_oracle():
    functions_by_path: dict[Path, set[str]] = {}
    missing: list[str] = []
    for item in EXTRA_THREATS:
        for reference in item["tests"]:
            path_text, separator, function = reference.partition("::")
            if separator != "::" or not function.startswith("test_"):
                missing.append(f"{item['id']}: malformed reference {reference}")
                continue
            path = ROOT / path_text
            if not path.is_file():
                missing.append(f"{item['id']}: missing file {path_text}")
                continue
            functions = functions_by_path.setdefault(path, _top_level_tests(path))
            if function not in functions:
                missing.append(f"{item['id']}: missing test {reference}")
    assert missing == []
