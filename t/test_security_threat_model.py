from __future__ import annotations

import ast
import itertools
import runpy
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CATALOG = runpy.run_path(str(Path(__file__).with_name("security_threat_catalog.py")))
THREATS = CATALOG["THREATS"]
STRIDE = CATALOG["STRIDE"]
BOUNDARIES = CATALOG["BOUNDARIES"]
ACTORS = CATALOG["ACTORS"]


def _test_reference(reference: str) -> tuple[Path, str]:
    path_text, separator, function = reference.partition("::")
    assert separator == "::", f"threat test reference must use path::function: {reference!r}"
    assert function.startswith("test_"), f"threat oracle must be a pytest test: {reference!r}"
    path = ROOT / path_text
    return path, function


def _top_level_functions(path: Path) -> set[str]:
    module = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    return {
        node.name
        for node in module.body
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
    }


def test_threat_catalog_has_unique_closed_identifiers_and_fields():
    expected_fields = {
        "id",
        "boundary",
        "stride",
        "actor",
        "scenario",
        "tests",
        "sources",
        "disposition",
        "rationale",
    }
    ids = [item["id"] for item in THREATS]
    assert len(ids) == len(set(ids))
    assert THREATS
    for item in THREATS:
        assert set(item) == expected_fields
        assert isinstance(item["id"], str) and item["id"]
        assert item["boundary"] in BOUNDARIES
        assert item["stride"] in STRIDE
        assert item["actor"] in ACTORS
        assert isinstance(item["scenario"], str) and item["scenario"].strip()
        assert item["disposition"] in {"tested", "out_of_scope"}
        assert isinstance(item["tests"], tuple)
        assert isinstance(item["sources"], tuple) and item["sources"]


def test_every_boundary_has_every_stride_prompt_modeled():
    expected = set(itertools.product(BOUNDARIES, STRIDE))
    actual = {(item["boundary"], item["stride"]) for item in THREATS}
    assert actual == expected


def test_every_applicable_threat_has_at_least_one_deterministic_test_oracle():
    missing = [item["id"] for item in THREATS if item["disposition"] == "tested" and not item["tests"]]
    assert missing == []


def test_every_out_of_scope_threat_has_an_explicit_bounded_rationale():
    invalid = []
    for item in THREATS:
        if item["disposition"] != "out_of_scope":
            continue
        rationale = item["rationale"]
        if not isinstance(rationale, str) or len(rationale.strip()) < 40:
            invalid.append(item["id"])
    assert invalid == []


def test_every_threat_test_reference_points_to_a_real_pytest_function():
    functions_by_path: dict[Path, set[str]] = {}
    missing: list[str] = []
    for item in THREATS:
        for reference in item["tests"]:
            path, function = _test_reference(reference)
            if not path.is_file():
                missing.append(f"{item['id']}: missing file {path.relative_to(ROOT)}")
                continue
            functions = functions_by_path.setdefault(path, _top_level_functions(path))
            if function not in functions:
                missing.append(f"{item['id']}: missing test {reference}")
    assert missing == []


def test_threat_model_exercises_every_declared_actor_and_boundary():
    assert {item["actor"] for item in THREATS} == set(ACTORS)
    assert {item["boundary"] for item in THREATS} == set(BOUNDARIES)
