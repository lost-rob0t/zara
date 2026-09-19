from __future__ import annotations

import re
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
ANDROID_ROOT = REPO_ROOT / "android"
PYTHON_ROOT = REPO_ROOT / "zara"

# These are compatibility fixtures only. Production code must discover/configure
# workspace roots instead of baking an operator layout into the application.
FORBIDDEN_PRODUCTION_ROOT_FRAGMENTS = (
    "Documents/Notes/org",
    "Documents/gpt-todos",
)

ORG_PARSER_DECLARATION = re.compile(r"\b(?:class|object)\s+OrgParser\b")
ORG_TASK_DECLARATION = re.compile(r"\b(?:data\s+class|class|object)\s+OrgTask\b")


def _android_production_sources() -> list[Path]:
    return sorted(
        path
        for path in ANDROID_ROOT.rglob("*")
        if path.is_file()
        and path.suffix in {".kt", ".java"}
        and "/src/main/" in path.as_posix()
    )


def _python_production_sources() -> list[Path]:
    return sorted(path for path in PYTHON_ROOT.rglob("*.py") if path.is_file())


def test_org_workspace_roots_are_not_operator_specific_in_production() -> None:
    offenders: list[str] = []
    for path in [*_android_production_sources(), *_python_production_sources()]:
        text = path.read_text(encoding="utf-8")
        matched = [fragment for fragment in FORBIDDEN_PRODUCTION_ROOT_FRAGMENTS if fragment in text]
        if matched:
            rel = path.relative_to(REPO_ROOT).as_posix()
            offenders.append(f"{rel}: {', '.join(matched)}")

    assert not offenders, (
        "Org production code must accept arbitrary configured roots; compatibility "
        "layouts belong in tests/fixtures only:\n" + "\n".join(offenders)
    )


def test_android_org_parser_and_task_types_have_one_shared_authority() -> None:
    offenders: list[str] = []
    canonical_prefix = "android/org-core/src/main/"

    for path in _android_production_sources():
        text = path.read_text(encoding="utf-8")
        if not (ORG_PARSER_DECLARATION.search(text) or ORG_TASK_DECLARATION.search(text)):
            continue
        rel = path.relative_to(REPO_ROOT).as_posix()
        if not rel.startswith(canonical_prefix):
            offenders.append(rel)

    assert not offenders, (
        "Android Org parser/task semantics must live in :org-core; focused APKs "
        "consume that authority instead of defining shadow models:\n" + "\n".join(offenders)
    )
