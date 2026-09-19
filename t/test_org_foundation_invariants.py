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
ORG_REPOSITORY_DECLARATION = re.compile(r"\binterface\s+OrgRepository\b")
ORG_TREE_REPOSITORY_DECLARATION = re.compile(r"\bclass\s+OrgTreeRepository\b")
GIT_ORG_WORKSPACE_DECLARATION = re.compile(r"\bclass\s+GitOrgWorkspace\b")
SYNC_GENERATION_FENCE_DECLARATION = re.compile(r"\bclass\s+SyncGenerationFence\b")


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


def test_android_org_storage_has_one_shared_authority() -> None:
    offenders: list[str] = []
    canonical_prefix = "android/org-storage/src/main/"

    for path in _android_production_sources():
        text = path.read_text(encoding="utf-8")
        if not (
            ORG_REPOSITORY_DECLARATION.search(text)
            or ORG_TREE_REPOSITORY_DECLARATION.search(text)
        ):
            continue
        rel = path.relative_to(REPO_ROOT).as_posix()
        if not rel.startswith(canonical_prefix):
            offenders.append(rel)

    assert not offenders, (
        "Android Org storage semantics must live in :org-storage; focused APKs "
        "must consume the shared repository instead of defining shadow stores:\n"
        + "\n".join(offenders)
    )


def test_android_org_git_sync_has_one_shared_engine() -> None:
    offenders: list[str] = []
    canonical_prefix = "android/org-sync-core/src/main/"

    for path in _android_production_sources():
        text = path.read_text(encoding="utf-8")
        if not (
            GIT_ORG_WORKSPACE_DECLARATION.search(text)
            or SYNC_GENERATION_FENCE_DECLARATION.search(text)
        ):
            continue
        rel = path.relative_to(REPO_ROOT).as_posix()
        if not rel.startswith(canonical_prefix):
            offenders.append(rel)

    assert not offenders, (
        "Android Org Git sync semantics must live in :org-sync-core; focused APKs "
        "must consume the shared sync engine instead of defining another one:\n"
        + "\n".join(offenders)
    )
