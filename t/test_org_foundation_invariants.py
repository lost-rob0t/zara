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

FORBIDDEN_ORG_STORAGE_MARKERS = (
    "android.permission.MANAGE_EXTERNAL_STORAGE",
    "android.permission.READ_EXTERNAL_STORAGE",
    "android.permission.WRITE_EXTERNAL_STORAGE",
    "/storage/emulated/",
    "/sdcard/",
)

ORG_PARSER_DECLARATION = re.compile(r"\b(?:class|object)\s+OrgParser\b")
ORG_TASK_DECLARATION = re.compile(r"\b(?:data\s+class|class|object)\s+OrgTask\b")
ORG_REPOSITORY_DECLARATION = re.compile(r"\binterface\s+OrgRepository\b")
ORG_TREE_REPOSITORY_DECLARATION = re.compile(r"\bclass\s+OrgTreeRepository\b")
ORG_HOME_DECLARATION = re.compile(r"\bobject\s+OrgHome\b")
GIT_ORG_WORKSPACE_DECLARATION = re.compile(r"\bclass\s+GitOrgWorkspace\b")
SYNC_GENERATION_FENCE_DECLARATION = re.compile(r"\bclass\s+SyncGenerationFence\b")
ORG_SYNC_PROVIDER_DECLARATION = re.compile(r"\bclass\s+OrgSyncProvider\b")
SHARED_WORKSPACE_STORE_DECLARATION = re.compile(r"\bclass\s+SharedWorkspaceStore\b")
ORG_WORKSPACE_STORE_DECLARATION = re.compile(r"\b(?:class|object)\s+OrgWorkspaceStore\b")
ORG_WORKSPACE_REGISTRY_DECLARATION = re.compile(r"\b(?:class|object)\s+OrgWorkspaceRegistry\b")


def _android_production_sources() -> list[Path]:
    return sorted(
        path
        for path in ANDROID_ROOT.rglob("*")
        if path.is_file()
        and path.suffix in {".kt", ".java"}
        and "/src/main/" in path.as_posix()
    )


def _android_production_text_files() -> list[Path]:
    """Production Android text that can carry a path/default into the APK."""
    return sorted(
        path
        for path in ANDROID_ROOT.rglob("*")
        if path.is_file()
        and path.suffix in {".kt", ".java", ".xml", ".json", ".properties", ".txt"}
        and "/src/main/" in path.as_posix()
    )


def _android_org_production_files() -> list[Path]:
    return sorted(
        path
        for path in ANDROID_ROOT.rglob("*")
        if path.is_file()
        and path.suffix in {".kt", ".java", ".xml"}
        and "/src/main/" in path.as_posix()
        and any(part.startswith("org-") for part in path.relative_to(ANDROID_ROOT).parts)
    )


def _python_production_sources() -> list[Path]:
    return sorted(path for path in PYTHON_ROOT.rglob("*.py") if path.is_file())


def test_org_workspace_roots_are_not_operator_specific_in_production() -> None:
    offenders: list[str] = []
    for path in [*_android_production_text_files(), *_python_production_sources()]:
        text = path.read_text(encoding="utf-8")
        matched = [fragment for fragment in FORBIDDEN_PRODUCTION_ROOT_FRAGMENTS if fragment in text]
        if matched:
            rel = path.relative_to(REPO_ROOT).as_posix()
            offenders.append(f"{rel}: {', '.join(matched)}")

    assert not offenders, (
        "Org production code/resources must accept arbitrary configured roots; compatibility "
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
            or ORG_HOME_DECLARATION.search(text)
        ):
            continue
        rel = path.relative_to(REPO_ROOT).as_posix()
        if not rel.startswith(canonical_prefix):
            offenders.append(rel)

    assert not offenders, (
        "Android Org storage/home semantics must live in :org-storage; focused APKs "
        "must consume the shared repository/home instead of defining shadow stores:\n"
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


def test_android_org_shared_workspace_provider_has_one_authority() -> None:
    offenders: list[str] = []
    canonical_prefix = "android/org-sync/src/main/"

    for path in _android_production_sources():
        text = path.read_text(encoding="utf-8")
        if not (
            ORG_SYNC_PROVIDER_DECLARATION.search(text)
            or SHARED_WORKSPACE_STORE_DECLARATION.search(text)
            or ORG_WORKSPACE_STORE_DECLARATION.search(text)
            or ORG_WORKSPACE_REGISTRY_DECLARATION.search(text)
        ):
            continue
        rel = path.relative_to(REPO_ROOT).as_posix()
        if not rel.startswith(canonical_prefix):
            offenders.append(rel)

    assert not offenders, (
        "Android shared Org workspace selection/provider/registry state must be owned by "
        ":org-sync; focused APKs consume the signature-scoped provider instead of creating "
        "app-local workspace stores or registries:\n" + "\n".join(offenders)
    )


def test_android_org_storage_stays_saf_scoped_and_user_visible() -> None:
    offenders: list[str] = []

    for path in _android_org_production_files():
        text = path.read_text(encoding="utf-8")
        matched = [marker for marker in FORBIDDEN_ORG_STORAGE_MARKERS if marker in text]
        if "appDataFolder" in text:
            matched.append("appDataFolder")
        if matched:
            rel = path.relative_to(REPO_ROOT).as_posix()
            offenders.append(f"{rel}: {', '.join(matched)}")

    assert not offenders, (
        "Org APKs must keep canonical files in user-visible SAF/configured workspaces; "
        "broad external-storage paths/permissions and Drive appDataFolder are not valid "
        "canonical Org homes:\n" + "\n".join(offenders)
    )
