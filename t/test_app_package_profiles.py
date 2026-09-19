from __future__ import annotations

import pytest

from zara.runtime.package_profiles import (
    PACKAGE_PROFILE_SCHEMA,
    AppPackageProfile,
    PackageProfileError,
    activate_profile_package,
)
from zara.runtime.symbols import ProgrammableSymbolRegistry, SymbolLookupError, SymbolSpec


def test_profile_round_trip_is_portable_and_deterministic():
    profile = AppPackageProfile.from_mapping(
        {
            "schema": PACKAGE_PROFILE_SCHEMA,
            "app_id": "org_editor",
            "enabled_packages": ["org_roam", "logseq_daily"],
            "pins": {"logseq_daily": "1.4.2"},
        }
    )

    assert profile.app_id == "org_editor"
    assert profile.enabled_packages == ("logseq_daily", "org_roam")
    assert profile.pins == (("logseq_daily", "1.4.2"),)
    assert profile.to_mapping() == {
        "schema": PACKAGE_PROFILE_SCHEMA,
        "app_id": "org_editor",
        "enabled_packages": ["logseq_daily", "org_roam"],
        "pins": {"logseq_daily": "1.4.2"},
    }


def test_profile_rejects_duplicate_enablement_and_pin_for_disabled_package():
    with pytest.raises(PackageProfileError, match="duplicate enabled package"):
        AppPackageProfile.from_mapping(
            {
                "schema": PACKAGE_PROFILE_SCHEMA,
                "app_id": "org_editor",
                "enabled_packages": ["org_roam", "org_roam"],
                "pins": {},
            }
        )

    with pytest.raises(PackageProfileError, match="pin requires package to be enabled"):
        AppPackageProfile.from_mapping(
            {
                "schema": PACKAGE_PROFILE_SCHEMA,
                "app_id": "org_todo",
                "enabled_packages": ["gtd"],
                "pins": {"logseq_daily": "1.4.2"},
            }
        )


def test_direct_constructor_cannot_bypass_profile_validation():
    with pytest.raises(PackageProfileError, match="app_id"):
        AppPackageProfile(
            app_id="../shared",
            enabled_packages=("org_roam",),
        )

    with pytest.raises(PackageProfileError, match="package_id"):
        AppPackageProfile(
            app_id="org_editor",
            enabled_packages=("../escape",),
        )

    with pytest.raises(PackageProfileError, match="pin requires package to be enabled"):
        AppPackageProfile(
            app_id="org_editor",
            enabled_packages=("org_roam",),
            pins=(("logseq_daily", "1.4.2"),),
        )


def test_profile_package_count_bound_fails_closed():
    packages = [f"pkg_{index}" for index in range(257)]

    with pytest.raises(PackageProfileError, match="too many enabled packages"):
        AppPackageProfile.from_mapping(
            {
                "schema": PACKAGE_PROFILE_SCHEMA,
                "app_id": "org_editor",
                "enabled_packages": packages,
                "pins": {},
            }
        )

    with pytest.raises(PackageProfileError, match="too many enabled packages"):
        AppPackageProfile(
            app_id="org_editor",
            enabled_packages=tuple(packages),
        )


def test_activation_reuses_app_registry_and_never_creates_global_package_state():
    editor_profile = AppPackageProfile.from_mapping(
        {
            "schema": PACKAGE_PROFILE_SCHEMA,
            "app_id": "org_editor",
            "enabled_packages": ["logseq_daily"],
            "pins": {},
        }
    )
    todo_profile = AppPackageProfile.from_mapping(
        {
            "schema": PACKAGE_PROFILE_SCHEMA,
            "app_id": "org_todo",
            "enabled_packages": ["logseq_daily"],
            "pins": {},
        }
    )
    editor_registry = ProgrammableSymbolRegistry()
    todo_registry = ProgrammableSymbolRegistry()

    activate_profile_package(
        editor_registry,
        editor_profile,
        "logseq_daily",
        (SymbolSpec("org:daily/open", "command", "editor-daily"),),
    )
    activate_profile_package(
        todo_registry,
        todo_profile,
        "logseq_daily",
        (SymbolSpec("org:daily/open", "command", "todo-daily"),),
    )

    assert editor_registry.get("org:daily/open") == "editor-daily"
    assert todo_registry.get("org:daily/open") == "todo-daily"
    assert editor_registry.describe("org:daily/open")[0].owner == (
        "app:org_editor:package:logseq_daily"
    )
    assert todo_registry.describe("org:daily/open")[0].owner == (
        "app:org_todo:package:logseq_daily"
    )

    editor_registry.clear_owner("app:org_editor:package:logseq_daily")
    with pytest.raises(SymbolLookupError):
        editor_registry.resolve("org:daily/open")
    assert todo_registry.get("org:daily/open") == "todo-daily"


def test_activation_enforces_app_local_version_pin_before_registry_mutation():
    profile = AppPackageProfile.from_mapping(
        {
            "schema": PACKAGE_PROFILE_SCHEMA,
            "app_id": "org_editor",
            "enabled_packages": ["logseq_daily"],
            "pins": {"logseq_daily": "1.4.2"},
        }
    )
    registry = ProgrammableSymbolRegistry()
    specs = (SymbolSpec("org:daily/open", "command", "editor-daily"),)

    with pytest.raises(PackageProfileError, match="requires package_version"):
        activate_profile_package(registry, profile, "logseq_daily", specs)
    assert registry.symbols() == ()

    with pytest.raises(PackageProfileError, match="does not match pinned version"):
        activate_profile_package(
            registry,
            profile,
            "logseq_daily",
            specs,
            package_version="1.4.1",
        )
    assert registry.symbols() == ()

    activate_profile_package(
        registry,
        profile,
        "logseq_daily",
        specs,
        package_version="1.4.2",
    )
    assert registry.get("org:daily/open") == "editor-daily"


def test_activation_fails_closed_for_package_not_enabled_in_target_app():
    profile = AppPackageProfile.from_mapping(
        {
            "schema": PACKAGE_PROFILE_SCHEMA,
            "app_id": "org_todo",
            "enabled_packages": ["gtd"],
            "pins": {},
        }
    )
    registry = ProgrammableSymbolRegistry()

    with pytest.raises(PackageProfileError, match="not enabled for app 'org_todo'"):
        activate_profile_package(
            registry,
            profile,
            "logseq_daily",
            (SymbolSpec("org:daily/open", "command", "wrong-target"),),
        )

    assert registry.symbols() == ()


def test_profile_rejects_unknown_schema_and_ambient_fields():
    with pytest.raises(PackageProfileError, match="unsupported package profile schema"):
        AppPackageProfile.from_mapping(
            {
                "schema": 99,
                "app_id": "org_editor",
                "enabled_packages": [],
                "pins": {},
            }
        )

    with pytest.raises(PackageProfileError, match="unknown package profile fields"):
        AppPackageProfile.from_mapping(
            {
                "schema": PACKAGE_PROFILE_SCHEMA,
                "app_id": "org_editor",
                "enabled_packages": [],
                "pins": {},
                "global_registry": True,
            }
        )


def test_package_versions_reject_path_shaped_and_nonportable_tokens_before_mutation():
    with pytest.raises(PackageProfileError, match="package pin version"):
        AppPackageProfile.from_mapping(
            {
                "schema": PACKAGE_PROFILE_SCHEMA,
                "app_id": "org_editor",
                "enabled_packages": ["logseq_daily"],
                "pins": {"logseq_daily": "../1.4.2"},
            }
        )

    profile = AppPackageProfile.from_mapping(
        {
            "schema": PACKAGE_PROFILE_SCHEMA,
            "app_id": "org_editor",
            "enabled_packages": ["logseq_daily"],
            "pins": {},
        }
    )
    registry = ProgrammableSymbolRegistry()
    specs = (SymbolSpec("org:daily/open", "command", "editor-daily"),)

    for invalid_version in ("1.4.2/escape", "1.4.2\\escape", "1.4.2-β"):
        with pytest.raises(PackageProfileError, match="package pin version"):
            activate_profile_package(
                registry,
                profile,
                "logseq_daily",
                specs,
                package_version=invalid_version,
            )
        assert registry.symbols() == ()
