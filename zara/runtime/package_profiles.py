"""Portable per-application package profile contract.

Profiles are immutable configuration. They never own a global registry: each app
passes its own canonical ``ProgrammableSymbolRegistry`` when activating package
exports. This keeps package enablement and symbol generations isolated by app
while reusing the same package/symbol ABI everywhere.
"""

from __future__ import annotations

from dataclasses import dataclass
import re
from typing import Iterable, Mapping

from .symbols import ProgrammableSymbolRegistry, SymbolSpec


PACKAGE_PROFILE_SCHEMA = 1
_MAX_ID_LENGTH = 48
_MAX_VERSION_LENGTH = 128
_MAX_PACKAGES = 256
_ID_RE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._-]*\Z")
_PROFILE_FIELDS = frozenset({"schema", "app_id", "enabled_packages", "pins"})


class PackageProfileError(ValueError):
    """Raised when a portable app package profile is invalid."""


def _bounded_id(value: object, *, label: str) -> str:
    if (
        not isinstance(value, str)
        or not value
        or len(value) > _MAX_ID_LENGTH
        or _ID_RE.fullmatch(value) is None
    ):
        raise PackageProfileError(
            f"{label} must be a non-empty {_MAX_ID_LENGTH}-character portable id"
        )
    return value


def _bounded_version(value: object) -> str:
    if (
        not isinstance(value, str)
        or not value
        or len(value) > _MAX_VERSION_LENGTH
        or value != value.strip()
        or any(
            ord(character) < 0x21
            or ord(character) > 0x7E
            or character in "/\\"
            for character in value
        )
    ):
        raise PackageProfileError("package pin version must be a bounded portable token")
    return value


@dataclass(frozen=True)
class AppPackageProfile:
    """Validated portable package enablement for exactly one Zara app."""

    app_id: str
    enabled_packages: tuple[str, ...]
    pins: tuple[tuple[str, str], ...] = ()
    schema: int = PACKAGE_PROFILE_SCHEMA

    def __post_init__(self) -> None:
        """Enforce the profile ABI even for direct Python construction.

        ``from_mapping`` is the normal wire/config entry point, but callers may
        instantiate the dataclass directly. Keeping validation here prevents a
        direct constructor from bypassing app/package namespace or pin rules.
        """

        if type(self.schema) is not int or self.schema != PACKAGE_PROFILE_SCHEMA:
            raise PackageProfileError(
                f"unsupported package profile schema: {self.schema!r}"
            )

        app_id = _bounded_id(self.app_id, label="app_id")

        if not isinstance(self.enabled_packages, (list, tuple)):
            raise PackageProfileError("enabled_packages must be a list")
        if len(self.enabled_packages) > _MAX_PACKAGES:
            raise PackageProfileError("too many enabled packages")

        enabled: list[str] = []
        seen: set[str] = set()
        for raw_package_id in self.enabled_packages:
            package_id = _bounded_id(raw_package_id, label="package_id")
            if package_id in seen:
                raise PackageProfileError(
                    f"duplicate enabled package: {package_id}"
                )
            seen.add(package_id)
            enabled.append(package_id)

        if not isinstance(self.pins, (list, tuple)):
            raise PackageProfileError("pins must contain package/version pairs")
        if len(self.pins) > _MAX_PACKAGES:
            raise PackageProfileError("too many package pins")

        pins: list[tuple[str, str]] = []
        pinned: set[str] = set()
        for raw_pin in self.pins:
            if not isinstance(raw_pin, (list, tuple)) or len(raw_pin) != 2:
                raise PackageProfileError("pins must contain package/version pairs")
            raw_package_id, raw_version = raw_pin
            package_id = _bounded_id(raw_package_id, label="package_id")
            if package_id in pinned:
                raise PackageProfileError(f"duplicate package pin: {package_id}")
            if package_id not in seen:
                raise PackageProfileError(
                    f"pin requires package to be enabled: {package_id}"
                )
            pinned.add(package_id)
            pins.append((package_id, _bounded_version(raw_version)))

        object.__setattr__(self, "app_id", app_id)
        object.__setattr__(self, "enabled_packages", tuple(sorted(enabled)))
        object.__setattr__(self, "pins", tuple(sorted(pins)))

    @classmethod
    def from_mapping(cls, raw: Mapping[str, object]) -> "AppPackageProfile":
        if not isinstance(raw, Mapping):
            raise PackageProfileError("package profile must be a mapping")

        unknown = sorted(set(raw) - _PROFILE_FIELDS)
        if unknown:
            raise PackageProfileError(
                f"unknown package profile fields: {', '.join(unknown)}"
            )

        schema = raw.get("schema")
        if type(schema) is not int or schema != PACKAGE_PROFILE_SCHEMA:
            raise PackageProfileError(
                f"unsupported package profile schema: {schema!r}"
            )

        app_id = _bounded_id(raw.get("app_id"), label="app_id")

        raw_enabled = raw.get("enabled_packages")
        if not isinstance(raw_enabled, (list, tuple)):
            raise PackageProfileError("enabled_packages must be a list")
        if len(raw_enabled) > _MAX_PACKAGES:
            raise PackageProfileError("too many enabled packages")

        enabled: list[str] = []
        seen: set[str] = set()
        for raw_package_id in raw_enabled:
            package_id = _bounded_id(raw_package_id, label="package_id")
            if package_id in seen:
                raise PackageProfileError(
                    f"duplicate enabled package: {package_id}"
                )
            seen.add(package_id)
            enabled.append(package_id)

        raw_pins = raw.get("pins")
        if not isinstance(raw_pins, Mapping):
            raise PackageProfileError("pins must be a mapping")
        if len(raw_pins) > _MAX_PACKAGES:
            raise PackageProfileError("too many package pins")

        pins: list[tuple[str, str]] = []
        for raw_package_id, raw_version in raw_pins.items():
            package_id = _bounded_id(raw_package_id, label="package_id")
            if package_id not in seen:
                raise PackageProfileError(
                    f"pin requires package to be enabled: {package_id}"
                )
            pins.append((package_id, _bounded_version(raw_version)))

        return cls(
            app_id=app_id,
            enabled_packages=tuple(sorted(enabled)),
            pins=tuple(sorted(pins)),
            schema=PACKAGE_PROFILE_SCHEMA,
        )

    def to_mapping(self) -> dict[str, object]:
        return {
            "schema": self.schema,
            "app_id": self.app_id,
            "enabled_packages": list(self.enabled_packages),
            "pins": dict(self.pins),
        }

    def enables(self, package_id: str) -> bool:
        package_id = _bounded_id(package_id, label="package_id")
        return package_id in self.enabled_packages

    def pinned_version(self, package_id: str) -> str | None:
        package_id = _bounded_id(package_id, label="package_id")
        return dict(self.pins).get(package_id)

    def owner_for(self, package_id: str) -> str:
        package_id = _bounded_id(package_id, label="package_id")
        if package_id not in self.enabled_packages:
            raise PackageProfileError(
                f"package {package_id!r} is not enabled for app {self.app_id!r}"
            )
        return f"app:{self.app_id}:package:{package_id}"


def activate_profile_package(
    registry: ProgrammableSymbolRegistry,
    profile: AppPackageProfile,
    package_id: str,
    specs: Iterable[SymbolSpec],
    *,
    package_version: str | None = None,
) -> tuple[int, ...]:
    """Replace one package generation in the target app's existing registry.

    The caller supplies the registry deliberately. No process-global registry is
    created or consulted here, so sibling apps remain isolated even when they
    enable the same portable package. A profile pin is enforced before any live
    registry mutation, keeping a failed or mismatched activation failure-atomic.
    """

    if not isinstance(registry, ProgrammableSymbolRegistry):
        raise PackageProfileError("activation requires the app symbol registry")
    if not isinstance(profile, AppPackageProfile):
        raise PackageProfileError("activation requires an AppPackageProfile")

    owner = profile.owner_for(package_id)
    pinned_version = profile.pinned_version(package_id)
    if pinned_version is not None:
        if package_version is None:
            raise PackageProfileError(
                f"package {package_id!r} is pinned to {pinned_version!r}; "
                "activation requires package_version"
            )
        normalized_version = _bounded_version(package_version)
        if normalized_version != pinned_version:
            raise PackageProfileError(
                f"package {package_id!r} version {normalized_version!r} "
                f"does not match pinned version {pinned_version!r}"
            )
    elif package_version is not None:
        _bounded_version(package_version)

    return registry.replace_owner(owner, tuple(specs), layer="package")


__all__ = [
    "PACKAGE_PROFILE_SCHEMA",
    "AppPackageProfile",
    "PackageProfileError",
    "activate_profile_package",
]
