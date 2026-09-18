"""Strict Zara product/release version context."""

from __future__ import annotations

import argparse
import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence


SEMVER_RE = re.compile(
    r"^(?P<major>0|[1-9][0-9]*)\."
    r"(?P<minor>0|[1-9][0-9]*)\."
    r"(?P<patch>0|[1-9][0-9]*)"
    r"(?:-(?P<prerelease>[0-9A-Za-z.-]+))?"
    r"(?:\+(?P<build>[0-9A-Za-z.-]+))?$"
)
EXPECTED_KEYS = {
    "schema",
    "zara.version",
    "android.versionCode",
    "release.target",
    "release.targetAndroidVersionCode",
}
ANDROID_VERSION_CODE_MAX = 2_100_000_000


class VersionContextError(ValueError):
    pass


@dataclass(frozen=True)
class VersionContext:
    schema: int
    version: str
    android_version_code: int
    release_target: str
    release_target_android_version_code: int
    python_version: str

    @property
    def tag(self) -> str:
        return f"v{self.version}"

    @property
    def release_ready(self) -> bool:
        return (
            self.version == self.release_target
            and self.android_version_code == self.release_target_android_version_code
        )

    def as_dict(self) -> dict[str, object]:
        return {
            "schema": self.schema,
            "version": self.version,
            "android_version_code": self.android_version_code,
            "release_target": self.release_target,
            "release_target_android_version_code": self.release_target_android_version_code,
            "python_version": self.python_version,
            "tag": self.tag,
            "release_ready": self.release_ready,
        }


def _parse_properties(path: Path) -> dict[str, str]:
    if not path.is_file():
        raise VersionContextError(f"Version context is missing: {path}")

    values: dict[str, str] = {}
    for line_number, raw_line in enumerate(path.read_text().splitlines(), 1):
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        if "=" not in line:
            raise VersionContextError(
                f"Malformed version context line {line_number}: {raw_line!r}"
            )
        key, value = (part.strip() for part in line.split("=", 1))
        if not key or not value:
            raise VersionContextError(
                f"Empty version context key/value on line {line_number}"
            )
        if key in values:
            raise VersionContextError(f"Duplicate version context key: {key}")
        values[key] = value

    missing = EXPECTED_KEYS - values.keys()
    unknown = values.keys() - EXPECTED_KEYS
    if missing:
        raise VersionContextError(
            "Missing version context keys: " + ", ".join(sorted(missing))
        )
    if unknown:
        raise VersionContextError(
            "Unknown version context keys: " + ", ".join(sorted(unknown))
        )
    return values


def _parse_semver(value: str) -> tuple[tuple[int, int, int], tuple[str, ...], str | None]:
    match = SEMVER_RE.fullmatch(value)
    if match is None:
        raise VersionContextError(f"Invalid SemVer: {value!r}")

    prerelease_text = match.group("prerelease")
    prerelease = tuple(prerelease_text.split(".")) if prerelease_text else ()
    for identifier in prerelease:
        if not identifier:
            raise VersionContextError(f"Invalid SemVer prerelease: {value!r}")
        if identifier.isdigit() and len(identifier) > 1 and identifier.startswith("0"):
            raise VersionContextError(
                f"Numeric SemVer prerelease identifiers cannot have leading zeros: {value!r}"
            )

    build = match.group("build")
    if build is not None and any(not part for part in build.split(".")):
        raise VersionContextError(f"Invalid SemVer build metadata: {value!r}")

    core = (
        int(match.group("major")),
        int(match.group("minor")),
        int(match.group("patch")),
    )
    return core, prerelease, build


def _compare_prerelease(left: tuple[str, ...], right: tuple[str, ...]) -> int:
    if not left and not right:
        return 0
    if not left:
        return 1
    if not right:
        return -1

    for left_part, right_part in zip(left, right):
        if left_part == right_part:
            continue
        left_numeric = left_part.isdigit()
        right_numeric = right_part.isdigit()
        if left_numeric and right_numeric:
            return -1 if int(left_part) < int(right_part) else 1
        if left_numeric != right_numeric:
            return -1 if left_numeric else 1
        return -1 if left_part < right_part else 1

    if len(left) == len(right):
        return 0
    return -1 if len(left) < len(right) else 1


def compare_semver(left: str, right: str) -> int:
    left_core, left_pre, _ = _parse_semver(left)
    right_core, right_pre, _ = _parse_semver(right)
    if left_core != right_core:
        return -1 if left_core < right_core else 1
    return _compare_prerelease(left_pre, right_pre)


def _android_version_code(value: str, key: str) -> int:
    if not value.isdigit():
        raise VersionContextError(f"{key} must be a positive decimal integer")
    parsed = int(value)
    if parsed < 1 or parsed > ANDROID_VERSION_CODE_MAX:
        raise VersionContextError(
            f"{key} must be between 1 and {ANDROID_VERSION_CODE_MAX}"
        )
    return parsed


def _python_version(version: str) -> str:
    core, prerelease, build = _parse_semver(version)
    base = ".".join(str(part) for part in core)
    if not prerelease:
        projected = base
    else:
        label = prerelease[0].lower()
        labels = {
            "alpha": "a",
            "a": "a",
            "beta": "b",
            "b": "b",
            "rc": "rc",
        }
        if label not in labels or len(prerelease) > 2:
            raise VersionContextError(
                f"Zara prerelease cannot be projected to PEP 440: {version!r}"
            )
        serial = "0"
        if len(prerelease) == 2:
            if not prerelease[1].isdigit():
                raise VersionContextError(
                    f"Zara prerelease serial must be numeric for Python packaging: {version!r}"
                )
            serial = str(int(prerelease[1]))
        projected = f"{base}{labels[label]}{serial}"

    if build:
        projected += "+" + build.lower().replace("-", ".")
    return projected


def load_version_context(path: Path | None = None) -> VersionContext:
    version_path = path or Path(__file__).resolve().parents[1] / "version.properties"
    values = _parse_properties(version_path)

    if values["schema"] != "1":
        raise VersionContextError(
            f"Unsupported version context schema: {values['schema']!r}"
        )

    version = values["zara.version"]
    release_target = values["release.target"]
    _parse_semver(version)
    _parse_semver(release_target)

    version_code = _android_version_code(
        values["android.versionCode"], "android.versionCode"
    )
    target_version_code = _android_version_code(
        values["release.targetAndroidVersionCode"],
        "release.targetAndroidVersionCode",
    )

    precedence = compare_semver(release_target, version)
    if precedence < 0:
        raise VersionContextError(
            f"release.target {release_target} cannot precede current Zara version {version}"
        )
    if precedence == 0 and target_version_code != version_code:
        raise VersionContextError(
            "Equal current/target versions must use the same Android versionCode"
        )
    if precedence > 0 and target_version_code <= version_code:
        raise VersionContextError(
            "A newer release.target requires a strictly newer Android versionCode"
        )

    return VersionContext(
        schema=1,
        version=version,
        android_version_code=version_code,
        release_target=release_target,
        release_target_android_version_code=target_version_code,
        python_version=_python_version(version),
    )


def _render(context: VersionContext, output_format: str) -> str:
    if output_format == "json":
        return json.dumps(context.as_dict(), sort_keys=True)
    if output_format == "github-output":
        values = {
            "version": context.version,
            "version_code": context.android_version_code,
            "tag": context.tag,
            "release_target": context.release_target,
            "release_target_version_code": context.release_target_android_version_code,
            "release_ready": str(context.release_ready).lower(),
            "python_version": context.python_version,
        }
        return "\n".join(f"{key}={value}" for key, value in values.items())
    return (
        f"current={context.tag} android_version_code={context.android_version_code}\n"
        f"target=v{context.release_target} "
        f"target_android_version_code={context.release_target_android_version_code} "
        f"release_ready={str(context.release_ready).lower()}"
    )


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--path",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "version.properties",
    )
    parser.add_argument(
        "--format",
        choices=("plain", "json", "github-output"),
        default="plain",
    )
    args = parser.parse_args(argv)

    context = load_version_context(args.path)
    print(_render(context, args.format))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
