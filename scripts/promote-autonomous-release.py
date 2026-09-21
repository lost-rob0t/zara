#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
from pathlib import Path

SEMVER = re.compile(
    r"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)"
    r"(?:-[0-9A-Za-z]+(?:[.-][0-9A-Za-z]+)*)?"
    r"(?:\+[0-9A-Za-z]+(?:[.-][0-9A-Za-z]+)*)?$"
)
EXPECTED = (
    "schema",
    "zara.version",
    "android.versionCode",
    "release.target",
    "release.targetAndroidVersionCode",
)


def read_properties(path: Path) -> dict[str, str]:
    out: dict[str, str] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        key, value = line.split("=", 1)
        out[key.strip()] = value.strip()
    if set(out) != set(EXPECTED):
        raise ValueError("version.properties keys do not match canonical schema")
    return out


def promoted_text(plan: dict, current: dict[str, str]) -> str:
    if plan.get("schema") != 1:
        raise ValueError("plan schema must be 1")
    target = str(plan["target_version"])
    target_code = int(plan["target_android_version_code"])
    if not SEMVER.fullmatch(target):
        raise ValueError("target_version is not SemVer")
    if target_code < 1:
        raise ValueError("target_android_version_code must be positive")
    current_code = int(current["android.versionCode"])
    if target_code < current_code:
        raise ValueError("target Android version code regresses current source")
    return (
        "schema=1\n"
        f"zara.version={target}\n"
        f"android.versionCode={target_code}\n"
        f"release.target={target}\n"
        f"release.targetAndroidVersionCode={target_code}\n"
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--plan", type=Path, required=True)
    parser.add_argument("--version-file", type=Path, required=True)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    plan = json.loads(args.plan.read_text(encoding="utf-8"))
    if not isinstance(plan, dict):
        raise ValueError("plan must be a JSON object")
    current = read_properties(args.version_file)
    text = promoted_text(plan, current)
    target = args.output or args.version_file
    target.write_text(text, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
