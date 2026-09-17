#!/usr/bin/env python3
"""Stage an unsigned F-Droid-compatible Zara binary repository.

This command is intentionally incapable of signing a repository. It consumes the
exact APK + provenance manifests produced by Zara's Android release/CI gates,
verifies those bytes again, writes minimal F-Droid binary metadata, and invokes
`fdroid update --nosign`.

A separately authorized publication process owns repository signing.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
from urllib.parse import urlparse


ROOT = Path(__file__).resolve().parents[1]
CONFIG_TEMPLATE = ROOT / "android/fdroid/config.yml.in"
SOURCE_REPO = "https://github.com/lost-rob0t/zara"
ISSUE_TRACKER = f"{SOURCE_REPO}/issues"
SEMVER = re.compile(
    r"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)"
    r"(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?$"
)
SHA256 = re.compile(r"^[0-9a-f]{64}$")
SOURCE_SHA = re.compile(r"^[0-9a-f]{40}$")
PACKAGE_NAME = re.compile(r"^[A-Za-z][A-Za-z0-9_]*(?:\.[A-Za-z][A-Za-z0-9_]*)+$")
FORBIDDEN_SIGNING_ENV = (
    "FDROID_KEY_STORE_PASS",
    "FDROID_KEY_PASS",
    "ZARA_FDROID_KEYSTORE",
    "KEYSTOREPASS",
    "KEYPASS",
)
ALLOWED_MANIFEST_FIELDS = {
    "schema",
    "source_sha",
    "package_name",
    "version_name",
    "version_code",
    "release_status",
    "apk",
    "apk_sha256",
}


class StageError(ValueError):
    pass


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def parse_manifest(path: Path) -> dict[str, str]:
    values: dict[str, str] = {}
    for number, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        line = raw.strip()
        if not line:
            continue
        if "=" not in line:
            raise StageError(f"{path}:{number}: manifest line must be key=value")
        key, value = line.split("=", 1)
        if not key or key in values:
            raise StageError(f"{path}:{number}: duplicate/blank manifest key: {key!r}")
        if key not in ALLOWED_MANIFEST_FIELDS:
            raise StageError(f"{path}:{number}: unsupported manifest field: {key}")
        values[key] = value

    required = {
        "schema",
        "source_sha",
        "package_name",
        "version_name",
        "version_code",
        "apk",
        "apk_sha256",
    }
    missing = sorted(required - values.keys())
    if missing:
        raise StageError(f"{path}: missing manifest fields: {', '.join(missing)}")
    if values["schema"] != "1":
        raise StageError(f"{path}: unsupported manifest schema: {values['schema']}")
    if not SOURCE_SHA.fullmatch(values["source_sha"]):
        raise StageError(f"{path}: source_sha must be a lowercase 40-character git SHA")
    if not PACKAGE_NAME.fullmatch(values["package_name"]):
        raise StageError(f"{path}: invalid Android package name")
    if not SEMVER.fullmatch(values["version_name"]):
        raise StageError(f"{path}: version_name is not SemVer")
    try:
        version_code = int(values["version_code"])
    except ValueError as error:
        raise StageError(f"{path}: version_code must be a positive integer") from error
    if version_code < 1 or str(version_code) != values["version_code"]:
        raise StageError(f"{path}: version_code must be a canonical positive integer")
    if Path(values["apk"]).name != values["apk"]:
        raise StageError(f"{path}: apk must be a basename, not a path")
    if not SHA256.fullmatch(values["apk_sha256"]):
        raise StageError(f"{path}: apk_sha256 must be a lowercase SHA-256 digest")
    return values


def validate_repo_url(value: str) -> str:
    parsed = urlparse(value)
    if parsed.scheme != "https" or not parsed.netloc or parsed.username or parsed.password:
        raise StageError("repo URL must be HTTPS and must not contain user info")
    if not parsed.path.rstrip("/").endswith("/fdroid/repo"):
        raise StageError("repo URL must end in /fdroid/repo")
    if parsed.params or parsed.query or parsed.fragment:
        raise StageError("repo URL must not contain params, query, or fragment")
    return value.rstrip("/")


def reject_signing_environment(env: dict[str, str]) -> None:
    present = sorted(name for name in FORBIDDEN_SIGNING_ENV if env.get(name))
    if present:
        raise StageError(
            "unsigned F-Droid staging refuses signing credentials: " + ", ".join(present)
        )


def find_apksigner(env: dict[str, str]) -> Path:
    explicit = env.get("ZARA_APKSIGNER")
    if explicit:
        candidate = Path(explicit)
        if candidate.is_file():
            return candidate
        raise StageError(f"ZARA_APKSIGNER is not a file: {candidate}")

    android_home = env.get("ANDROID_HOME") or env.get("ANDROID_SDK_ROOT")
    if android_home:
        candidates = sorted(Path(android_home).glob("build-tools/*/apksigner"))
        if candidates:
            return candidates[-1]

    path = shutil.which("apksigner")
    if path:
        return Path(path)
    raise StageError("could not locate apksigner in the pinned Android toolchain")


def signer_sha256(apksigner: Path, apk: Path) -> list[str]:
    completed = subprocess.run(
        [str(apksigner), "verify", "--print-certs", str(apk)],
        check=True,
        capture_output=True,
        text=True,
        env={**os.environ, "LC_ALL": "C"},
    )
    signers = []
    pattern = re.compile(r"^Signer #\d+ certificate SHA-256 digest: ([0-9A-Fa-f:]+)$")
    for line in completed.stdout.splitlines():
        match = pattern.match(line.strip())
        if match:
            digest = match.group(1).replace(":", "").lower()
            if not SHA256.fullmatch(digest):
                raise StageError(f"apksigner returned malformed SHA-256 digest for {apk}")
            signers.append(digest)
    signers = sorted(set(signers))
    if not signers:
        raise StageError(f"no APK signing certificate found for {apk}")
    if len(signers) > 16:
        raise StageError(f"too many APK signers for {apk}")
    return signers


def verify_artifact(
    apk: Path,
    manifest_path: Path,
    expected_package: str,
) -> dict[str, str]:
    if not apk.is_file():
        raise StageError(f"APK does not exist: {apk}")
    if not manifest_path.is_file():
        raise StageError(f"manifest does not exist: {manifest_path}")
    manifest = parse_manifest(manifest_path)
    if manifest["package_name"] != expected_package:
        raise StageError(
            f"expected package {expected_package}, manifest has {manifest['package_name']}"
        )
    if manifest["apk"] != apk.name:
        raise StageError(f"manifest APK basename does not match input: {manifest['apk']} != {apk.name}")
    actual_sha = sha256_file(apk)
    if actual_sha != manifest["apk_sha256"]:
        raise StageError(f"APK SHA-256 mismatch for {apk}: {actual_sha}")
    return manifest


def yaml_string(value: str) -> str:
    # JSON strings are a valid YAML 1.2 scalar representation.
    return json.dumps(value, ensure_ascii=False)


def write_metadata(path: Path, manifest: dict[str, str]) -> None:
    path.write_text(
        "\n".join(
            [
                "License: GPL-3.0-only",
                f"SourceCode: {SOURCE_REPO}",
                f"IssueTracker: {ISSUE_TRACKER}",
                f"CurrentVersion: {yaml_string(manifest['version_name'])}",
                f"CurrentVersionCode: {manifest['version_code']}",
                "",
            ]
        ),
        encoding="utf-8",
    )


def source_commit_epoch(source_sha: str) -> int:
    completed = subprocess.run(
        ["git", "show", "-s", "--format=%ct", source_sha],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    value = completed.stdout.strip()
    if not value.isdigit():
        raise StageError(f"could not resolve source commit timestamp for {source_sha}")
    return int(value)


def load_fdroid_index(path: Path) -> dict:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise StageError(f"invalid generated F-Droid index: {path}") from error


def stage(args: argparse.Namespace) -> None:
    env = dict(os.environ)
    reject_signing_environment(env)
    repo_url = validate_repo_url(args.repo_url)

    app_apk = args.app_apk.resolve()
    store_apk = args.store_apk.resolve()
    app = verify_artifact(app_apk, args.app_manifest.resolve(), "ai.zara.app")
    store = verify_artifact(store_apk, args.store_manifest.resolve(), "ai.zara.store")
    if app["source_sha"] != store["source_sha"]:
        raise StageError("phone and Store artifacts must come from the same exact source SHA")

    output = args.output.resolve()
    if output.exists() and any(output.iterdir()):
        raise StageError(f"output directory must be empty: {output}")
    output.mkdir(parents=True, exist_ok=True)
    repo_dir = output / "repo"
    metadata_dir = output / "metadata"
    zara_dir = output / "zara"
    repo_dir.mkdir()
    metadata_dir.mkdir()
    zara_dir.mkdir()

    config = CONFIG_TEMPLATE.read_text(encoding="utf-8").replace(
        "__ZARA_FDROID_REPO_URL__", yaml_string(repo_url)
    )
    if "__ZARA_FDROID_REPO_URL__" in config:
        raise StageError("F-Droid repo URL template substitution failed")
    (output / "config.yml").write_text(config, encoding="utf-8")

    timestamp = source_commit_epoch(app["source_sha"])
    staged = []
    apksigner = find_apksigner(env)
    for manifest, source_apk in ((app, app_apk), (store, store_apk)):
        dest = repo_dir / f"{manifest['package_name']}_{manifest['version_code']}.apk"
        shutil.copyfile(source_apk, dest)
        os.utime(dest, (timestamp, timestamp))
        signers = signer_sha256(apksigner, dest)
        write_metadata(metadata_dir / f"{manifest['package_name']}.yml", manifest)
        staged.append(
            {
                "package_name": manifest["package_name"],
                "version_name": manifest["version_name"],
                "version_code": int(manifest["version_code"]),
                "apk": f"/fdroid/repo/{dest.name}",
                "apk_sha256": manifest["apk_sha256"],
                "apk_signer_sha256": signers,
                "source_repo": SOURCE_REPO,
                "source_sha": manifest["source_sha"],
            }
        )

    source_manifest = {
        "schema": 1,
        "source_sha": app["source_sha"],
        "fdroid_repo": repo_url,
        "release_status": "unsigned_staging",
        "packages": staged,
    }
    (zara_dir / "source-manifest.json").write_text(
        json.dumps(source_manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )

    subprocess.run(
        [
            "fdroid",
            "update",
            "--clean",
            "--nosign",
            "--pretty",
            "--use-date-from-apk",
        ],
        cwd=output,
        env={**env, "LC_ALL": "C.UTF-8"},
        check=True,
    )

    index_path = repo_dir / "index-v2.json"
    index = load_fdroid_index(index_path)
    packages = index.get("packages")
    if not isinstance(packages, dict):
        raise StageError("generated F-Droid v2 index has no package map")
    expected = {"ai.zara.app", "ai.zara.store"}
    missing = sorted(expected - packages.keys())
    if missing:
        raise StageError("generated F-Droid v2 index is missing: " + ", ".join(missing))

    print(f"unsigned Zara F-Droid repo staged at {output}")
    print(f"source_sha={app['source_sha']}")
    print(f"repo_url={repo_url}")
    print(f"fdroid_index={index_path}")


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser(description=__doc__)
    value.add_argument("--output", type=Path, required=True)
    value.add_argument("--repo-url", required=True)
    value.add_argument("--app-apk", type=Path, required=True)
    value.add_argument("--app-manifest", type=Path, required=True)
    value.add_argument("--store-apk", type=Path, required=True)
    value.add_argument("--store-manifest", type=Path, required=True)
    return value


def main() -> int:
    try:
        stage(parser().parse_args())
    except (StageError, subprocess.CalledProcessError, OSError) as error:
        print(f"zara fdroid staging failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
