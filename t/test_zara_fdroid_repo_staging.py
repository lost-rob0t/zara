from importlib.util import module_from_spec, spec_from_file_location
from pathlib import Path
import hashlib

import pytest


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts/stage-zara-fdroid-repo.py"
SPEC = spec_from_file_location("zara_fdroid_staging", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
MODULE = module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def _manifest(apk: Path, *, package: str = "ai.zara.app", version: str = "0.1.2-alpha") -> str:
    digest = hashlib.sha256(apk.read_bytes()).hexdigest()
    return "\n".join(
        [
            "schema=1",
            f"source_sha={'a' * 40}",
            f"package_name={package}",
            f"version_name={version}",
            "version_code=3",
            f"apk={apk.name}",
            f"apk_sha256={digest}",
            "",
        ]
    )


def test_manifest_parser_accepts_exact_release_evidence(tmp_path):
    apk = tmp_path / "zara-android-0.1.2-alpha.apk"
    apk.write_bytes(b"fixture-apk")
    manifest = tmp_path / "manifest.txt"
    manifest.write_text(_manifest(apk))

    parsed = MODULE.verify_artifact(apk, manifest, "ai.zara.app")

    assert parsed["source_sha"] == "a" * 40
    assert parsed["apk_sha256"] == hashlib.sha256(b"fixture-apk").hexdigest()


def test_manifest_parser_rejects_tampered_apk(tmp_path):
    apk = tmp_path / "zara-android-0.1.2-alpha.apk"
    apk.write_bytes(b"original")
    manifest = tmp_path / "manifest.txt"
    manifest.write_text(_manifest(apk))
    apk.write_bytes(b"tampered")

    with pytest.raises(MODULE.StageError, match="APK SHA-256 mismatch"):
        MODULE.verify_artifact(apk, manifest, "ai.zara.app")


def test_manifest_parser_rejects_unknown_fields(tmp_path):
    apk = tmp_path / "zara.apk"
    apk.write_bytes(b"fixture")
    manifest = tmp_path / "manifest.txt"
    manifest.write_text(_manifest(apk) + "surprise=true\n")

    with pytest.raises(MODULE.StageError, match="unsupported manifest field"):
        MODULE.parse_manifest(manifest)


def test_repo_url_must_be_https_standard_fdroid_path():
    assert (
        MODULE.validate_repo_url("https://repo.example/fdroid/repo/")
        == "https://repo.example/fdroid/repo"
    )
    for invalid in (
        "http://repo.example/fdroid/repo",
        "https://user:secret@repo.example/fdroid/repo",
        "https://repo.example/repo",
        "https://repo.example/fdroid/repo?token=secret",
    ):
        with pytest.raises(MODULE.StageError):
            MODULE.validate_repo_url(invalid)


def test_unsigned_staging_refuses_repo_signing_credentials():
    for name in MODULE.FORBIDDEN_SIGNING_ENV:
        with pytest.raises(MODULE.StageError, match="refuses signing credentials"):
            MODULE.reject_signing_environment({name: "present"})


def test_fdroid_invocation_is_unsigned_and_config_has_no_signing_keys():
    source = SCRIPT.read_text()
    config = (ROOT / "android/fdroid/config.yml.in").read_text()

    assert '"--nosign"' in source
    assert '"fdroid",\n            "update"' in source
    assert "fdroid signindex" not in source
    assert "fdroid publish" not in source
    assert "repo_keyalias:" not in config
    assert "keystore:" not in config
    assert "keystorepass:" not in config
    assert "keypass:" not in config
