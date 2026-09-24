from __future__ import annotations

from pathlib import Path
import subprocess

from zara.protocol import DEVICE_CAPABILITIES
from zara.security import Capability


ROOT = Path(__file__).resolve().parents[1]
VERIFIER = ROOT / "verification" / "peer_node_verify.pl"


def _swipl(goal: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            "swipl",
            "-q",
            "-s",
            str(VERIFIER),
            "-g",
            f"(({goal}) -> halt(0); halt(1))",
            "-t",
            "halt(1)",
        ],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )


def _quoted_atom(value: str) -> str:
    return "'" + value.replace("'", "''") + "'"


def test_peer_node_authority_proof_is_terminal_green() -> None:
    result = _swipl("peer_node_verify:verify")

    assert result.returncode == 0, result.stderr or result.stdout
    assert "PEER_NODE_VERIFY_OK" in result.stdout


def test_formal_device_capabilities_match_runtime_closed_set() -> None:
    for capability in DEVICE_CAPABILITIES:
        result = _swipl(
            f"peer_node_verify:device_capability({_quoted_atom(capability)})"
        )
        assert result.returncode == 0, capability

    result = _swipl(
        "findall(C,peer_node_verify:device_capability(C),Cs),"
        "sort(Cs,S),length(S,N),N=:=" + str(len(DEVICE_CAPABILITIES))
    )
    assert result.returncode == 0, result.stderr or result.stdout


def test_formal_security_capabilities_match_runtime_closed_set() -> None:
    runtime = {capability.value for capability in Capability}
    for capability in runtime:
        result = _swipl(
            f"peer_node_verify:security_capability({_quoted_atom(capability)})"
        )
        assert result.returncode == 0, capability

    result = _swipl(
        "findall(C,peer_node_verify:security_capability(C),Cs),"
        "sort(Cs,S),length(S,N),N=:=" + str(len(runtime))
    )
    assert result.returncode == 0, result.stderr or result.stdout


def test_formal_binding_rejects_stale_revoked_and_route_selected_identity() -> None:
    valid = _swipl(
        "peer_node_verify:node_binding_valid(phone_alice,phone_alice,key_a,key_a,2,2,true)"
    )
    assert valid.returncode == 0

    stale = _swipl(
        "\\+ peer_node_verify:node_binding_valid(phone_alice,phone_alice,key_a,key_a,1,2,true)"
    )
    assert stale.returncode == 0

    revoked = _swipl(
        "\\+ peer_node_verify:node_binding_valid(phone_alice,phone_alice,key_a,key_a,2,2,false)"
    )
    assert revoked.returncode == 0

    route = _swipl("\\+ peer_node_verify:authority_source(route_identity)")
    assert route.returncode == 0


def test_formal_security_authority_requires_same_active_zap_registry_identity() -> None:
    valid = _swipl(
        "peer_node_verify:authenticated_security_capability(user_alice,user_alice,true,'turn.submit')"
    )
    assert valid.returncode == 0, valid.stderr or valid.stdout

    mismatch = _swipl(
        "\\+ peer_node_verify:authenticated_security_capability(user_alice,user_mallory,true,'turn.submit')"
    )
    assert mismatch.returncode == 0

    revoked = _swipl(
        "\\+ peer_node_verify:authenticated_security_capability(user_alice,user_alice,false,'turn.submit')"
    )
    assert revoked.returncode == 0

    missing_zap_identity = _swipl(
        "\\+ peer_node_verify:authenticated_security_capability('',user_alice,true,'turn.submit')"
    )
    assert missing_zap_identity.returncode == 0
