import ast
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
INSTALLED_ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"


def test_installed_transcript_requires_conversation_and_evidence_identity_across_recreation() -> None:
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")
    tree = ast.parse(source, filename=str(INSTALLED_ACCEPTANCE))

    helpers = {
        node.name: node
        for node in tree.body
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
    }
    assert "assert_checkpoint_continuity" in helpers, (
        "Installed pure-symbolic acceptance must reject a transcript that silently replaces the "
        "canonical conversation between process recreations or swaps the admitted expert evidence "
        "before the restart-safe `why?` follow-up."
    )

    helper_source = ast.get_source_segment(source, helpers["assert_checkpoint_continuity"]) or ""
    required_fragments = (
        '"conversation_id"',
        '"turn_id"',
        '"expert_evidence"',
        '"expert-answer"',
        '"expert-follow-up-after-restart"',
    )
    for fragment in required_fragments:
        assert fragment in helper_source, (
            "Checkpoint continuity must bind one durable conversation identity, distinct committed "
            "turn identities, and the same canonical expert evidence across recreation; missing "
            f"{fragment}."
        )

    assert "seen_turn_ids" in helper_source, (
        "Installed multi-turn acceptance must reject a stale projection that reuses an earlier "
        "canonical turn_id for a later UI-visible turn."
    )
    assert 'projection.get("turn_id")' in helper_source, (
        "The final durable SQLite snapshot must be tied to the terminal restart-safe `why?` turn, "
        "not merely to the same conversation/evidence identity."
    )

    exercise = helpers.get("exercise_pure_symbolic_dialogue")
    assert exercise is not None
    exercise_source = ast.get_source_segment(source, exercise) or ""
    assert "projection = inspect_pure_symbolic_database(device, output)" in exercise_source
    assert "assert_checkpoint_continuity(accounting_checkpoints, projection)" in exercise_source
    assert exercise_source.index("projection = inspect_pure_symbolic_database(device, output)") < exercise_source.index(
        "assert_checkpoint_continuity(accounting_checkpoints, projection)"
    ), (
        "Continuity must be verified against the final durable database snapshot, not only against "
        "transient in-process state."
    )
