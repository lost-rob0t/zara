import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKBENCH = ROOT / "voice-fixtures.org"
BEGIN = "#+begin_src emacs-lisp"
END = "#+end_src"
REQUIRED_FORMS = {
    "zara-vf-open-worktree",
    "zara-vf-new-case",
    "zara-vf-record",
    "zara-vf-record-next",
    "zara-vf-run-case-gate",
    "zara-vf-run-gate",
    "zara-vf-commit-and-push",
    "zara-vf-sync-master",
}


def _source_blocks(text: str) -> list[str]:
    lines = text.splitlines()
    blocks = []
    current = None

    for line_number, line in enumerate(lines, start=1):
        normalized = line.strip().lower()
        if normalized == BEGIN:
            assert current is None, f"nested Emacs Lisp block at line {line_number}"
            current = []
            continue
        if normalized == END:
            if current is not None:
                blocks.append("\n".join(current))
                current = None
            continue
        if current is not None:
            current.append(line)

    assert current is None, "unterminated Emacs Lisp source block"
    return blocks


def _assert_balanced_elisp(block: str, block_number: int) -> None:
    depth = 0
    in_string = False
    escaped = False
    in_comment = False

    for offset, char in enumerate(block):
        if in_comment:
            if char == "\n":
                in_comment = False
            continue

        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue

        if char == ";":
            in_comment = True
        elif char == '"':
            in_string = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            assert depth >= 0, (
                f"Emacs Lisp block {block_number}: unexpected ')' at byte {offset}"
            )

    assert not in_string, f"Emacs Lisp block {block_number}: unterminated string"
    assert depth == 0, (
        f"Emacs Lisp block {block_number}: unbalanced parentheses, depth={depth}"
    )


def test_voice_fixture_org_workbench_is_structurally_valid():
    text = WORKBENCH.read_text(encoding="utf-8")
    blocks = _source_blocks(text)

    assert len(blocks) >= 2, "voice workbench must contain executable Emacs Lisp"
    for index, block in enumerate(blocks, start=1):
        _assert_balanced_elisp(block, index)


def test_voice_fixture_org_workbench_exposes_required_controls():
    text = WORKBENCH.read_text(encoding="utf-8")
    definitions = set(re.findall(r"\(defun\s+(zara-vf-[^\s()]+)", text))
    missing = sorted(REQUIRED_FORMS - definitions)

    assert not missing, f"voice workbench is missing required controls: {missing}"
    assert 'defconst zara-vf-branch "fixtures/voice-recordings"' in text
    assert '~/git/worktrees/zara-voice-recordings' in text
    assert "--force-with-lease" in text
    assert "--signal=INT" in text
    assert ".part.wav" in text
