from pathlib import Path

import pytest

from zara.literate_org import LiterateOrgConfigError, tangle_file, tangle_text


def test_tangle_text_matches_doom_python_prolog_header_args():
    source = """
#+title: Zara literate config
#+property: header-args :results none
#+property: header-args:python :tangle hooks/config.py
#+property: header-args:prolog :tangle logic/config.pl

* Python
#+begin_src python
def configure(zara):
    zara.theme = "outrun"
#+end_src

* Prolog
#+begin_src prolog
zara_theme(outrun).
#+end_src
""".strip()

    outputs = tangle_text(source, source_name="zara.org")
    assert [item.path for item in outputs] == ["hooks/config.py", "logic/config.pl"]
    assert "def configure" in outputs[0].content
    assert "zara_theme(outrun)." in outputs[1].content


def test_tangle_file_writes_only_below_output_root(tmp_path: Path):
    source = tmp_path / "config.org"
    source.write_text(
        """
#+property: header-args:python :tangle generated/config.py
#+begin_src python
VALUE = 42
#+end_src
""".strip(),
        encoding="utf-8",
    )

    written = tangle_file(source, output_root=tmp_path / "runtime")
    assert written == (tmp_path / "runtime" / "generated" / "config.py",)
    assert written[0].read_text(encoding="utf-8") == "VALUE = 42\n"


def test_parent_traversal_is_rejected():
    source = """
#+begin_src prolog :tangle ../escape.pl
bad(idea).
#+end_src
""".strip()
    with pytest.raises(LiterateOrgConfigError, match="unsafe"):
        tangle_text(source)


def test_source_blocks_are_inert_during_tangle(tmp_path: Path):
    marker = tmp_path / "should-not-exist"
    source = f"""
#+begin_src python :tangle config.py
from pathlib import Path
Path({str(marker)!r}).write_text("executed")
#+end_src
""".strip()

    outputs = tangle_text(source)
    assert len(outputs) == 1
    assert not marker.exists()
