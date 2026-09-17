from pathlib import Path

import pytest

from zara.context import (
    ContextAttachmentKind,
    ContextAttachmentNotFound,
    ContextAttachmentScope,
    ContextAttachmentStore,
)


def test_text_file_attachment_is_explicit_bounded_and_removable(tmp_path: Path):
    source = tmp_path / "notes.txt"
    source.write_text("alpha\nbeta\n", encoding="utf-8")
    store = ContextAttachmentStore(max_inline_bytes=64)

    attachment = store.add_path(
        source,
        kind=ContextAttachmentKind.FILE,
        scope=ContextAttachmentScope.TURN,
        source="desktop:add-context",
    )

    assert attachment.display_name == "notes.txt"
    assert attachment.text == "alpha\nbeta\n"
    assert attachment.locator == str(source.resolve())
    assert store.resolve((attachment.id,)) == (attachment,)
    rendered = store.render((attachment.id,))
    assert "notes.txt" in rendered
    assert "alpha" in rendered

    assert store.remove(attachment.id)
    with pytest.raises(ContextAttachmentNotFound):
        store.resolve((attachment.id,))


def test_audio_video_and_project_are_references_not_implicit_binary_reads(tmp_path: Path):
    audio = tmp_path / "voice.wav"
    video = tmp_path / "clip.mp4"
    project = tmp_path / "project"
    audio.write_bytes(b"RIFF" + b"x" * 20)
    video.write_bytes(b"video" + b"y" * 20)
    project.mkdir()

    store = ContextAttachmentStore(max_inline_bytes=8)
    voice = store.add_path(audio, kind=ContextAttachmentKind.AUDIO)
    clip = store.add_path(video, kind=ContextAttachmentKind.VIDEO)
    folder = store.add_path(project, kind=ContextAttachmentKind.PROJECT)

    assert voice.text is None
    assert clip.text is None
    assert folder.text is None
    rendered = store.render((voice.id, clip.id, folder.id))
    assert "voice.wav" in rendered
    assert "clip.mp4" in rendered
    assert "project" in rendered
    assert "RIFF" not in rendered


def test_system_context_is_textual_provenanced_and_render_order_follows_ids():
    store = ContextAttachmentStore()
    first = store.add_text(
        "desktop=qtile\nsession=wayland",
        kind=ContextAttachmentKind.SYSTEM,
        display_name="Linux system context",
        source="plugin:linux-system-context",
    )
    second = store.add_text(
        "project=starintel",
        kind=ContextAttachmentKind.PROJECT,
        display_name="Project context",
        source="user",
    )

    rendered = store.render((second.id, first.id))

    assert rendered.index("Project context") < rendered.index("Linux system context")
    assert "plugin:linux-system-context" in rendered


def test_unknown_context_id_fails_closed():
    store = ContextAttachmentStore()

    with pytest.raises(ContextAttachmentNotFound):
        store.render(("ctx-missing",))
