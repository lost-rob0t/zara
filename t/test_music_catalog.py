from __future__ import annotations

from zara.music.catalog import normalize_navidrome_rows


def row(
    media_id,
    *,
    title="Track",
    album="Album",
    artist="Artist",
    mbz_recording_id=None,
    album_id="album-1",
    artist_id="artist-1",
):
    return {
        "id": media_id,
        "library_id": "library-1",
        "path": f"Artist/Album/{media_id}.flac",
        "title": title,
        "album": album,
        "artist": artist,
        "duration": 180.5,
        "size": 123456,
        "updated_at": "2026-09-18T20:00:00Z",
        "album_id": album_id,
        "artist_id": artist_id,
        "mbz_recording_id": mbz_recording_id,
        "year": 2007,
        "suffix": "flac",
    }


def test_normalizer_builds_file_recording_artist_album_and_library_graph():
    batch = normalize_navidrome_rows(
        [row("media-1", mbz_recording_id="recording-mbid")]
    )

    document = batch.documents[0]
    assert document["value"]["dtype"] == "music_file"
    assert document["value"]["source"] == "navidrome"
    assert document["value"]["source_id"] == "media-1"
    assert document["value"]["path"] == "Artist/Album/media-1.flac"

    node_types = {node["props"]["dtype"] for node in batch.nodes}
    assert node_types == {"music_file", "recording", "artist", "album", "library"}

    predicates = {edge["predicate"] for edge in batch.edges}
    assert predicates == {
        "represents-recording",
        "in-library",
        "performed-by",
        "part-of-album",
    }


def test_hard_musicbrainz_recording_id_merges_recording_identity_across_files():
    batch = normalize_navidrome_rows(
        [
            row("lossless", mbz_recording_id="same-mbid"),
            row("mp3", mbz_recording_id="same-mbid"),
        ]
    )

    recording_nodes = [
        node for node in batch.nodes if node["props"]["dtype"] == "recording"
    ]
    file_nodes = [
        node for node in batch.nodes if node["props"]["dtype"] == "music_file"
    ]

    assert len(recording_nodes) == 1
    assert len(file_nodes) == 2


def test_without_hard_recording_id_files_do_not_fuzzy_merge():
    batch = normalize_navidrome_rows(
        [
            row("master-a", mbz_recording_id=None),
            row("master-b", mbz_recording_id=None),
        ]
    )

    recording_nodes = [
        node for node in batch.nodes if node["props"]["dtype"] == "recording"
    ]

    assert len(recording_nodes) == 2
    assert recording_nodes[0]["id"] != recording_nodes[1]["id"]


def test_shared_artist_album_and_library_nodes_are_deduplicated_per_batch():
    batch = normalize_navidrome_rows(
        [
            row("media-1", title="One"),
            row("media-2", title="Two"),
        ]
    )

    counts = {}
    for node in batch.nodes:
        counts[node["props"]["dtype"]] = counts.get(node["props"]["dtype"], 0) + 1

    assert counts == {
        "music_file": 2,
        "recording": 2,
        "artist": 1,
        "album": 1,
        "library": 1,
    }


def test_ids_and_edges_are_deterministic_across_input_order():
    first = normalize_navidrome_rows(
        [row("a", title="One"), row("b", title="Two")]
    )
    second = normalize_navidrome_rows(
        [row("b", title="Two"), row("a", title="One")]
    )

    assert {node["id"] for node in first.nodes} == {
        node["id"] for node in second.nodes
    }
    assert {edge["id"] for edge in first.edges} == {
        edge["id"] for edge in second.edges
    }
    assert {document["id"] for document in first.documents} == {
        document["id"] for document in second.documents
    }
