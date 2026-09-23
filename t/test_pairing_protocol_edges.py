from __future__ import annotations

import io
from urllib.parse import urlencode

import pytest

import zara.pairing as pairing


SERVER_KEY = "a" * 40
TOKEN = "pairing-token"


def uri(**changes):
    values = {
        "broker_host": "127.0.0.1",
        "broker_port": "43210",
        "endpoint": "tcp://127.0.0.1:7731",
        "server_key": SERVER_KEY,
        "token": TOKEN,
        "expires": "1100",
    }
    values.update(changes)
    return "zara://pair/v1?" + urlencode(values)


def test_parse_pairing_uri_validates_every_boundary():
    payload = pairing.parse_pairing_uri(uri(), now=1000)
    assert payload.broker_host == "127.0.0.1"
    assert payload.broker_port == 43210
    assert payload.endpoint == "tcp://127.0.0.1:7731"
    assert payload.server_key == SERVER_KEY
    assert payload.token == TOKEN
    assert payload.expires_at == 1100

    bad_values = [
        ("https://example.invalid", pairing.PairingError),
        ("zara://pair/v1#frag", pairing.PairingError),
        (uri(broker_host="bad host"), pairing.PairingError),
        (uri(broker_port="abc"), pairing.PairingError),
        (uri(broker_port="0"), pairing.PairingError),
        (uri(token="short"), pairing.PairingError),
        (uri(expires="abc"), pairing.PairingError),
        (uri(expires="1000"), pairing.PairingExpired),
        (uri(expires="1701"), pairing.PairingError),
    ]
    for raw, error in bad_values:
        with pytest.raises(error):
            pairing.parse_pairing_uri(raw, now=1000)

    with pytest.raises(pairing.PairingError, match="fields"):
        pairing.parse_pairing_uri("zara://pair/v1?broker_host=x", now=1000)


def test_message_and_endpoint_helpers_cover_invalid_inputs(monkeypatch, tmp_path):
    assert pairing._read_client_message(io.BytesIO(b'{"status":"ok"}\n')) == {"status": "ok"}
    for raw in [b"", b"{}", b"not-json\n", b"[]\n"]:
        with pytest.raises(pairing.PairingError):
            pairing._read_client_message(io.BytesIO(raw))

    assert pairing._validate_tcp_endpoint("tcp://host:123/") == "tcp://host:123"
    for endpoint in ["http://host:1", "tcp://host", "tcp://u:p@host:1", "tcp://host:1/path", "tcp://host:1?q=x"]:
        with pytest.raises(ValueError):
            pairing._validate_tcp_endpoint(endpoint)

    assert pairing._endpoint_host("tcp://127.0.0.1:1") == ""
    assert pairing._endpoint_host("tcp://0.0.0.0:1") == ""
    assert pairing._endpoint_host("tcp://example.test:1") == "example.test"
    assert pairing._endpoint_with_host("tcp://host:22", "example.test") == "tcp://example.test:22"
    assert pairing._endpoint_with_host("tcp://host:22", "2001:db8::1") == "tcp://[2001:db8::1]:22"
    with pytest.raises(ValueError):
        pairing._endpoint_with_host("tcp://host", "x")

    assert pairing._security_directory(None, str(tmp_path / "explicit")) == tmp_path / "explicit"
    monkeypatch.setenv("ZARA_SECURITY_DIR", str(tmp_path / "env"))
    assert pairing._security_directory(None, None) == tmp_path / "env"
    monkeypatch.delenv("ZARA_SECURITY_DIR")

    class Config:
        def get_section(self, _name):
            return {"security_dir": str(tmp_path / "config")}

    assert pairing._security_directory(Config(), None) == tmp_path / "config"
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path / "state"))
    assert pairing._security_directory(None, None) == tmp_path / "state" / "zarathushtra" / "security"


def test_qr_fingerprint_and_pair_client_main(monkeypatch, capsys):
    assert len(pairing._fingerprint(SERVER_KEY)) == 16
    monkeypatch.setattr(pairing.shutil, "which", lambda _name: None)
    assert pairing._render_qr("zara://pair/v1") is False

    calls = []
    monkeypatch.setattr(pairing.shutil, "which", lambda _name: "/bin/qrencode")
    monkeypatch.setattr(pairing.subprocess, "run", lambda *args, **kwargs: calls.append((args, kwargs)))
    assert pairing._render_qr("uri") is True
    assert calls and calls[0][1]["check"] is True

    monkeypatch.setattr(
        pairing,
        "pair_client",
        lambda *_args, **_kwargs: pairing.PairingClientOutcome("tcp://server:1", "desktop-abc"),
    )
    assert pairing.pair_client_main(["zara://pair/v1?ignored=1"]) == 0
    assert "Paired desktop-abc" in capsys.readouterr().out

    monkeypatch.setattr(pairing, "pair_client", lambda *_args, **_kwargs: (_ for _ in ()).throw(pairing.PairingError("nope")))
    assert pairing.pair_client_main(["zara://pair/v1?ignored=1"]) == 2
    assert "Pairing failed" in capsys.readouterr().err


def test_pair_client_protocol_rejections_are_fail_closed(monkeypatch):
    public_key = "b" * 40

    class Store:
        def identity_or_create(self):
            return public_key, "secret"

        def complete_pairing(self, **_kwargs):
            pytest.fail("rejected protocol must not complete pairing")

    class Stream:
        def __init__(self, responses):
            self.responses = iter(responses)
            self.closed = False

        def write(self, _data):
            pass

        def readline(self, _limit):
            return next(self.responses)

        def close(self):
            self.closed = True

    class Connection:
        def __init__(self, responses):
            self.stream = Stream(responses)
            self.closed = False

        def settimeout(self, _timeout):
            pass

        def makefile(self, *_args, **_kwargs):
            return self.stream

        def close(self):
            self.closed = True

    raw = uri()
    expected_code = pairing.verification_code(TOKEN, public_key)
    expected_id = pairing.derive_device_id(public_key, "desktop")

    cases = [
        [b'{"status":"rejected","code":"no"}\n'],
        [f'{{"status":"pending","verification_code":"{expected_code}","device_id":"{expected_id}","extra":1}}\n'.encode()],
        [b'{"status":"pending","verification_code":"bad","device_id":"bad"}\n'],
        [f'{{"status":"pending","verification_code":"{expected_code}","device_id":"{expected_id}"}}\n'.encode(), b'{"status":"rejected","code":"no"}\n'],
        [f'{{"status":"pending","verification_code":"{expected_code}","device_id":"{expected_id}"}}\n'.encode(), b'{"status":"approved","endpoint":"tcp://wrong:1","server_key":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","device_id":"bad"}\n'],
    ]
    for responses in cases:
        connection = Connection(responses)
        monkeypatch.setattr(pairing.socket, "create_connection", lambda *_args, connection=connection, **_kwargs: connection)
        with pytest.raises(pairing.PairingError):
            pairing.pair_client(raw, store=Store())
        assert connection.closed and connection.stream.closed

    monkeypatch.setattr(pairing.socket, "create_connection", lambda *_args, **_kwargs: (_ for _ in ()).throw(OSError("offline")))
    with pytest.raises(pairing.PairingError, match="could not reach"):
        pairing.pair_client(raw, store=Store())
