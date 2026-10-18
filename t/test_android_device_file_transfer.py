import hashlib
import importlib.util
from pathlib import Path
import subprocess

import pytest

spec = importlib.util.spec_from_file_location("device_file_probe", Path("android/integration/device_acceptance.py"))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class Device:
    def __init__(self, data, *, advertised_size=None, digest=None, short=False, failed=False):
        self.data = data
        self.size = len(data) if advertised_size is None else advertised_size
        self.digest = digest or hashlib.sha256(data).hexdigest()
        self.short = short
        self.failed = failed
        self.reads = []

    def adb(self, *args, binary=False):
        if args[:3] == ("shell", "stat", "-c"):
            return str(self.size)
        if args[:2] == ("shell", "sha256sum"):
            return self.digest + "  /data/local/tmp/probe\n"
        assert args[:2] == ("exec-out", "dd")
        assert binary
        options = dict(arg.split("=", 1) for arg in args[2:])
        assert options["if"] == "/data/local/tmp/probe"
        assert options["count"] == "1"
        size, offset = int(options["bs"]), int(options["skip"])
        assert size <= 65536
        self.reads.append(offset)
        if self.failed:
            raise subprocess.CalledProcessError(1, args)
        result = self.data[offset * size:(offset + 1) * size]
        return result[:-1] if self.short else result


def test_complete_binary_file_crosses_many_bounded_reads():
    data = bytes(range(256)) * 1300
    device = Device(data)
    assert module.read_device_file(device, "/data/local/tmp/probe") == data
    assert device.reads == list(range(6))


@pytest.mark.parametrize("kwargs", [
    {"advertised_size": 0}, {"advertised_size": -1},
    {"advertised_size": 64 * 1024 * 1024}, {"advertised_size": "invalid"},
    {"digest": "0" * 64}, {"digest": "invalid"}, {"short": True},
])
def test_incomplete_or_unverifiable_dump_fails_closed(kwargs):
    with pytest.raises((ValueError, AssertionError)):
        module.read_device_file(Device(b"log evidence", **kwargs), "/data/local/tmp/probe")


def test_transport_error_is_not_retried_or_reported_as_success():
    device = Device(b"log evidence", failed=True)
    with pytest.raises(subprocess.CalledProcessError):
        module.read_device_file(device, "/data/local/tmp/probe")
    assert device.reads == [0]
