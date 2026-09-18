from importlib.util import module_from_spec, spec_from_file_location
from pathlib import Path
import unittest


MODULE_PATH = Path(__file__).resolve().parents[1] / "android" / "integration" / "device_acceptance.py"
SPEC = spec_from_file_location("android_device_acceptance", MODULE_PATH)
assert SPEC is not None and SPEC.loader is not None
DEVICE_ACCEPTANCE = module_from_spec(SPEC)
SPEC.loader.exec_module(DEVICE_ACCEPTANCE)


class DeviceHierarchyDumpTest(unittest.TestCase):
    def test_nodes_uses_shell_writable_tmp_path_and_clears_stale_dump(self) -> None:
        device = DEVICE_ACCEPTANCE.Device("emulator-5554", Path("."))
        calls: list[tuple[str, ...]] = []

        def fake_adb(*arguments: str, binary: bool = False):
            self.assertFalse(binary)
            calls.append(arguments)
            if arguments[:2] == ("shell", "cat"):
                return '<hierarchy><node text="Chat" /></hierarchy>'
            return ""

        device.adb = fake_adb

        nodes = list(device.nodes())

        self.assertEqual([node.get("text") for node in nodes], ["Chat"])
        self.assertEqual(
            calls,
            [
                ("shell", "rm", "-f", "/data/local/tmp/zara-acceptance.xml"),
                (
                    "shell",
                    "uiautomator",
                    "dump",
                    "/data/local/tmp/zara-acceptance.xml",
                ),
                ("shell", "cat", "/data/local/tmp/zara-acceptance.xml"),
            ],
        )


if __name__ == "__main__":
    unittest.main()
