from __future__ import annotations

import base64
import re
import shutil
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Optional, Sequence

from langchain_core.tools import StructuredTool

from zara.plugins import PluginMetadata, ServicePlugin, StartupUnavailable
from zara.prolog_engine import PrologEngine, locate_main_pl


MAX_SCREENSHOT_BYTES = 16 * 1024 * 1024
MAX_UI_TREE_BYTES = 1024 * 1024
MAX_STDERR_BYTES = 4096
MAX_TEXT_BYTES = 512
MAX_COORDINATE = 16384
MAX_PLAN_ACTIONS = 32
PLAN_NAME = re.compile(r"[a-z][a-z0-9_]{0,63}")
SAFE_TEXT = re.compile(r"[A-Za-z0-9 _.,:@/+\-]{1,512}")
PACKAGE = re.compile(r"[A-Za-z][A-Za-z0-9_]*(?:\.[A-Za-z][A-Za-z0-9_]*)+")
KEYCODES = {
    "back": 4,
    "home": 3,
    "enter": 66,
    "recents": 187,
    "tab": 61,
    "escape": 111,
    "delete": 67,
    "up": 19,
    "down": 20,
    "left": 21,
    "right": 22,
}


@dataclass(frozen=True)
class AdbDevice:
    serial: str
    state: str
    detail: str


class AdbCommandError(RuntimeError):
    pass


class AdbTransport:
    """Bounded ADB transport. No caller can supply an arbitrary shell command."""

    def __init__(
        self,
        adb: str,
        *,
        serial: Optional[str] = None,
        timeout_seconds: float = 20.0,
        runner: Callable[..., subprocess.CompletedProcess] = subprocess.run,
        sleeper: Callable[[float], None] = time.sleep,
    ) -> None:
        self.adb = adb
        self.serial = serial
        self.timeout_seconds = timeout_seconds
        self._runner = runner
        self._sleeper = sleeper

    def devices(self) -> tuple[AdbDevice, ...]:
        result = self._run_host(("devices", "-l"))
        rows: list[AdbDevice] = []
        for raw in result.stdout.decode("utf-8", "replace").splitlines()[1:]:
            line = raw.strip()
            if not line:
                continue
            fields = line.split()
            if len(fields) < 2:
                continue
            serial, state = fields[:2]
            rows.append(AdbDevice(serial=serial, state=state, detail=" ".join(fields[2:])))
        return tuple(rows)

    def selected_serial(self) -> str:
        available = [device for device in self.devices() if device.state == "device"]
        if self.serial:
            if any(device.serial == self.serial for device in available):
                return self.serial
            raise AdbCommandError("configured Android ADB target is not connected")
        if len(available) != 1:
            raise AdbCommandError(
                "ADB control requires exactly one connected device or plugins.android-adb.serial"
            )
        return available[0].serial

    def screenshot_png(self) -> bytes:
        payload = self._run_device(("exec-out", "screencap", "-p"), max_stdout=MAX_SCREENSHOT_BYTES).stdout
        if not payload.startswith(b"\x89PNG\r\n\x1a\n"):
            raise AdbCommandError("Android target did not return a PNG screenshot")
        return payload

    def ui_tree(self) -> str:
        payload = self._run_device(
            ("exec-out", "uiautomator", "dump", "/dev/tty"),
            max_stdout=MAX_UI_TREE_BYTES,
        ).stdout
        text = payload.decode("utf-8", "replace")
        start = text.find("<?xml")
        if start < 0:
            raise AdbCommandError("Android target did not return a UI hierarchy")
        return text[start:].strip()

    def tap(self, x: int, y: int) -> None:
        self._coordinate(x)
        self._coordinate(y)
        self._run_device(("shell", "input", "tap", str(x), str(y)), max_stdout=4096)

    def swipe(self, x1: int, y1: int, x2: int, y2: int, duration_ms: int) -> None:
        for value in (x1, y1, x2, y2):
            self._coordinate(value)
        if not isinstance(duration_ms, int) or isinstance(duration_ms, bool) or not 1 <= duration_ms <= 5000:
            raise ValueError("swipe duration must be between 1 and 5000 ms")
        self._run_device(
            (
                "shell",
                "input",
                "swipe",
                str(x1),
                str(y1),
                str(x2),
                str(y2),
                str(duration_ms),
            ),
            max_stdout=4096,
        )

    def type_text(self, text: str) -> None:
        encoded = self._input_text(text)
        self._run_device(("shell", "input", "text", encoded), max_stdout=4096)

    def key(self, key: str) -> None:
        normalized = str(key).strip().lower()
        try:
            code = KEYCODES[normalized]
        except KeyError as error:
            raise ValueError("unsupported Android key") from error
        self._run_device(("shell", "input", "keyevent", str(code)), max_stdout=4096)

    def open_package(self, package: str) -> None:
        if not isinstance(package, str) or not PACKAGE.fullmatch(package):
            raise ValueError("invalid Android package name")
        self._run_device(
            (
                "shell",
                "monkey",
                "-p",
                package,
                "-c",
                "android.intent.category.LAUNCHER",
                "1",
            ),
            max_stdout=16 * 1024,
        )

    def wait(self, duration_ms: int) -> None:
        if not isinstance(duration_ms, int) or isinstance(duration_ms, bool) or not 0 <= duration_ms <= 5000:
            raise ValueError("wait duration must be between 0 and 5000 ms")
        self._sleeper(duration_ms / 1000.0)

    def _run_host(self, args: Sequence[str], *, max_stdout: int = 1024 * 1024) -> subprocess.CompletedProcess:
        return self._run((self.adb, *args), max_stdout=max_stdout)

    def _run_device(self, args: Sequence[str], *, max_stdout: int) -> subprocess.CompletedProcess:
        serial = self.selected_serial()
        return self._run((self.adb, "-s", serial, *args), max_stdout=max_stdout)

    def _run(self, argv: Sequence[str], *, max_stdout: int) -> subprocess.CompletedProcess:
        result = self._runner(
            list(argv),
            shell=False,
            check=False,
            capture_output=True,
            timeout=self.timeout_seconds,
        )
        stdout = bytes(result.stdout or b"")
        stderr = bytes(result.stderr or b"")
        if len(stdout) > max_stdout:
            raise AdbCommandError("ADB response exceeds byte limit")
        if result.returncode != 0:
            detail = stderr[:MAX_STDERR_BYTES].decode("utf-8", "replace").strip()
            raise AdbCommandError(detail or f"ADB exited with status {result.returncode}")
        return subprocess.CompletedProcess(result.args, result.returncode, stdout, stderr)

    @staticmethod
    def _coordinate(value: int) -> None:
        if not isinstance(value, int) or isinstance(value, bool) or not 0 <= value <= MAX_COORDINATE:
            raise ValueError(f"coordinate must be between 0 and {MAX_COORDINATE}")

    @staticmethod
    def _input_text(value: str) -> str:
        if not isinstance(value, str):
            raise TypeError("text must be a string")
        if len(value.encode("utf-8")) > MAX_TEXT_BYTES or not SAFE_TEXT.fullmatch(value):
            raise ValueError(
                "ADB text is limited to letters, numbers, spaces, and ._,:@/+- punctuation"
            )
        return value.replace(" ", "%s")


class AndroidAdbPlugin(ServicePlugin):
    enabled_by_default = False
    metadata = PluginMetadata(
        name="android-adb",
        version="0.1.0",
        description="Prolog-governed typed ADB control and screenshot observation.",
    )

    def __init__(self) -> None:
        self._transport: Optional[AdbTransport] = None
        self._engine: Optional[PrologEngine] = None

    def start(self, runtime):
        configuration = dict(runtime.configuration)
        adb_value = str(configuration.get("adb_path", "adb")).strip()
        adb = self._resolve_adb(adb_value)
        if adb is None:
            return StartupUnavailable("adb executable is not available")
        serial_raw = configuration.get("serial")
        serial = str(serial_raw).strip() if serial_raw is not None else None
        if serial == "":
            serial = None
        timeout = float(configuration.get("timeout_seconds", 20.0))
        if not 1.0 <= timeout <= 60.0:
            raise ValueError("android-adb timeout_seconds must be between 1 and 60")
        self._transport = AdbTransport(adb, serial=serial, timeout_seconds=timeout)
        self._transport._run_host(("version",), max_stdout=64 * 1024)

        engine = PrologEngine()
        engine.consult(locate_main_pl().parent / "kb" / "android_control.pl")
        self._engine = engine
        return None

    def stop(self) -> None:
        self._transport = None
        self._engine = None

    def tools(self):
        return (
            StructuredTool.from_function(
                self.android_adb_devices,
                name="android_adb_devices",
                description="List ADB devices visible to Zara and show which target is selectable.",
            ),
            StructuredTool.from_function(
                self.android_adb_screenshot,
                name="android_adb_screenshot",
                description=(
                    "Capture the selected Android screen as a PNG data URL. Use this as "
                    "multimodal vision input before choosing a typed Android action."
                ),
            ),
            StructuredTool.from_function(
                self.android_adb_ui_tree,
                name="android_adb_ui_tree",
                description="Read the selected Android device's bounded UIAutomator hierarchy.",
            ),
            StructuredTool.from_function(
                self.android_adb_run_plan,
                name="android_adb_run_plan",
                description="Execute one named, validated android_adb_plan/2 selected by Prolog.",
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.android_adb_tap,
                name="android_adb_tap",
                description=(
                    "Tap a bounded screen coordinate on the selected Android target. "
                    "For vision control, inspect android_adb_screenshot first."
                ),
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.android_adb_swipe,
                name="android_adb_swipe",
                description="Perform a bounded swipe on the selected Android target.",
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.android_adb_type_text,
                name="android_adb_type_text",
                description="Type bounded safe text into the focused Android field.",
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.android_adb_key,
                name="android_adb_key",
                description="Send an allowlisted Android navigation key.",
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.android_adb_open_app,
                name="android_adb_open_app",
                description=(
                    "Open an Android app by a semantic alias resolved through "
                    "android_app_package/2 in Prolog."
                ),
                metadata={"zara_requires_approval": True},
            ),
        )

    def android_adb_devices(self) -> dict[str, Any]:
        transport = self._require_transport()
        devices = transport.devices()
        selected = None
        try:
            selected = transport.selected_serial()
        except AdbCommandError:
            pass
        return {
            "selected": selected,
            "devices": [
                {"serial": item.serial, "state": item.state, "detail": item.detail}
                for item in devices
            ],
        }

    def android_adb_screenshot(self) -> dict[str, Any]:
        transport = self._require_transport()
        payload = transport.screenshot_png()
        encoded = base64.b64encode(payload).decode("ascii")
        return {
            "serial": transport.selected_serial(),
            "mime_type": "image/png",
            "bytes": len(payload),
            "data_url": f"data:image/png;base64,{encoded}",
        }

    def android_adb_ui_tree(self) -> dict[str, Any]:
        transport = self._require_transport()
        tree = transport.ui_tree()
        return {"serial": transport.selected_serial(), "xml": tree}

    def android_adb_run_plan(self, name: str) -> dict[str, Any]:
        if not isinstance(name, str) or not PLAN_NAME.fullmatch(name):
            raise ValueError("plan name must be a bounded lowercase Prolog atom")
        engine = self._require_engine()
        result = engine.query_once(f"kb_android_control:android_adb_plan({name}, Actions)")
        if not result:
            raise ValueError(f"unknown Android ADB plan: {name}")
        actions = self._parse_actions(result.get("Actions"))
        for action in actions:
            self._execute_action(action)
        return {"plan": name, "actions": len(actions), "completed": True}

    def android_adb_tap(self, x: int, y: int) -> dict[str, Any]:
        self._require_vision_mutation()
        self._require_transport().tap(x, y)
        return {"completed": True, "action": "tap"}

    def android_adb_swipe(
        self,
        x1: int,
        y1: int,
        x2: int,
        y2: int,
        duration_ms: int = 300,
    ) -> dict[str, Any]:
        self._require_vision_mutation()
        self._require_transport().swipe(x1, y1, x2, y2, duration_ms)
        return {"completed": True, "action": "swipe"}

    def android_adb_type_text(self, text: str) -> dict[str, Any]:
        self._require_vision_mutation()
        self._require_transport().type_text(text)
        return {"completed": True, "action": "text"}

    def android_adb_key(self, key: str) -> dict[str, Any]:
        self._require_vision_mutation()
        self._require_transport().key(key)
        return {"completed": True, "action": "key"}

    def android_adb_open_app(self, alias: str) -> dict[str, Any]:
        self._require_vision_mutation()
        if not isinstance(alias, str) or not PLAN_NAME.fullmatch(alias):
            raise ValueError("app alias must be a bounded lowercase Prolog atom")
        engine = self._require_engine()
        result = engine.query_once(
            f"kb_android_control:android_app_package({alias}, Package)"
        )
        if not result:
            raise ValueError(f"unknown Android app alias: {alias}")
        package = self._text(result.get("Package"))
        self._require_transport().open_package(package)
        return {"completed": True, "action": "open_app", "alias": alias}

    def _require_vision_mutation(self) -> None:
        engine = self._require_engine()
        result = engine.query_once("kb_android_control:android_vision_policy(Policy)")
        policy = self._text(result.get("Policy")) if result else "disabled"
        if policy in {"disabled", "observe_only"}:
            raise PermissionError(f"Android vision mutation policy is {policy}")
        if policy != "confirm_each_action":
            raise PermissionError("unknown Android vision mutation policy")

    def _parse_actions(self, raw: Any) -> tuple[tuple[Any, ...], ...]:
        if not isinstance(raw, (list, tuple)):
            raise ValueError("android_adb_plan/2 must return a list")
        if len(raw) > MAX_PLAN_ACTIONS:
            raise ValueError("Android ADB plan exceeds action limit")
        return tuple(self._parse_action(value) for value in raw)

    def _parse_action(self, value: Any) -> tuple[Any, ...]:
        name = getattr(value, "name", None)
        args = getattr(value, "args", None)
        if not isinstance(name, str) or args is None:
            raise ValueError("Android ADB plan contains a non-compound action")
        values = list(args)
        if name == "tap" and len(values) == 2:
            return ("tap", self._integer(values[0]), self._integer(values[1]))
        if name == "swipe" and len(values) == 5:
            return (
                "swipe",
                self._integer(values[0]),
                self._integer(values[1]),
                self._integer(values[2]),
                self._integer(values[3]),
                self._integer(values[4]),
            )
        if name == "text" and len(values) == 1:
            return ("text", self._text(values[0]))
        if name == "key" and len(values) == 1:
            return ("key", self._text(values[0]))
        if name == "wait" and len(values) == 1:
            return ("wait", self._integer(values[0]))
        if name == "open_app" and len(values) == 1:
            return ("open_app", self._text(values[0]))
        raise ValueError(f"unsupported Android ADB plan action: {name}")

    def _execute_action(self, action: tuple[Any, ...]) -> None:
        transport = self._require_transport()
        name, *args = action
        if name == "tap":
            transport.tap(*args)
        elif name == "swipe":
            transport.swipe(*args)
        elif name == "text":
            transport.type_text(*args)
        elif name == "key":
            transport.key(*args)
        elif name == "wait":
            transport.wait(*args)
        elif name == "open_app":
            alias = str(args[0])
            if not PLAN_NAME.fullmatch(alias):
                raise ValueError("invalid app alias in Android ADB plan")
            result = self._require_engine().query_once(
                f"kb_android_control:android_app_package({alias}, Package)"
            )
            if not result:
                raise ValueError(f"unknown Android app alias: {alias}")
            transport.open_package(self._text(result.get("Package")))
        else:
            raise ValueError("unsupported Android ADB action")

    def _require_transport(self) -> AdbTransport:
        if self._transport is None:
            raise RuntimeError("android-adb plugin is not running")
        return self._transport

    def _require_engine(self) -> PrologEngine:
        if self._engine is None:
            raise RuntimeError("android-adb Prolog policy is not running")
        return self._engine

    @staticmethod
    def _integer(value: Any) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise ValueError("Android ADB action integer is invalid")
        return value

    @staticmethod
    def _text(value: Any) -> str:
        if isinstance(value, bytes):
            value = value.decode("utf-8")
        if isinstance(value, str):
            return value
        rendered = str(value)
        if not rendered:
            raise ValueError("Android ADB action text is empty")
        return rendered

    @staticmethod
    def _resolve_adb(value: str) -> Optional[str]:
        if not value:
            return None
        if "/" in value:
            path = Path(value).expanduser()
            return str(path) if path.is_file() else None
        return shutil.which(value)


def create_plugin():
    return AndroidAdbPlugin()
