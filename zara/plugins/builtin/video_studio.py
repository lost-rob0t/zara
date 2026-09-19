from __future__ import annotations

import os
from pathlib import Path
import re
import signal
import subprocess
import threading
from typing import Optional

from langchain_core.tools import StructuredTool

from zara.plugins import PluginMetadata, ServicePlugin


_SLUG = re.compile(r"^[a-z0-9]+(?:-[a-z0-9]+)*$")
_TOKEN = re.compile(r"^[a-zA-Z0-9][a-zA-Z0-9._-]{0,63}$")
_MAX_ERROR_CHARS = 1000
_DEFAULT_COMMAND_TIMEOUT = 1800.0
_DEFAULT_RECORD_STOP_TIMEOUT = 8.0


class VideoStudioPlugin(ServicePlugin):
    enabled_by_default = False
    metadata = PluginMetadata(
        name="video-studio",
        version="0.1.0",
        description="Record and build checked-in StarIntel video projects.",
    )

    def __init__(self) -> None:
        self._configuration: dict = {}
        self._social_root: Optional[Path] = None
        self._use_nix = True
        self._command_timeout = _DEFAULT_COMMAND_TIMEOUT
        self._record_stop_timeout = _DEFAULT_RECORD_STOP_TIMEOUT
        self._record_lock = threading.RLock()
        self._record_process: Optional[subprocess.Popen] = None
        self._record_slug: Optional[str] = None

    def tools(self):
        read_metadata = {"zara_requires_approval": False}
        write_metadata = {"zara_requires_approval": True}
        return [
            StructuredTool.from_function(
                self._plan,
                name="video_plan",
                description=(
                    "Inspect the deterministic StarIntel video production plan "
                    "for a checked-in project slug."
                ),
                metadata=read_metadata,
            ),
            StructuredTool.from_function(
                self._record_start,
                name="video_record_start",
                description=(
                    "Start one approved StarIntel screen-recording session for "
                    "a checked-in video project."
                ),
                metadata=write_metadata,
            ),
            StructuredTool.from_function(
                self._record_stop,
                name="video_record_stop",
                description="Stop Zara's active StarIntel video recording.",
                metadata=write_metadata,
            ),
            StructuredTool.from_function(
                self._record_status,
                name="video_record_status",
                description="Report whether Zara has an active video recording.",
                metadata=read_metadata,
            ),
            StructuredTool.from_function(
                self._render,
                name="video_render",
                description=(
                    "Render one variant or every configured variant for a "
                    "checked-in StarIntel video project."
                ),
                metadata=write_metadata,
            ),
            StructuredTool.from_function(
                self._generate,
                name="video_generate",
                description=(
                    "Run the configured AI-video adapter for one shot or every "
                    "configured synthetic shot in a checked-in project."
                ),
                metadata=write_metadata,
            ),
            StructuredTool.from_function(
                self._package,
                name="video_package",
                description=(
                    "Prepare local YouTube/ads metadata and asset paths for a "
                    "checked-in StarIntel video project."
                ),
                metadata=write_metadata,
            ),
        ]

    def start(self, runtime) -> None:
        configuration = dict(runtime.configuration)
        root_value = str(configuration.get("social_root", "")).strip()
        if not root_value:
            raise ValueError("video-studio requires plugins.video-studio.social_root")

        root = Path(
            os.path.expanduser(os.path.expandvars(root_value))
        ).resolve()
        if not root.is_dir():
            raise ValueError("video-studio social_root must be an existing directory")
        module_root = root / "video_studio" / "src"
        if not module_root.is_dir():
            raise ValueError(
                "video-studio social_root does not contain video_studio/src"
            )

        use_nix = configuration.get("use_nix", True)
        if not isinstance(use_nix, bool):
            raise ValueError("video-studio use_nix must be true or false")

        command_timeout = float(
            configuration.get("command_timeout_seconds", _DEFAULT_COMMAND_TIMEOUT)
        )
        if not 1.0 <= command_timeout <= 21600.0:
            raise ValueError(
                "video-studio command_timeout_seconds must be between 1 and 21600"
            )

        stop_timeout = float(
            configuration.get(
                "record_stop_timeout_seconds",
                _DEFAULT_RECORD_STOP_TIMEOUT,
            )
        )
        if not 0.5 <= stop_timeout <= 60.0:
            raise ValueError(
                "video-studio record_stop_timeout_seconds must be between 0.5 and 60"
            )

        self._configuration = configuration
        self._social_root = root
        self._use_nix = use_nix
        self._command_timeout = command_timeout
        self._record_stop_timeout = stop_timeout

    def stop(self) -> None:
        self._stop_active_recording()

    def _require_root(self) -> Path:
        root = self._social_root
        if root is None:
            raise RuntimeError("video-studio plugin is not started")
        return root

    def _project_path(self, slug: str) -> Path:
        slug = str(slug).strip()
        if not _SLUG.fullmatch(slug):
            raise ValueError("video project slug must be lowercase kebab-case")
        root = self._require_root()
        path = (
            root
            / "youtube"
            / "video-source"
            / slug
            / "video.json"
        ).resolve()
        source_root = (root / "youtube" / "video-source").resolve()
        if source_root not in path.parents:
            raise ValueError("video project slug escaped the source root")
        if not path.is_file():
            raise ValueError(f"video project does not exist: {slug}")
        return path

    @staticmethod
    def _optional_token(value: Optional[str], field: str) -> Optional[str]:
        if value is None:
            return None
        token = str(value).strip()
        if not _TOKEN.fullmatch(token):
            raise ValueError(
                f"{field} must contain only letters, digits, '.', '_' or '-'"
            )
        return token

    def _environment(self) -> dict[str, str]:
        root = self._require_root()
        environment = dict(os.environ)
        source = str((root / "video_studio" / "src").resolve())
        existing = environment.get("PYTHONPATH", "")
        environment["PYTHONPATH"] = (
            source if not existing else source + os.pathsep + existing
        )
        adapter = self._configuration.get("ai_video_command_json")
        if adapter is not None:
            if not isinstance(adapter, str):
                raise ValueError(
                    "video-studio ai_video_command_json must be a string"
                )
            environment["STARINTEL_AI_VIDEO_COMMAND_JSON"] = adapter
        return environment

    def _command(self, slug: str, action: str) -> list[str]:
        root = self._require_root()
        project = self._project_path(slug)
        prefix = ["nix", "develop", "--command"] if self._use_nix else []
        return prefix + [
            "python3",
            "-m",
            "starintel_video_studio.cli",
            "--root",
            str(root),
            "--project",
            str(project),
            action,
        ]

    @staticmethod
    def _bounded_error(value: str) -> str:
        normalized = " ".join(str(value).split())
        return normalized[:_MAX_ERROR_CHARS]

    def _run(self, command: list[str]) -> str:
        root = self._require_root()
        try:
            result = subprocess.run(
                command,
                cwd=root,
                env=self._environment(),
                capture_output=True,
                text=True,
                timeout=self._command_timeout,
                check=False,
            )
        except FileNotFoundError as error:
            raise RuntimeError(
                f"video command is unavailable: {command[0]}"
            ) from error
        except subprocess.TimeoutExpired as error:
            raise RuntimeError(
                f"video command exceeded {self._command_timeout:g} seconds"
            ) from error

        if result.returncode != 0:
            detail = self._bounded_error(result.stderr or result.stdout)
            suffix = f": {detail}" if detail else ""
            raise RuntimeError(
                f"video command failed with exit {result.returncode}{suffix}"
            )
        return result.stdout.strip() or "Completed."

    def _plan(self, slug: str) -> str:
        return self._run(self._command(slug, "plan"))

    def _record_start(
        self,
        slug: str,
        geometry: Optional[str] = None,
        audio: bool = True,
    ) -> str:
        if not isinstance(audio, bool):
            raise ValueError("audio must be true or false")
        if geometry is not None:
            geometry = str(geometry).strip()
            if not geometry or len(geometry) > 128:
                raise ValueError("geometry must contain 1 to 128 characters")

        root = self._require_root()
        with self._record_lock:
            process = self._record_process
            if process is not None and process.poll() is None:
                raise RuntimeError(
                    f"video recording is already running for {self._record_slug}"
                )
            self._record_process = None
            self._record_slug = None

            command = self._command(slug, "record")
            if not audio:
                command.append("--no-audio")
            if geometry is not None:
                command.extend(["--geometry", geometry])

            try:
                process = subprocess.Popen(
                    command,
                    cwd=root,
                    env=self._environment(),
                    stdin=subprocess.DEVNULL,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    start_new_session=True,
                )
            except FileNotFoundError as error:
                raise RuntimeError(
                    f"video recording command is unavailable: {command[0]}"
                ) from error

            self._record_process = process
            self._record_slug = slug
            return f"Video recording started for {slug} (pid {process.pid})."

    def _record_status(self) -> str:
        with self._record_lock:
            process = self._record_process
            if process is None:
                return "No video recording is running."
            returncode = process.poll()
            if returncode is None:
                return (
                    f"Video recording is running for {self._record_slug} "
                    f"(pid {process.pid})."
                )
            slug = self._record_slug
            self._record_process = None
            self._record_slug = None
            return (
                f"Video recording for {slug} exited with code {returncode}."
            )

    def _stop_process_group(self, process: subprocess.Popen) -> None:
        if process.poll() is not None:
            return
        try:
            os.killpg(process.pid, signal.SIGINT)
        except ProcessLookupError:
            return
        try:
            process.wait(timeout=self._record_stop_timeout)
            return
        except subprocess.TimeoutExpired:
            pass

        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            return
        try:
            process.wait(timeout=2.0)
            return
        except subprocess.TimeoutExpired:
            pass

        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            return
        try:
            process.wait(timeout=2.0)
        except subprocess.TimeoutExpired as error:
            raise RuntimeError("video recording process group did not stop") from error

    def _stop_active_recording(self) -> Optional[str]:
        with self._record_lock:
            process = self._record_process
            slug = self._record_slug
            if process is None:
                return None
            try:
                self._stop_process_group(process)
            finally:
                self._record_process = None
                self._record_slug = None
            return slug

    def _record_stop(self) -> str:
        slug = self._stop_active_recording()
        if slug is None:
            return "No video recording is running."
        return f"Video recording stopped for {slug}."

    def _render(
        self,
        slug: str,
        variant: Optional[str] = None,
    ) -> str:
        command = self._command(slug, "render")
        variant = self._optional_token(variant, "variant")
        if variant is not None:
            command.extend(["--variant", variant])
        return self._run(command)

    def _generate(
        self,
        slug: str,
        shot: Optional[str] = None,
    ) -> str:
        command = self._command(slug, "generate")
        shot = self._optional_token(shot, "shot")
        if shot is not None:
            command.extend(["--shot", shot])
        return self._run(command)

    def _package(self, slug: str) -> str:
        return self._run(self._command(slug, "package"))


def create_plugin() -> VideoStudioPlugin:
    return VideoStudioPlugin()
