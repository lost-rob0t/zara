"""TTS provider contracts and lifecycle management."""

import asyncio
import os
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from .qwen import Qwen3TTSClient


@dataclass(frozen=True)
class SynthesisResult:
    provider: str
    success: bool
    audio: bytes = b""
    audio_format: Optional[str] = None
    error: Optional[str] = None
    cancelled: bool = False


@dataclass(frozen=True)
class PlaybackResult:
    provider: str
    success: bool
    error: Optional[str] = None
    cancelled: bool = False


class TTSEngine:
    def __init__(self, provider: str = "local", config: Optional[dict] = None):
        self.provider = "qwen3" if provider == "qwen" else provider
        self.config = config or {}
        self.qwen3_client = None
        tts_config = self.config.get("tts", {}) or {}
        self.total_timeout = float(tts_config.get("total_timeout", 30.0))
        self.connect_timeout = float(tts_config.get("connect_timeout", 5.0))
        self.read_timeout = float(tts_config.get("read_timeout", 20.0))

        if self.provider == "local":
            self._init_local()
        elif self.provider == "11labs":
            self._init_elevenlabs()
        elif self.provider == "edge":
            self._init_edge()
        elif self.provider == "qwen3":
            self._init_qwen3()
        else:
            raise ValueError(f"Unknown TTS provider: {self.provider}")

    def _init_local(self) -> None:
        if subprocess.run(["which", "piper"], capture_output=True).returncode == 0:
            self.local_engine = "piper"
            self.piper_model = (self.config.get("tts", {}) or {}).get(
                "model_path",
                "~/.local/share/piper/en_US-lessac-medium.onnx",
            )
        else:
            self.local_engine = "espeak"

    def _init_elevenlabs(self) -> None:
        tts_config = self.config.get("tts", {}) or {}
        if not tts_config.get("elevenlabs_api_key"):
            raise ValueError("tts.elevenlabs_api_key missing in config")
        if not tts_config.get("elevenlabs_voice_id"):
            raise ValueError("tts.elevenlabs_voice_id missing in config")
        output_format = tts_config.get("elevenlabs_output_format", "mp3_44100_128")
        if not str(output_format).startswith("mp3"):
            raise ValueError("Only MP3 ElevenLabs output is currently playable")

    def _init_edge(self) -> None:
        import edge_tts

        self.edge_tts = edge_tts
        self.voice = (self.config.get("tts", {}) or {}).get(
            "edge_voice", "en-US-GuyNeural"
        )

    def _init_qwen3(self) -> None:
        tts_config = self.config.get("tts", {}) or {}
        self.qwen3_url = tts_config.get(
            "endpoint", os.getenv("QWEN3_TTS_URL", "http://localhost:7860")
        )
        self.qwen3_voice = tts_config.get("voice", os.getenv("QWEN3_VOICE", "zara"))

    async def synthesize_async(self, text: str) -> SynthesisResult:
        if not text or not text.strip():
            return self._failure("Text is empty")

        try:
            audio, audio_format = await asyncio.wait_for(
                self._synthesize_provider(text),
                timeout=self.total_timeout,
            )
        except asyncio.TimeoutError:
            return self._failure(
                f"Synthesis timed out after {self.total_timeout:g} seconds"
            )
        except asyncio.CancelledError:
            return self._failure("Synthesis cancelled", cancelled=True)
        except Exception as error:
            return self._failure(str(error))

        if not audio:
            return self._failure("Provider returned empty audio")
        if not self._valid_audio(audio, audio_format):
            return self._failure(f"Provider returned invalid {audio_format.upper()} audio")
        return SynthesisResult(
            provider=self.provider,
            success=True,
            audio=audio,
            audio_format=audio_format,
        )

    def synthesize(self, text: str) -> SynthesisResult:
        return asyncio.run(self.synthesize_async(text))

    async def _synthesize_provider(self, text: str) -> tuple[bytes, str]:
        if self.provider == "local":
            return await self._synthesize_local(text)
        if self.provider == "11labs":
            return await self._synthesize_elevenlabs(text)
        if self.provider == "edge":
            return await self._synthesize_edge(text)
        if self.provider == "qwen3":
            return await self._synthesize_qwen3(text)
        raise RuntimeError(f"Unsupported TTS provider: {self.provider}")

    async def _synthesize_local(self, text: str) -> tuple[bytes, str]:
        if self.local_engine == "piper":
            model_path = Path(self.piper_model).expanduser()
            if not model_path.is_file():
                raise RuntimeError(f"Piper model not found: {model_path}")
            process = await asyncio.create_subprocess_exec(
                "piper",
                "-m",
                str(model_path),
                "--output_file",
                "-",
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            audio = await self._communicate(process, text.encode())
            return audio, "wav"

        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as output:
            output_path = Path(output.name)
        try:
            process = await asyncio.create_subprocess_exec(
                "espeak",
                "-w",
                str(output_path),
                text,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            await self._communicate(process)
            return output_path.read_bytes(), "wav"
        finally:
            output_path.unlink(missing_ok=True)

    async def _communicate(
        self,
        process: asyncio.subprocess.Process,
        input_data: Optional[bytes] = None,
    ) -> bytes:
        try:
            stdout, stderr = await process.communicate(input_data)
        except asyncio.CancelledError:
            if process.returncode is None:
                process.kill()
                await process.wait()
            raise
        if process.returncode != 0:
            detail = stderr.decode(errors="replace").strip()
            raise RuntimeError(detail or f"TTS process exited with {process.returncode}")
        return stdout

    async def _synthesize_elevenlabs(self, text: str) -> tuple[bytes, str]:
        from . import elevenlabs

        tts_config = self.config.get("tts", {}) or {}
        try:
            return await elevenlabs.synthesize(text, tts_config), "mp3"
        finally:
            await elevenlabs.close()

    async def _synthesize_edge(self, text: str) -> tuple[bytes, str]:
        with tempfile.NamedTemporaryFile(suffix=".mp3", delete=False) as output:
            output_path = Path(output.name)
        try:
            communicate = self.edge_tts.Communicate(text, self.voice)
            await communicate.save(str(output_path))
            return output_path.read_bytes(), "mp3"
        finally:
            output_path.unlink(missing_ok=True)

    async def _synthesize_qwen3(self, text: str) -> tuple[bytes, str]:
        if self.qwen3_client is None:
            self.qwen3_client = Qwen3TTSClient(
                self.qwen3_url,
                total_timeout=self.total_timeout,
                connect_timeout=self.connect_timeout,
                read_timeout=self.read_timeout,
            )
        client = self.qwen3_client
        try:
            audio = await client.synthesize_speech(
                text=text,
                voice=self.qwen3_voice,
            )
            return audio, "wav"
        finally:
            await client.close()
            if self.qwen3_client is client:
                self.qwen3_client = None

    def _failure(self, error: str, cancelled: bool = False) -> SynthesisResult:
        return SynthesisResult(
            provider=self.provider,
            success=False,
            error=error,
            cancelled=cancelled,
        )

    @staticmethod
    def _valid_audio(audio: bytes, audio_format: str) -> bool:
        if audio_format == "wav":
            return len(audio) >= 12 and audio[:4] == b"RIFF" and audio[8:12] == b"WAVE"
        if audio_format == "mp3":
            return audio.startswith(b"ID3") or (
                len(audio) >= 2 and audio[0] == 0xFF and audio[1] & 0xE0 == 0xE0
            )
        return False

    async def close(self) -> None:
        if self.qwen3_client:
            await self.qwen3_client.close()
            self.qwen3_client = None
        if self.provider == "11labs":
            from . import elevenlabs

            await elevenlabs.close()
