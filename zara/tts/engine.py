"""
TTS Engine - provider abstraction
"""

import asyncio
import os
import subprocess
from pathlib import Path
from typing import Optional

from .qwen import Qwen3TTSClient


class TTSEngine:

    def __init__(self, provider: str = "local", config: Optional[dict] = None):
        self.provider = provider
        self.config = config or {}
        self.qwen3_client = None

        if provider == "local":
            self._init_local()
        elif provider == "11labs":
            self._init_elevenlabs()
        elif provider == "edge":
            self._init_edge()
        elif provider == "qwen3":
            self._init_qwen3()
        else:
            raise ValueError(f"Unknown TTS provider: {provider}")

    def _init_local(self):
        if subprocess.run(["which", "piper"], capture_output=True).returncode == 0:
            self.local_engine = "piper"
            self.piper_model = (self.config.get("tts", {}) or {}).get(
                "model_path",
                "~/.local/share/piper/en_US-lessac-medium.onnx"
            )
        else:
            self.local_engine = "espeak"

    def _init_elevenlabs(self):
        tts_cfg = self.config.get("tts", {}) or {}
        # config-only (your rule)
        if not tts_cfg.get("elevenlabs_api_key"):
            raise ValueError("tts.elevenlabs_api_key missing in config")
        if not tts_cfg.get("elevenlabs_voice_id"):
            raise ValueError("tts.elevenlabs_voice_id missing in config")

    def _init_edge(self):
        import edge_tts
        self.edge_tts = edge_tts
        self.voice = (self.config.get("tts", {}) or {}).get("edge_voice", "en-US-GuyNeural")

    def _init_qwen3(self):
        tts_cfg = self.config.get("tts", {}) or {}
        self.qwen3_url = tts_cfg.get("endpoint", os.getenv("QWEN3_TTS_URL", "http://localhost:7860"))
        self.qwen3_voice = tts_cfg.get("voice", os.getenv("QWEN3_VOICE", "zara"))

    async def synthesize_async(self, text: str) -> Optional[bytes]:
        if self.provider == "local":
            return await self._synthesize_local(text)
        if self.provider == "11labs":
            return await self._synthesize_elevenlabs(text)
        if self.provider == "edge":
            return await self._synthesize_edge(text)
        if self.provider == "qwen3":
            return await self._synthesize_qwen3(text)
        raise RuntimeError("Invalid provider")

    def synthesize(self, text: str) -> Optional[bytes]:
        return asyncio.run(self.synthesize_async(text))

    async def _synthesize_local(self, text: str) -> Optional[bytes]:
        if self.local_engine == "piper":
            model_path = Path(self.piper_model).expanduser()
            if not model_path.exists():
                print(f"Piper model not found: {model_path}")
                return None

            cmd = ["piper", "-m", str(model_path), "--output-raw"]
            proc = await asyncio.create_subprocess_exec(
                *cmd,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            stdout, _ = await proc.communicate(text.encode())
            return stdout

        import tempfile
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            tmp = f.name

        proc = await asyncio.create_subprocess_exec("espeak", "-w", tmp, text)
        await proc.wait()

        data = Path(tmp).read_bytes()
        Path(tmp).unlink()
        return data

    async def _synthesize_elevenlabs(self, text: str) -> Optional[bytes]:
        try:
            from . import elevenlabs
            tts_cfg = self.config.get("tts", {}) or {}
            return await elevenlabs.synthesize(text, tts_cfg)
        except Exception as e:
            print(f"ElevenLabs error: {e}")
            return None

    def synthesize_streaming_async(self, text: str):
        """
        Synthesize speech with streaming support (where available).
        Returns an async iterator that yields audio chunks as they're received.

        Only ElevenLabs currently supports streaming.
        Other providers will fall back to non-streaming behavior.
        """
        if self.provider == "11labs":
            return self._synthesize_elevenlabs_streaming(text)
        else:
            # Fallback: synthesize all at once and yield as single chunk
            return self._non_streaming_fallback(text)

    async def _non_streaming_fallback(self, text: str):
        """Fallback for providers without streaming support"""
        audio = await self.synthesize_async(text)
        if audio:
            yield audio

    async def _synthesize_elevenlabs_streaming(self, text: str):
        """Stream audio chunks from ElevenLabs"""
        try:
            from . import elevenlabs
            tts_cfg = self.config.get("tts", {}) or {}
            async for chunk in elevenlabs.synthesize_streaming(text, tts_cfg):
                yield chunk
        except Exception as e:
            print(f"ElevenLabs streaming error: {e}")
            return

    async def _synthesize_edge(self, text: str) -> Optional[bytes]:
        try:
            import tempfile
            with tempfile.NamedTemporaryFile(suffix=".mp3", delete=False) as f:
                tmp = f.name

            communicate = self.edge_tts.Communicate(text, self.voice)
            await communicate.save(tmp)

            data = Path(tmp).read_bytes()
            Path(tmp).unlink()
            return data
        except Exception as e:
            print(f"Edge TTS error: {e}")
            return None

    async def _synthesize_qwen3(self, text: str) -> Optional[bytes]:
        try:
            if self.qwen3_client is None:
                self.qwen3_client = Qwen3TTSClient(self.qwen3_url)

            return await self.qwen3_client.synthesize_speech(text=text, voice=self.qwen3_voice)
        except Exception as e:
            print(f"Qwen3 TTS error: {e}")
            return None

    async def close(self):
        if self.qwen3_client:
            await self.qwen3_client.close()
            self.qwen3_client = None

        # close module session
        if self.provider == "11labs":
            from . import elevenlabs
            await elevenlabs.close()
