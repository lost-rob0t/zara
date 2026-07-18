"""Async Qwen3-TTS HTTP client."""

from pathlib import Path
from typing import Optional

import aiohttp


class Qwen3TTSClient:
    def __init__(
        self,
        base_url: str = "http://localhost:7860",
        total_timeout: float = 30.0,
        connect_timeout: float = 5.0,
        read_timeout: float = 20.0,
    ):
        self.base_url = base_url.rstrip("/")
        self.timeout = aiohttp.ClientTimeout(
            total=total_timeout,
            connect=connect_timeout,
            sock_read=read_timeout,
        )
        self.session: Optional[aiohttp.ClientSession] = None

    async def __aenter__(self):
        await self._ensure_session()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.close()

    async def _ensure_session(self) -> aiohttp.ClientSession:
        if self.session is None or self.session.closed:
            self.session = aiohttp.ClientSession(timeout=self.timeout)
        return self.session

    async def close(self) -> None:
        if self.session is not None:
            await self.session.close()
            self.session = None

    async def synthesize_speech(
        self,
        text: str,
        voice: str = "zara",
        speed: float = 1.0,
    ) -> bytes:
        return await self._get_audio(
            "synthesize_speech/",
            {"text": text, "voice": voice, "speed": speed},
        )

    async def base_tts(self, text: str, speed: float = 1.0) -> bytes:
        return await self._get_audio(
            "base_tts/",
            {"text": text, "speed": speed},
        )

    async def _get_audio(self, endpoint: str, params: dict) -> bytes:
        session = await self._ensure_session()
        async with session.get(f"{self.base_url}/{endpoint}", params=params) as response:
            if response.status != 200:
                detail = await response.text()
                raise RuntimeError(f"Qwen3-TTS returned {response.status}: {detail}")
            return await response.read()

    async def upload_voice(self, audio_file_path: str, voice_label: str) -> dict:
        path = Path(audio_file_path)
        if not path.is_file():
            return {"error": f"File not found: {audio_file_path}"}

        session = await self._ensure_session()
        data = aiohttp.FormData()
        data.add_field("audio_file_label", voice_label)
        with path.open("rb") as audio_file:
            data.add_field(
                "file",
                audio_file,
                filename=path.name,
                content_type="audio/mpeg",
            )
            async with session.post(
                f"{self.base_url}/upload_audio/", data=data
            ) as response:
                if response.status == 200:
                    return await response.json()
                detail = await response.text()
                return {"error": f"{response.status} - {detail}"}

    async def change_voice(
        self,
        audio_file_path: str,
        reference_speaker: str,
    ) -> bytes:
        path = Path(audio_file_path)
        if not path.is_file():
            raise FileNotFoundError(f"File not found: {audio_file_path}")

        session = await self._ensure_session()
        data = aiohttp.FormData()
        data.add_field("reference_speaker", reference_speaker)
        with path.open("rb") as audio_file:
            data.add_field(
                "file",
                audio_file,
                filename=path.name,
                content_type="audio/wav",
            )
            async with session.post(
                f"{self.base_url}/change_voice/", data=data
            ) as response:
                if response.status != 200:
                    detail = await response.text()
                    raise RuntimeError(
                        f"Qwen3-TTS returned {response.status}: {detail}"
                    )
                return await response.read()
