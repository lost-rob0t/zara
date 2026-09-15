"""Async client for the OpenAI-compatible Qwen3-TTS (qwentts.cpp) server.

Endpoints: POST /v1/audio/speech (response_format "pcm" streams s16le
24 kHz mono as it is generated, "wav" returns a one-shot RIFF file),
GET/POST /v1/audio/voices, DELETE /v1/audio/voices/{name}, GET /health.
"""

import base64
from pathlib import Path
from typing import AsyncIterator, Optional

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
        self.stream_timeout = aiohttp.ClientTimeout(
            total=None,
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

    async def health(self) -> dict:
        session = await self._ensure_session()
        async with session.get(f"{self.base_url}/health") as response:
            await self._raise_for_status(response)
            return await response.json()

    async def list_voices(self) -> list[str]:
        session = await self._ensure_session()
        async with session.get(f"{self.base_url}/v1/audio/voices") as response:
            await self._raise_for_status(response)
            payload = await response.json()
        return [entry["name"] for entry in payload.get("voices", [])]

    async def register_voice(
        self,
        name: str,
        wav_file_path: str,
        reference_text: str = "",
    ) -> dict:
        path = Path(wav_file_path)
        if not path.is_file():
            raise FileNotFoundError(f"File not found: {wav_file_path}")

        wav_b64 = base64.b64encode(path.read_bytes()).decode("ascii")
        body = {"name": name, "wav_b64": wav_b64}
        if reference_text:
            body["ref_text"] = reference_text

        session = await self._ensure_session()
        async with session.post(
            f"{self.base_url}/v1/audio/voices", json=body
        ) as response:
            await self._raise_for_status(response)
            return await response.json()

    async def delete_voice(self, name: str) -> dict:
        session = await self._ensure_session()
        async with session.delete(
            f"{self.base_url}/v1/audio/voices/{name}"
        ) as response:
            await self._raise_for_status(response)
            return await response.json(content_type=None)

    async def synthesize_speech(
        self,
        text: str,
        voice: str = "zara",
        speed: float = 1.0,
    ) -> bytes:
        session = await self._ensure_session()
        body = {
            "input": text,
            "voice": voice,
            "response_format": "wav",
        }
        async with session.post(
            f"{self.base_url}/v1/audio/speech", json=body, timeout=self.timeout
        ) as response:
            await self._raise_for_status(response)
            return await response.read()

    async def stream_speech(
        self,
        text: str,
        voice: str = "zara",
        speed: float = 1.0,
    ) -> AsyncIterator[bytes]:
        """Yield s16le 24 kHz mono PCM chunks as the server generates them.

        No total timeout: a long utterance may legitimately outlive the
        buffered-request budget; only connect and per-read bounds apply.
        """
        session = await self._ensure_session()
        body = {
            "input": text,
            "voice": voice,
            "response_format": "pcm",
        }
        async with session.post(
            f"{self.base_url}/v1/audio/speech", json=body, timeout=self.stream_timeout
        ) as response:
            await self._raise_for_status(response)
            async for chunk in response.content.iter_any():
                if chunk:
                    yield chunk

    @staticmethod
    async def _raise_for_status(response: aiohttp.ClientResponse) -> None:
        if response.status != 200:
            detail = await response.text()
            raise RuntimeError(f"Qwen3-TTS returned {response.status}: {detail}")
