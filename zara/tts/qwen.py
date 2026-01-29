"""
Qwen3-TTS HTTP Client
Async client for the Qwen3-TTS server API
"""

import asyncio
import aiohttp
from typing import Optional
from pathlib import Path


class Qwen3TTSClient:
    """
    Async HTTP client for Qwen3-TTS server
    """

    def __init__(self, base_url: str = "http://localhost:7860"):
        self.base_url = base_url.rstrip("/")
        self.session: Optional[aiohttp.ClientSession] = None

    async def __aenter__(self):
        """Async context manager entry"""
        self.session = aiohttp.ClientSession()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit"""
        if self.session:
            await self.session.close()

    async def _ensure_session(self):
        """Ensure session exists"""
        if self.session is None:
            self.session = aiohttp.ClientSession()

    async def close(self):
        """Close the session"""
        if self.session:
            await self.session.close()
            self.session = None

    async def synthesize_speech(
        self,
        text: str,
        voice: str = "zara",
        speed: float = 1.0
    ) -> Optional[bytes]:
        """
        Synthesize speech from text using specified voice

        Args:
            text: Text to synthesize
            voice: Voice label (filename prefix in resources/)
            speed: Speech speed multiplier (default: 1.0)

        Returns:
            Audio bytes (WAV format) or None on error
        """
        await self._ensure_session()

        try:
            params = {
                "text": text,
                "voice": voice,
                "speed": speed
            }

            url = f"{self.base_url}/synthesize_speech/"

            async with self.session.get(url, params=params) as response:
                if response.status == 200:
                    return await response.read()
                else:
                    error_text = await response.text()
                    print(f"Qwen3-TTS error: {response.status} - {error_text}")
                    return None

        except aiohttp.ClientError as e:
            print(f"HTTP client error: {e}")
            return None
        except Exception as e:
            print(f"Unexpected error in Qwen3 TTS: {e}")
            return None

    async def base_tts(self, text: str, speed: float = 1.0) -> Optional[bytes]:
        """
        Generate speech using default English voice

        Args:
            text: Text to synthesize
            speed: Speech speed multiplier (default: 1.0)

        Returns:
            Audio bytes (WAV format) or None on error
        """
        await self._ensure_session()

        try:
            params = {
                "text": text,
                "speed": speed
            }

            url = f"{self.base_url}/base_tts/"

            async with self.session.get(url, params=params) as response:
                if response.status == 200:
                    return await response.read()
                else:
                    error_text = await response.text()
                    print(f"Qwen3-TTS error: {response.status} - {error_text}")
                    return None

        except aiohttp.ClientError as e:
            print(f"HTTP client error: {e}")
            return None
        except Exception as e:
            print(f"Unexpected error in Qwen3 TTS: {e}")
            return None

    async def upload_voice(
        self,
        audio_file_path: str,
        voice_label: str
    ) -> dict:
        """
        Upload an audio file to use as reference voice

        Args:
            audio_file_path: Path to audio file (wav, mp3, flac, ogg)
            voice_label: Label/name for the voice

        Returns:
            Response dict with message or error
        """
        await self._ensure_session()

        try:
            path = Path(audio_file_path)
            if not path.exists():
                return {"error": f"File not found: {audio_file_path}"}

            url = f"{self.base_url}/upload_audio/"

            data = aiohttp.FormData()
            data.add_field("audio_file_label", voice_label)
            data.add_field(
                "file",
                open(audio_file_path, "rb"),
                filename=path.name,
                content_type="audio/mpeg"
            )

            async with self.session.post(url, data=data) as response:
                if response.status == 200:
                    return await response.json()
                else:
                    error_text = await response.text()
                    return {"error": f"{response.status} - {error_text}"}

        except Exception as e:
            return {"error": str(e)}

    async def change_voice(
        self,
        audio_file_path: str,
        reference_speaker: str
    ) -> Optional[bytes]:
        """
        Convert the voice of an existing audio file

        Args:
            audio_file_path: Path to audio file to convert
            reference_speaker: Voice label to convert to

        Returns:
            Audio bytes (WAV format) or None on error
        """
        await self._ensure_session()

        try:
            path = Path(audio_file_path)
            if not path.exists():
                print(f"File not found: {audio_file_path}")
                return None

            url = f"{self.base_url}/change_voice/"

            data = aiohttp.FormData()
            data.add_field("reference_speaker", reference_speaker)
            data.add_field(
                "file",
                open(audio_file_path, "rb"),
                filename=path.name,
                content_type="audio/wav"
            )

            async with self.session.post(url, data=data) as response:
                if response.status == 200:
                    return await response.read()
                else:
                    error_text = await response.text()
                    print(f"Qwen3-TTS error: {response.status} - {error_text}")
                    return None

        except Exception as e:
            print(f"Error in change_voice: {e}")
            return None


if __name__ == "__main__":
    # Test the client
    async def test():
        async with Qwen3TTSClient() as client:
            # Test basic TTS
            audio = await client.base_tts("Thus spoke Zarathustra")

            if audio:
                print(f"Generated {len(audio)} bytes of audio")

                # Save to file
                with open("/tmp/test_qwen3.wav", "wb") as f:
                    f.write(audio)
                print("Saved to /tmp/test_qwen3.wav")
            else:
                print("Failed to generate audio")

    asyncio.run(test())
