"""
TTS Engine - text to speech with multiple backends
"""

import os
import asyncio
import subprocess
from typing import Optional
from pathlib import Path
from .qwen import Qwen3TTSClient


class TTSEngine:
    """
    Text-to-speech with provider abstraction
    Supports: local (espeak/piper), 11labs, edge-tts, qwen3
    """

    def __init__(self, provider: str = "local"):
        self.provider = provider

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
        """Initialize local TTS (espeak or piper)"""
        # Check for piper first (better quality)
        if subprocess.run(["which", "piper"], capture_output=True).returncode == 0:
            self.local_engine = "piper"
            # You'll need to download a piper model
            self.piper_model = os.getenv(
                "PIPER_MODEL",
                "~/.local/share/piper/en_US-lessac-medium.onnx"
            )
        else:
            # Fallback to espeak
            self.local_engine = "espeak"

    def _init_elevenlabs(self):
        """Initialize ElevenLabs API"""
        self.api_key = os.getenv("ELEVENLABS_API_KEY")
        if not self.api_key:
            raise ValueError("ELEVENLABS_API_KEY not set")

        self.voice_id = os.getenv("ELEVENLABS_VOICE_ID", "pNInz6obpgDQGcFmaJgB")  # Adam voice

        try:
            from elevenlabs import set_api_key, generate, stream
            set_api_key(self.api_key)
            self.elevenlabs_generate = generate
            self.elevenlabs_stream = stream
        except ImportError:
            raise ImportError("elevenlabs package not installed: pip install elevenlabs")

    def _init_edge(self):
        """Initialize edge-tts (free, good quality)"""
        try:
            import edge_tts
            self.edge_tts = edge_tts
            self.voice = os.getenv("EDGE_VOICE", "en-US-GuyNeural")
        except ImportError:
            raise ImportError("edge-tts not installed: pip install edge-tts")

    def _init_qwen3(self):
        """Initialize Qwen3-TTS HTTP client"""
        self.qwen3_url = os.getenv("QWEN3_TTS_URL", "http://localhost:7860")
        self.qwen3_voice = os.getenv("QWEN3_VOICE", "demo_speaker0")
        self.qwen3_client = None

    async def synthesize_async(self, text: str) -> Optional[bytes]:
        """
        Convert text to speech audio
        Returns: audio bytes (WAV format) or None
        """
        if self.provider == "local":
            return await self._synthesize_local(text)
        elif self.provider == "11labs":
            return await self._synthesize_elevenlabs(text)
        elif self.provider == "edge":
            return await self._synthesize_edge(text)
        elif self.provider == "qwen3":
            return await self._synthesize_qwen3(text)

    async def _synthesize_local(self, text: str) -> Optional[bytes]:
        """Synthesize with local engine"""
        if self.local_engine == "piper":
            # Piper: echo "text" | piper -m model.onnx -f output.wav
            model_path = Path(self.piper_model).expanduser()
            if not model_path.exists():
                print(f"Piper model not found: {model_path}")
                return None

            cmd = ["piper", "-m", str(model_path), "--output-raw"]
            proc = await asyncio.create_subprocess_exec(
                *cmd,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )

            stdout, _ = await proc.communicate(text.encode())
            return stdout

        else:  # espeak
            # espeak -w output.wav "text"
            import tempfile
            with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
                temp_file = f.name

            cmd = ["espeak", "-w", temp_file, text]
            proc = await asyncio.create_subprocess_exec(*cmd)
            await proc.wait()

            # Read the WAV file
            with open(temp_file, "rb") as f:
                audio = f.read()

            os.unlink(temp_file)
            return audio

    async def _synthesize_elevenlabs(self, text: str) -> Optional[bytes]:
        """Synthesize with ElevenLabs"""
        try:
            # Run in executor since elevenlabs is sync
            audio = await asyncio.get_event_loop().run_in_executor(
                None,
                lambda: self.elevenlabs_generate(
                    text=text,
                    voice=self.voice_id,
                    model="eleven_monolingual_v1"
                )
            )
            return audio
        except Exception as e:
            print(f"ElevenLabs error: {e}")
            return None

    async def _synthesize_edge(self, text: str) -> Optional[bytes]:
        """Synthesize with edge-tts"""
        try:
            import tempfile

            with tempfile.NamedTemporaryFile(suffix=".mp3", delete=False) as f:
                temp_file = f.name

            communicate = self.edge_tts.Communicate(text, self.voice)
            await communicate.save(temp_file)

            # Read the audio
            with open(temp_file, "rb") as f:
                audio = f.read()

            os.unlink(temp_file)
            return audio

        except Exception as e:
            print(f"Edge TTS error: {e}")
            return None

    async def _synthesize_qwen3(self, text: str) -> Optional[bytes]:
        """Synthesize with Qwen3-TTS"""
        try:
            # Create client if not exists
            if self.qwen3_client is None:
                self.qwen3_client = Qwen3TTSClient(self.qwen3_url)

            # Synthesize speech
            audio = await self.qwen3_client.synthesize_speech(
                text=text,
                voice=self.qwen3_voice
            )

            return audio

        except Exception as e:
            print(f"Qwen3 TTS error: {e}")
            return None

    async def close(self):
        """Close any open connections"""
        if self.provider == "qwen3" and self.qwen3_client:
            await self.qwen3_client.close()

    def synthesize(self, text: str) -> Optional[bytes]:
        """Synchronous wrapper"""
        return asyncio.run(self.synthesize_async(text))


if __name__ == "__main__":
    # Test
    async def test():
        # Test with local (espeak)
        tts = TTSEngine("local")
        audio = await tts.synthesize_async("Thus spoke Zarathustra")

        if audio:
            print(f"Generated {len(audio)} bytes of audio")

            # Save to file for testing
            with open("/tmp/test_tts.wav", "wb") as f:
                f.write(audio)
            print("Saved to /tmp/test_tts.wav")

    asyncio.run(test())
