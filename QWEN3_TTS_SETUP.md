# Qwen3-TTS Integration for Zara

This document describes how to deploy and use the Qwen3-TTS server with Zara.

## Quick Start

### 1. Build the Qwen3-TTS Docker Image

The modified server is available in `~/git/Qwen3-TTS_server` with CPU and CUDA support.

```bash
cd ~/git/Qwen3-TTS_server

# Build CPU version (no GPU required)
./build.sh cpu

# Build CUDA version (requires NVIDIA GPU)
./build.sh cuda

# Build both versions
./build.sh both
```

### 2. Start the TTS Server

```bash
# From the Zarathushtra project directory
docker-compose up -d qwen3-tts
```

This will start the Qwen3-TTS server on port 7860.

### 3. Configure Zara to Use Qwen3-TTS

Set the following environment variables:

```bash
export ZARA_TTS_PROVIDER=qwen3
export QWEN3_TTS_URL=http://localhost:7860
export QWEN3_VOICE=demo_speaker0
```

### 4. Test the Integration

```python
# Test the TTS client directly
python -m zara.tts.qwen

# Or use it in wake mode
nix run .#zara -- --wake
```

## Configuration

### Environment Variables

- `ZARA_TTS_PROVIDER`: TTS provider to use (default: "qwen3")
  - Options: "local", "11labs", "edge", "qwen3"
- `QWEN3_TTS_URL`: URL of the Qwen3-TTS server (default: "http://localhost:7860")
- `QWEN3_VOICE`: Voice to use (default: "demo_speaker0")

### Docker Compose Configuration

The `docker-compose.yml` includes:
- GPU access (requires NVIDIA Docker runtime)
- Volume mounts for voice resources and outputs
- Health checks
- Auto-restart policy

## API Endpoints

The Qwen3-TTS server provides the following endpoints:

### GET /synthesize_speech/
Generate speech from text using a specified voice.

**Parameters:**
- `text` (string, required): Text to synthesize
- `voice` (string, required): Voice label (e.g., "demo_speaker0")
- `speed` (float, optional): Speech speed multiplier (default: 1.0)

**Example:**
```bash
curl "http://localhost:7860/synthesize_speech/?text=Hello%20world&voice=demo_speaker0" \
  --output output.wav
```

### GET /base_tts/
Generate speech using the default English voice.

**Parameters:**
- `text` (string, required): Text to synthesize
- `speed` (float, optional): Speech speed multiplier (default: 1.0)

### POST /upload_audio/
Upload an audio file to use as a reference voice.

**Form Fields:**
- `audio_file_label` (string): Label/name for the voice
- `file` (file): Audio file (wav, mp3, flac, ogg; max 5MB)

**Example:**
```bash
curl -X POST "http://localhost:7860/upload_audio/" \
  -F "audio_file_label=my_voice" \
  -F "file=@/path/to/voice_sample.mp3"
```

### POST /change_voice/
Convert the voice of an existing audio file.

**Form Fields:**
- `reference_speaker` (string): Voice label to convert to
- `file` (file): Audio file to convert

## Architecture

### Async HTTP Client

The Qwen3 TTS client (`zara/tts/qwen.py`) uses `aiohttp` for async HTTP requests:

- Persistent session management
- Context manager support for automatic cleanup
- Error handling and retry logic
- Streaming response handling for audio data

### Integration with Wake Mode

The wake word listener now supports TTS responses:

1. User speaks a query after wake word
2. System routes to either Prolog (execution) or LLM (conversation)
3. LLM generates response text
4. Qwen3-TTS synthesizes the response
5. Audio is played back to user via `aplay`

## Features Implemented

1. **Docker Deployment**: One-command deployment with docker-compose
2. **Async HTTP Client**: High-performance async client for TTS requests
3. **TTS Provider Integration**: Seamless integration with existing TTS abstraction
4. **LLM Response with TTS**: Wake mode now speaks back LLM responses
5. **Voice Cloning Support**: Upload custom voices via API
6. **Multi-language Support**: 10 languages supported out of the box

## Troubleshooting

### Server not starting
- Check GPU availability: `nvidia-smi`
- Verify Docker has GPU access: `docker run --rm --gpus all nvidia/cuda:12.8.0-base-ubuntu22.04 nvidia-smi`
- Check logs: `docker-compose logs qwen3-tts`

### Connection refused
- Ensure server is running: `docker ps`
- Check port is accessible: `curl http://localhost:7860/docs`

### No audio output
- Check `aplay` is installed: `which aplay`
- Test audio system: `speaker-test -t wav -c 2`
- Verify audio device permissions

## Performance

- **First request latency**: ~2-5 seconds (model warmup)
- **Subsequent requests**: ~0.5-1 second (varies by text length)
- **GPU memory**: ~4GB VRAM for 1.7B model
- **Audio quality**: 24kHz, 16-bit WAV output

## Next Steps

1. Add support for streaming TTS (partial audio playback)
2. Implement voice caching for frequently used responses
3. Add emotion/style control parameters
4. Create custom voice profiles for Zara

## References

- [Qwen3-TTS Server](https://github.com/ValyrianTech/Qwen3-TTS_server)
- [Qwen3-TTS Official](https://github.com/QwenLM/Qwen3-TTS)
- [Qwen3-TTS Research](https://qwen.ai/blog/qwen3-tts/)
