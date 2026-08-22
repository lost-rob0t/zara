# Speech-to-text backends

Zara keeps one microphone/VAD/command pipeline and swaps the transcription engine behind it. That is the useful boundary: changing models should not require cloning the wake loop and teaching a second copy how microphones work.

**Related docs:** [docs index](index.org) · [voice pipeline](voice.org) · [CLI](cli.org) · [configuration](configuration.org) · [literate main config](config/main.org)

## Provider matrix

| Provider | Local | Model argument | Notes |
| --- | --- | --- | --- |
| `faster-whisper` | yes | Whisper model name/path | Default CTranslate2 backend |
| `whisper-cpp` | yes | GGML model file/name resolved by Zara | Persistent local server; Vulkan-capable GPU path |
| `whisper` / `openai-whisper` | yes | OpenAI Whisper model name | Reference OpenAI Whisper implementation |
| `moonshine` | yes | sherpa-onnx Moonshine v1 model directory | Four-file legacy Moonshine export |
| `moonshine-v2` | yes | sherpa-onnx Moonshine v2 model directory | Current merged-decoder export |
| `zipformer` | yes | sherpa-onnx transducer model directory | ONNX transducer |
| `sense-voice` | yes | sherpa-onnx SenseVoice model directory | Multilingual ONNX model |
| `sherpa-onnx` | yes | supported sherpa model directory | Auto-detect Moonshine v1/v2, Zipformer, SenseVoice |
| `groq` | no | Groq transcription model | Requires `GROQ_API_KEY` |
| `openai` | no | OpenAI transcription model | Requires `OPENAI_API_KEY` |

The existing Silero VAD remains the utterance boundary for wake mode. Local sherpa models are not downloaded implicitly: point `--model` at a model directory whose files came from the sherpa-onnx model releases.

Provider aliases such as `faster_whisper`, `whisper_cpp`, `sherpa`, `moonshine_v2`, and `sensevoice` are normalized internally. The CLI help shows the stable user-facing names.

## Fast examples

### faster-whisper

```sh
nix run .#zara -- --wake --pets \
  --stt-provider faster-whisper \
  --model small
```

The historical `--mode` spelling is retained as an alias for `--model`:

```sh
nix run .#zara -- --wake --pets --mode tiny
```

### whisper.cpp

```sh
nix run .#zara -- --wake \
  --stt-provider whisper-cpp \
  --model /path/to/ggml-model.bin \
  --device cpu
```

For GPU transcription Zara's whisper.cpp path uses Vulkan. On an AMD/ROCm machine you can use the explicit spelling:

```sh
nix run .#zara -- --wake \
  --stt-provider whisper-cpp \
  --model /path/to/ggml-model.bin \
  --device vulkan
```

`amd`, `rocm`, and `hip` are accepted convenience spellings and normalize to Vulkan for whisper.cpp. `--device cuda` is deliberately rejected for this backend so a familiar token does not silently select the wrong acceleration API.

The adapter uses a persistent local whisper.cpp server rather than starting a new heavyweight inference process for every utterance.

### Reference OpenAI Whisper

```sh
nix run .#zara -- --wake --pets \
  --stt-provider whisper \
  --model tiny.en
```

`whisper` and `openai-whisper` select the same reference implementation path.

### Moonshine v1

```sh
nix run .#zara -- --wake --pets \
  --stt-provider moonshine \
  --model "$HOME/.local/share/zara/models/moonshine"
```

### Moonshine v2

```sh
nix run .#zara -- --wake --pets \
  --stt-provider moonshine-v2 \
  --model "$HOME/.local/share/zara/models/moonshine-v2"
```

The v2 directory is expected to contain `encoder_model.ort`, `decoder_model_merged.ort`, and `tokens.txt`. The generic `sherpa-onnx` provider auto-detects this layout too.

### Zipformer

```sh
nix run .#zara -- --wake --pets \
  --stt-provider zipformer \
  --model "$HOME/.local/share/zara/models/zipformer"
```

### SenseVoice

```sh
nix run .#zara -- --wake --pets \
  --stt-provider sense-voice \
  --model "$HOME/.local/share/zara/models/sense-voice"
```

### Generic sherpa-onnx auto-detection

```sh
nix run .#zara -- --wake --pets \
  --stt-provider sherpa-onnx \
  --model /path/to/model-directory
```

### Groq transcription

```sh
export GROQ_API_KEY='...'
nix run .#zara -- --wake --pets \
  --stt-provider groq \
  --model whisper-large-v3-turbo
```

### OpenAI transcription

```sh
export OPENAI_API_KEY='...'
nix run .#zara -- --wake --pets \
  --stt-provider openai \
  --model gpt-4o-mini-transcribe
```

When a remote provider is selected with a legacy local Whisper size such as `tiny`, `base`, or `small`, Zara maps it to that provider's low-latency remote default instead of confidently submitting a model name the API does not have.

## Configuration

Persist the same selection under `[stt]`:

```toml
[stt]
provider = "moonshine-v2"
model = "/home/me/.local/share/zara/models/moonshine-v2"
device = "cpu"
threads = 4
```

Or whisper.cpp/Vulkan:

```toml
[stt]
provider = "whisper-cpp"
model = "/home/me/.local/share/zara/models/ggml-model.bin"
device = "vulkan"
threads = 4
```

The full annotated config lives in [`docs/config/main.org`](config/main.org). CLI flags override the corresponding selection for the current invocation.

For `faster-whisper`, named models retain Zara's observable Hugging Face cache/download path. Other providers do not accidentally wander into the faster-whisper downloader merely because they also contain the word “Whisper.”

## Wake-mode VAD stays shared

Speech segmentation is deliberately upstream of the provider adapter. Wake mode uses Silero VAD at 16 kHz and passes an already-bounded utterance into the selected transcription backend.

That means backend experiments are comparable against the same capture behavior, and changing STT does not quietly change pre-roll, trailing silence, no-speech timeouts, or microphone queue policy.

See [`docs/voice.org`](voice.org) for those controls.

## faster-whisper quality path

The final faster-whisper decode no longer runs a second VAD pass over an utterance Silero already segmented. Final commands use a higher-quality beam while passive wake detection uses a lighter beam, previous-text conditioning is disabled between independent turns, and captured PCM is sanitized before transcription.

This fixes the particularly irritating failure mode where speech is captured correctly and then the second segmentation/conditioning layer invents a worse version of it.

## GPU fallback in the unified CLI

Wake mode recognizes GPU-initialization failures for supported local paths and can retry on CPU. It does not turn every runtime exception into a fake “GPU unavailable” diagnosis.

For whisper.cpp, use Vulkan rather than `cuda`. For CTranslate2/faster-whisper-style paths, `cuda` remains the expected accelerator token.

## Remote provider boundaries

OpenAI and Groq adapters send a WAV representation to their OpenAI-compatible `/audio/transcriptions` endpoint. API keys come from `OPENAI_API_KEY` or `GROQ_API_KEY` respectively.

Remote STT is therefore a network/credential boundary. Do not treat it like a local model just because the rest of Zara's VAD pipeline is local.

## What this document does not cover

- Wake state, acknowledgement, TTS and barge-in: [voice.org](voice.org)
- Unified flags and entry points: [cli.org](cli.org)
- Main config ownership/precedence: [configuration.org](configuration.org)
- Install/Nix dependencies and regression commands: [wiki/install.org](../wiki/install.org)

And, because the flag is easy to misread: `zara --voice` one-shot mode is not implemented. Use `--wake` for the assistant or `--dictate` for continuous transcription.
