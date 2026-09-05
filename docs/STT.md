# Speech-to-text backends

## Scope

This page documents Zara's transcription providers and the local/wake-mode STT pipeline. It is **not** the complete daemon voice architecture.

In daemon/client voice mode, the client device owns microphone capture and speaker playback while `zara-server` owns the accepted VAD/STT/routing/turn/TTS runtime work. Audio and transcript events are correlated with canonical Zara conversation/turn/stream ids over `ZARA/1`. See [`server.org`](server.org) for that boundary.

Within a Zara runtime, transcription remains provider-selectable rather than requiring a separate voice implementation per provider.

## Providers

| Provider | Local | Model argument | Notes |
| --- | --- | --- | --- |
| `faster-whisper` | yes | Whisper model name/path | Default CTranslate2 backend |
| `whisper` / `openai-whisper` | yes | OpenAI Whisper model name | Reference OpenAI Whisper implementation |
| `moonshine` | yes | sherpa-onnx Moonshine v1 model directory | Four-file legacy Moonshine export |
| `moonshine-v2` | yes | sherpa-onnx Moonshine v2 model directory | Current two-file/merged-decoder export |
| `zipformer` | yes | sherpa-onnx transducer model directory | Fast ONNX transducer candidate |
| `sense-voice` | yes | sherpa-onnx SenseVoice model directory | Multilingual ONNX model |
| `sherpa-onnx` | yes | supported sherpa model directory | Auto-detect Moonshine v1/v2, Zipformer, SenseVoice |
| `groq` | no | Groq transcription model | Requires `GROQ_API_KEY` |
| `openai` | no | OpenAI transcription model | Requires `OPENAI_API_KEY` |

The existing Silero VAD remains the utterance boundary for local wake mode. Local sherpa models are not downloaded implicitly: point `--model` at a model directory whose files came from the sherpa-onnx model releases.

Remote STT providers send accepted audio/transcription requests to the configured external service. Local providers keep model execution local.

## Examples

Default faster-whisper:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider faster-whisper \
  --model small
```

The historical `--mode` spelling is retained as an explicit alias for `--model`, so this also works:

```sh
nix run .#zara -- --wake --pets --mode tiny
```

Reference OpenAI Whisper locally:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider whisper \
  --model tiny.en
```

Moonshine v1 through sherpa-onnx:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider moonshine \
  --model "$HOME/.local/share/zara/models/moonshine"
```

Moonshine v2 through sherpa-onnx:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider moonshine-v2 \
  --model "$HOME/.local/share/zara/models/moonshine-v2"
```

The v2 directory is expected to contain `encoder_model.ort`, `decoder_model_merged.ort`, and `tokens.txt`. The generic `sherpa-onnx` provider auto-detects this layout too.

Zipformer transducer through sherpa-onnx:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider zipformer \
  --model "$HOME/.local/share/zara/models/zipformer"
```

SenseVoice through sherpa-onnx:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider sense-voice \
  --model "$HOME/.local/share/zara/models/sense-voice"
```

Auto-detect a supported sherpa-onnx directory:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider sherpa-onnx \
  --model /path/to/model-directory
```

Groq transcription:

```sh
export GROQ_API_KEY='...'
nix run .#zara -- --wake --pets \
  --stt-provider groq \
  --model whisper-large-v3-turbo
```

OpenAI transcription:

```sh
export OPENAI_API_KEY='...'
nix run .#zara -- --wake --pets \
  --stt-provider openai \
  --model gpt-4o-mini-transcribe
```

When a remote provider is selected with a legacy local Whisper size such as `tiny`, `base`, or `small`, Zara maps it to that provider's low-latency default instead of sending an invalid model name.

## Configuration

The same values can be persisted in Zara's TOML config:

```toml
[stt]
provider = "moonshine-v2"
model = "/home/me/.local/share/zara/models/moonshine-v2"
device = "cpu"
threads = 4
```

For `faster-whisper`, named models retain Zara's observable Hugging Face cache and download path. Other providers never enter the faster-whisper downloader.

## Wake-mode utterance handling

Wake mode uses Silero VAD to segment speech. The final faster-whisper decode no longer runs a second VAD pass over that already-segmented utterance. Final commands use a quality beam while passive wake detection uses a lighter beam, previous-text conditioning is disabled between independent turns, and captured PCM is sanitized before transcription.

This addresses the failure mode where valid speech was segmented by Silero but a second VAD or poorly conditioned decode produced a badly corrupted command.

## Runtime and conversation integration

STT output should become canonical runtime events rather than UI-only strings.

For live assistant use:

- partial/final transcript events should retain correlation to the active voice stream;
- accepted final text should enter the same turn/conversation model as typed input;
- cancellation/barge-in must invalidate stale output through canonical turn ids;
- raw audio must not be copied into normal logs, audit records, or conversation text storage;
- desktop and Android should observe durable conversation metadata rather than creating separate voice-only histories.

The design-ready unified Copilot work also moves voice-session conversation ownership and automatic title/summary generation to the durable runtime/conversation boundary instead of Qt widgets.
