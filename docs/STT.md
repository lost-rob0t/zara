# Speech-to-text backends

Zara keeps one microphone/VAD/command pipeline and allows the transcription
engine behind it to be selected independently.

## Providers

| Provider | Local | Model argument | Notes |
| --- | --- | --- | --- |
| `faster-whisper` | yes | Whisper model name/path | Default CTranslate2 backend |
| `whisper` / `openai-whisper` | yes | OpenAI Whisper model name | Reference OpenAI Whisper implementation |
| `moonshine` | yes | sherpa-onnx Moonshine model directory | Low-latency local candidate |
| `zipformer` | yes | sherpa-onnx transducer model directory | Fast ONNX transducer candidate |
| `sense-voice` | yes | sherpa-onnx SenseVoice model directory | Multilingual ONNX model |
| `sherpa-onnx` | yes | supported sherpa model directory | Auto-detect Moonshine/Zipformer/SenseVoice |
| `groq` | no | Groq transcription model | Requires `GROQ_API_KEY` |
| `openai` | no | OpenAI transcription model | Requires `OPENAI_API_KEY` |

The existing Silero VAD remains the utterance boundary for wake mode. Local
sherpa models are not downloaded implicitly: point `--model` at a model
directory whose files came from the sherpa-onnx model releases.

## Examples

Default faster-whisper:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider faster-whisper \
  --model small
```

The historical `--mode` spelling is retained as an explicit alias for
`--model`, so this also works:

```sh
nix run .#zara -- --wake --pets --mode tiny
```

Reference OpenAI Whisper locally:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider whisper \
  --model tiny.en
```

Moonshine through sherpa-onnx:

```sh
nix run .#zara -- --wake --pets \
  --stt-provider moonshine \
  --model "$HOME/.local/share/zara/models/moonshine"
```

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

When a remote provider is selected with a legacy local Whisper size such as
`tiny`, `base`, or `small`, Zara maps it to that provider's low-latency default
instead of sending an invalid model name.

## Configuration

The same values can be persisted in Zara's TOML config:

```toml
[stt]
provider = "moonshine"
model = "/home/me/.local/share/zara/models/moonshine"
device = "cpu"
threads = 4
```

For `faster-whisper`, named models retain Zara's observable Hugging Face cache
and download path. Other providers never enter the faster-whisper downloader.

## Whisper quality fix

Wake mode uses Silero VAD to segment speech. The final faster-whisper decode no
longer runs a second VAD pass over that already-segmented utterance. Final
commands use a quality beam while passive wake detection uses a lighter beam,
previous-text conditioning is disabled between independent turns, and captured
PCM is sanitized before transcription.

This addresses the failure mode where valid speech was segmented by Silero but
a second VAD/poorly conditioned decode produced a badly corrupted command.
