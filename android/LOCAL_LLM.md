# Android local LLM

This slice adds an **experimental phone-local model backend** for the Android standalone runtime. It advances #622 without choosing the final embedded inference framework before the required real-device bakeoff.

## Authority boundary

The runtime order is deliberate:

```text
user text
  -> local Prolog / expert routing
     -> matched: return deterministic symbolic result
     -> unresolved: optional local model
```

Explicit Prolog queries and expert commands never fall through to the model. The local model receives no tool definitions and cannot directly execute timers, apps, URLs, device actions, or other effects. Those remain owned by Zara's typed symbolic/capability runtime.

## Current backend

The current backend speaks the OpenAI-compatible chat-completions protocol to a server bound to the same Android device. Configuration accepts only loopback origins (`localhost`, `127.0.0.1`, or `::1`), does not follow redirects, and is disabled by default.

The backend provides:

- explicit backend/model/quantization identity;
- bounded prompt and output sizes;
- serialized generation on a dedicated actor thread;
- streaming response consumption;
- deadline and cancellation handling;
- generation fencing when configuration changes;
- explicit disabled/unavailable/unsupported/OOM/cancelled/timeout/invalid-response failures;
- no automatic cloud fallback.

Android cleartext traffic remains disabled globally. HTTP is allowed only for the loopback server so a Termux `llama-server` can be used without weakening unrelated network traffic.

## Termux quick start

Put a reviewed GGUF model on the phone, then run:

```sh
bash scripts/android-local-llm-termux.sh /path/to/model.gguf 8080
```

The launcher tries the Termux `llama-cpp` package first and can build upstream `ggml-org/llama.cpp` locally when the package is unavailable. It binds `llama-server` to `127.0.0.1` only.

Then configure Zara from local chat:

```text
/model use http://127.0.0.1:8080 local-model Q4_K_M
/model status
```

Disable or cancel it with:

```text
/model off
/model cancel
```

No model is downloaded or bundled by Zara in this slice. Model licensing, provenance, size and quantization remain explicit user choices.

## Phone model sizing

Start the hardware bakeoff with instruction-tuned GGUF models in roughly the sub-1B to 1.5B class at a reviewed 4-bit quantization, then test larger models only when the target phone has acceptable latency, memory, battery and thermal behavior. The launcher defaults to a 4096-token context and CPU-safe `-ngl 0`; both are overrideable through environment variables.

Do not infer production suitability from desktop benchmarks. #622 still requires representative Android measurements for cold/warm load, time to first token, tokens/sec, RSS/peak memory, cancellation, repeated-turn thermals, battery use and lifecycle stability before a model/runtime becomes the built-in default.

## Next production slice

The loopback contract is intentionally backend-neutral enough to survive the bakeoff. A future embedded backend can implement the same bounded `LocalModelBackend` contract while loading an app-private model directly. The choice among llama.cpp JNI/native embedding, ExecuTorch, MLC, ONNX Runtime or another maintained Android runtime remains a measured decision rather than a dependency picked by name recognition.
