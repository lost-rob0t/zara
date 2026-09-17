# Android cloud model providers

Zara Android keeps symbolic Prolog resolution authoritative. Cloud models are an optional conversational/coding layer and never execute Android side effects directly.

## Provider settings

Open the **Model Providers** launcher surface. API keys are encrypted with an Android Keystore-backed AES-GCM key and stored separately from provider metadata and Prolog sources.

Supported profiles:

- **Generic OpenAI-compatible** — any HTTPS base URL implementing `POST <base>/chat/completions`.
- **StarIntel** — preset for `https://llm.starintel.actor/v1`; this is the same generic OpenAI-compatible transport.
- **StatIntel** — preset for `https://llm.statintel.actor/v1`; this also uses the generic OpenAI-compatible transport.
- **OpenRouter** — `https://openrouter.ai/api/v1`; Zara sends the configured/effective app identity as `X-Title`.
- **Z.AI Coding Plan** — `https://api.z.ai/api/coding/paas/v4`; hard-gated to explicit coding requests.

The StarIntel/StatIntel presets are protocol configuration only: the endpoint must actually expose an OpenAI-compatible `chat/completions` API and accept the configured model/API key.

The Z.AI Coding Plan endpoint is intentionally not interchangeable with Z.AI's general prepaid endpoint. See the current ZCode endpoint documentation and usage policy before changing that gate:

- https://zcode.z.ai/en/docs/configuration
- https://zcode.z.ai/en/docs/qa

OpenRouter's OpenAI-compatible API documentation is at https://openrouter.ai/developers.

## OpenRouter provider policy

Cloud-model metadata is stored as schema version `1`. The OpenRouter profile carries a typed provider-routing policy alongside the exact selected model. This follows the same separation used by `llm.starintel.actor`: model/provider policy is explicit configuration and the transport remains a bounded execution adapter.

Current safe defaults are:

- provider sorting: `price`;
- provider fallback: enabled **only among endpoints serving the same exact model**;
- accepted quantizations: `fp16`, `bf16`, `fp8`;
- provider data collection: `deny`;
- require request-parameter support: enabled;
- Zero Data Retention requirement: disabled unless explicitly requested.

The policy also supports explicit provider `order`, `only`, and `ignore` lists plus prompt/completion price ceilings in USD per million tokens. Invalid provider slugs, duplicate/unknown quantization entries, conflicting allow/block lists, and invalid price ceilings fail validation.

Zara deliberately does **not** emit OpenRouter's cross-model `models` fallback array. A configured model ID therefore remains the model identity for the request; provider failover does not authorize switching to a different paid model.

The `provider` object is emitted only for the OpenRouter profile. A generic OpenAI-compatible endpoint such as `llm.starintel.actor` receives the ordinary OpenAI-compatible request and remains responsible for its own server-side provider routing, budget policy, and fallback rules.

## Commands

Provider metadata can be changed from chat without exposing secrets:

```text
/provider status
/provider use starintel <model>
/provider use openrouter <model>
/provider use zai <model>
/provider use openai <https-base-url> <model>
/provider app <fallback LLM app name>
/provider on
/provider off
/provider cancel
```

For the exact StatIntel host from the Android provider picker, the generic command equivalent is:

```text
/provider use openai https://llm.statintel.actor/v1 <model>
```

API keys are never accepted through chat commands. Store or clear them only from **Model Providers**.

A Z.AI Coding Plan profile is invoked explicitly:

```text
/code implement a bounded Prolog parser for complication templates
```

Normal unresolved assistant turns do **not** route to a coding-only provider.

## Prolog identity

Android reads data-only identity facts from the private Prolog workspace:

```prolog
project_name("Mara").
llm_app_name("Mara Android").
```

`project_name/1` changes the default project identity and generated wake-word vocabulary. `llm_app_name/1` may override the name presented to local/cloud LLMs. Last matching fact wins in deterministic workspace-file order.

Identity precedence for model calls is:

1. explicit `llm_app_name/1`
2. explicit `project_name/1`
3. provider-settings fallback app name
4. `Zara`

For a renamed project with no explicit wake-word facts, defaults become `hey <project>` and `<project>`. The `Zara` identity preserves the legacy aliases for compatibility.

## Routing

The unresolved-turn order is:

1. Prolog symbolic runtime
2. local loopback LLM, when enabled
3. general cloud model, when enabled

If the local model is enabled and fails, Zara reports that failure instead of silently spending cloud tokens. A Z.AI Coding Plan profile is never eligible for step 3.
