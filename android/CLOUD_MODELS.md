# Android cloud model providers

Zara Android keeps symbolic Prolog resolution authoritative. Cloud models are an optional conversational/coding layer and never execute Android side effects directly.

## Provider settings

Open the **Model Providers** launcher surface. API keys are encrypted with an Android Keystore-backed AES-GCM key and stored separately from provider metadata and Prolog sources.

Supported profiles:

- **Generic OpenAI-compatible** — any HTTPS base URL implementing `POST <base>/chat/completions`.
- **StarIntel** — preset for `https://llm.starintel.actor/v1`; this is the same generic OpenAI-compatible transport.
- **OpenRouter** — `https://openrouter.ai/api/v1`; Zara sends the configured/effective app identity as `X-Title`.
- **Z.AI Coding Plan** — `https://api.z.ai/api/coding/paas/v4`; hard-gated to explicit coding requests.

The Z.AI Coding Plan endpoint is intentionally not interchangeable with Z.AI's general prepaid endpoint. See the current ZCode endpoint documentation and usage policy before changing that gate:

- https://zcode.z.ai/en/docs/configuration
- https://zcode.z.ai/en/docs/qa

OpenRouter's OpenAI-compatible API documentation is at https://openrouter.ai/developers.

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
