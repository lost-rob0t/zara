# Zara Bench and bounded evolution

`zara-bench` is a direct-provider benchmark and evolutionary tuning harness.
It does **not** require `zara-server`; provider isolation and tuning can keep
running when the Zara server is unavailable.

The default live target is the reproducible OpenRouter model
`z-ai/glm-5.3-flash`, with ten Pykka rollout actors. Override the model when
comparing another GLM release.

## OpenRouter GLM smoke

```sh
export OPENROUTER_API_KEY='...'
zara-bench --provider openrouter --model z-ai/glm-5.3-flash --workers 10 \
  bench benchmarks/zara-glm-smoke.jsonl
```

The live convenience gate is `scripts/test-openrouter-glm-bench-live.sh`.
Live provider calls are intentionally separate from deterministic CI.

## llm.starintel.actor through normal Zara config

The StarIntel gateway can be used when it exposes an OpenAI-compatible
`/v1` surface. Keep the provider protocol explicit and point Zara at the
gateway:

```toml
[llm]
provider = "openrouter"
model = "z-ai/glm-5.3-flash"
endpoint = "https://llm.starintel.actor/v1"
api_key = ""
```

Prefer `ZARA_LLM_API_KEY` instead of storing a gateway credential in the
file. Provider-specific `OPENROUTER_API_KEY` still takes precedence.
`zara-bench` normalizes a configured `/v1` base to
`/v1/chat/completions` for the low-level client.

## Evolution

A benchmark JSONL corpus must contain `train` and `heldout` cases.
Supported mutable resource classes are `prompt`, `prolog-skill`, and `kb`.

```sh
zara-bench --config ~/.config/zarathushtra/config.toml --workers 10 \
  evolve benchmarks/zara-glm-smoke.jsonl \
  --resource skills/example/SKILL.md \
  --kind prompt \
  --generations 3 \
  --output-dir .zara/bench/evolution
```

Each generation keeps the candidate text, per-case results, lineage,
held-out score, policy failures, and latency. Rejected generations do not
become the parent of later generations. Prolog-skill and KB candidates must
pass an SWI-Prolog syntax-only parse before evaluation or promotion.

Promotion is opt-in:

```sh
zara-bench ... evolve ... --promote path/to/generated-resource
```

Promotion fails closed unless a candidate has zero policy failures and
strictly improves held-out score. The optimizer prompt forbids edits to
permission policy, secrets, evaluator definitions, and benchmark fixtures.
The harness generates versioned candidate artifacts; it does not silently
rewrite user-authored source.
