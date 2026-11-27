% Android cloud-model providers and chat routing — verified contracts.
% Provenance: session run e554906d916a4c7d80ecc3c1ec1fbfb7; feature commit
% 323f34c0 (GLM coding-plan provider with explicit per-profile purpose).
% Evidence: RED 'No parameter with name purpose' -> GREEN :app full suite.

% Providers are an enum in CloudModelContract.kt with validated endpoints.
% Z.AI coding plan is coding-only: validated() fails closed unless the
% profile purpose is coding.
cloud_provider(openai_compatible, "https://llm.starintel.actor/v1", any_purpose).
cloud_provider(openrouter, "https://openrouter.ai/api/v1", any_purpose).
cloud_provider(z_ai_coding_plan, "https://api.z.ai/api/coding/paas/v4", coding_only).
provider_wire_name(z_ai_coding_plan, "z-ai-coding-plan").
provider_preset_default_model(z_ai_coding_plan, "glm-4.6").

% Every CloudModelConfig carries an explicit purpose (general/coding)
% persisted in cloud.properties as purpose=<lowercase>. Direct-provider
% chat turns MUST use state.config.purpose; hard-coding GENERAL broke
% coding-plan profiles (regression fenced by RemoteModelRoutingContractTest).
config_purpose_field(explicit_per_profile).
config_purpose_persistence(properties_lowercase).
chat_turn_purpose_source(state_config_purpose).

% OpenRouter supports a full provider policy (sort, quantizations fp16/
% bf16/fp8, data_collection, zdr, require_parameters, order/only/ignore,
% max_price) validated fail-closed; endpoint must be an official base URL.
openrouter_policy_modeled(fail_closed).

% Chat execution targets (ChatExecutionTarget): local, direct_provider,
% zara_server. RuntimeMode: auto/symbolic/local/remote. Symbolic and
% Local are network-closed; Auto never silently routes to cloud
% (asserted by RemoteModelRoutingContractTest against the source text).
execution_target(local).
execution_target(direct_provider).
execution_target(zara_server).
routing_fence(symbolic_local_network_closed).
routing_fence(auto_never_silent_cloud).

% Remote provider secrets live in Android Keystore (CloudApiKeyStore);
% never in resources, BuildConfig, logs, diagnostics, or the settings UI
% (contract-tested).
api_key_storage(android_keystore_wrapped).

% Local AI stack: models are .litertlm files imported via SAF into
% app-private storage with mandatory SHA-256 verification
% (LocalModelStore); generation runs in-process (LiteRT) or through the
% ai.zara.llmserve companion APK, which exposes an Ollama-compatible
% loopback server to other local apps. Local mode never falls back to a
% network model.
local_model_format(litertlm_sha256_verified).
local_llm_companion('ai.zara.llmserve', ollama_loopback).
local_fallback_rule(never_network).

% APK fleet: 14 application modules; android-latest.yml AND release.yml
% both publish every app separately with per-app manifest + signer check.
% wear-app intentionally shares applicationId ai.zara.app with the phone
% app (companion pairing). Fleet module facts live in org_fleet_delivery.
fleet_apk_count(14).
wear_application_id_shared_with_phone(intentional).
