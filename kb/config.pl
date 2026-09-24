% ======================================================================
% FILE: kb/config.pl
% ======================================================================
% Shared semantic configuration (issue #158 split).
%
% Everything here is safe to consult on any host, including a headless
% server: settings and data templates only, never shell commands. Linux
% device provider configuration (app mappings, dictation, sounds) lives in
% kb/device_providers.pl and is consulted only by the desktop boot.
% Server provider configuration lives in kb/server_providers.pl.

:- module(kb_config,
    [
        todo_destination/1,
        todo_destination_md/1,
        todo_context_mode/1,
        todo_format/1,              % org | markdown
        todo_template/2,            % todo_template(Format, TemplateString)

        search_engine/1,
        project_name/1,             % product/project identity; first clause wins
        wake_word/1,                % explicit wake phrase; otherwise derived
        llm_app_name/1,             % app identity presented to LLM providers

        llm_provider/1,             % anthropic | openai | openrouter | ollama
        llm_model/1,                % model name/ID
        llm_endpoint/1,             % API endpoint URL

        commerce_provider/1,        % default commerce provider
        commerce_confirmation/1,    % always
        preference_learning/1,      % enabled | disabled
        preference_min_observations/1,
        preference_max_patterns/1,
        preference_min_confidence/1
    ]).

:- discontiguous kb_config:todo_destination/1.
:- discontiguous kb_config:todo_template/2.
:- discontiguous kb_config:project_name/1.
:- discontiguous kb_config:wake_word/1.
:- discontiguous kb_config:llm_app_name/1.
:- discontiguous kb_config:llm_provider/1.
:- discontiguous kb_config:llm_model/1.
:- discontiguous kb_config:llm_endpoint/1.
:- dynamic todo_destination/1.
:- dynamic todo_destination_md/1.
:- dynamic todo_context_mode/1.
:- dynamic search_engine/1.
:- dynamic project_name/1.
:- dynamic wake_word/1.
:- dynamic llm_app_name/1.
:- dynamic llm_provider/1.
:- dynamic llm_model/1.
:- dynamic llm_endpoint/1.
:- dynamic commerce_provider/1.
:- dynamic commerce_confirmation/1.
:- dynamic preference_learning/1.
:- dynamic preference_min_observations/1.
:- dynamic preference_max_patterns/1.
:- dynamic preference_min_confidence/1.

% ============================================================
% ZARATHUSTRA DEFAULT CONFIGURATION
% ============================================================
% This configuration provides sensible defaults that work across
% most Linux distributions. Provisioned/base overrides may live in
% ~/.config/zarathushtra/config.pl. Mutable/private operator overrides belong
% in ~/.config/zarathushtra/config.local.pl, which is loaded after config.pl.
% The config loader installs user facts with asserta/2, so the effective
% scalar identity is the first matching clause: local overlay, provisioned
% base, then the packaged fallback below.

% ---- Project Identity ----
%
% Renaming the project changes generated default wake words. With no explicit
% wake_word/1 facts, project_name("Mara") yields "hey mara" and "mara".
% Zara keeps its legacy recognition aliases for backwards compatibility.
project_name("Zara").

% Name sent to LLM providers as the application identity (for example,
% OpenRouter's X-Title header and model system identity). It may differ from
% project_name/1 when a deployment wants a more specific client label.
llm_app_name("Zara").

% ---- TODO Settings ----
% Where to store TODO entries (Org-mode format)
todo_destination("~/todo.org").

% Optional markdown destination (used when todo_format(markdown)).
% If missing, markdown falls back to todo_destination/1.
% todo_destination_md("~/todo.md").

% Context inference mode for TODO categorization
% Options: infer | infer_with_llm | llm_only
todo_context_mode(infer).

% Output format for todo capture templates
% Options: org | markdown
todo_format(org).

% ---- TODO Template System ----
% Placeholders you can use:
%   {task} {tag} {category} {created}
%   {scheduled}        -> e.g. "2026-01-30 15:00" or ""
%   {scheduled_org}    -> "<2026-01-30 Tue 15:00>" or ""
%   {scheduled_line}   -> org helper: "SCHEDULED: <...>\n" or ""
%   {due_suffix}       -> markdown helper: " (due: 2026-01-30 15:00)" or ""
%   {cursor}           -> marker string: "%%"

todo_template(org,
"* TODO {task} :{tag}:
:PROPERTIES:
:CREATED:  {created}
:CATEGORY: {category}
:END:
{scheduled_line}{cursor}

").

todo_template(markdown,
"- [ ] {task}{due_suffix}  <!-- tag:{tag} cat:{category} created:{created} -->
  - {cursor}

").


% Search engine template for the `search` intent.
% Data only: the device side opens the browser with it; the server side
% answers with the resolved URL. Mutable/private overrides belong in
% ~/.config/zarathushtra/config.local.pl.
search_engine("https://search.brave.com/search?q=~w").

% ---- Wake Words ----
% Explicit wake_word/1 facts override generated project-name wake words.
% Matching tolerates small transcription errors (edit distance ~25% of the
% phrase length). Example operator overrides:
%
%   wake_word("computer").
%   wake_word("hey computer").
%
% With no explicit facts, wake words are derived from project_name/1.

% ---- Commerce + Preference Learning ----
%
% Commerce is provider-neutral at the policy layer. Provider plugins own
% operational details; the semantic policy here is safe on desktop/server.
commerce_provider(doordash).

% External purchases always require Zara Core's canonical interactive approval.
% There is intentionally no "never confirm" mode.
commerce_confirmation(always).

% The Mega Brain can persist bounded low-authority observations and derive
% deterministic preference patterns. These facts never grant action authority.
preference_learning(enabled).
preference_min_observations(2).
preference_max_patterns(10).
preference_min_confidence(0.5).

% ---- LLM Provider Configuration ----

% Used by Python wake listener for conversational queries.
% Options: anthropic | openai | openrouter | ollama
llm_provider(ollama).

% Model name (provider-specific)
% Ollama: llama3.2, mistral, neural-chat, etc.
% OpenAI: gpt-4o-mini, gpt-4, gpt-4-turbo
% Anthropic: claude-sonnet-4-20250514, claude-opus-4-5-20251101
llm_model("llama3.2:latest").

% API endpoint (optional, uses provider defaults if not specified)
% Ollama default: http://localhost:11434/api/chat
% OpenAI default: https://api.openai.com/v1/chat/completions
% OpenRouter default: https://openrouter.ai/api/v1/chat/completions
% Anthropic: handled by SDK (don't override)
llm_endpoint("http://localhost:11434/api/chat").
