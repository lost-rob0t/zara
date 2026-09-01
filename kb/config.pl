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
        wake_word/1,                % wake phrase accepted by the listener

        llm_provider/1,             % anthropic | openai | openrouter | ollama
        llm_model/1,                % model name/ID
        llm_endpoint/1,             % API endpoint URL

        prolog_rlm_enabled/1,       % route LLM command rewrites through Prolog-RLM direct mode
        prolog_rlm_model/1          % OpenRouter model id used by direct-mode rewrites
    ]).

:- discontiguous kb_config:todo_destination/1.
:- discontiguous kb_config:todo_template/2.
:- discontiguous kb_config:llm_provider/1.
:- discontiguous kb_config:llm_model/1.
:- discontiguous kb_config:llm_endpoint/1.
:- discontiguous kb_config:wake_word/1.
:- dynamic todo_destination/1.
:- dynamic todo_destination_md/1.
:- dynamic todo_context_mode/1.
:- dynamic search_engine/1.
:- dynamic wake_word/1.
:- dynamic llm_provider/1.
:- dynamic llm_model/1.
:- dynamic llm_endpoint/1.
:- dynamic prolog_rlm_enabled/1.
:- dynamic prolog_rlm_model/1.

% ============================================================
% ZARATHUSTRA DEFAULT CONFIGURATION
% ============================================================
% This configuration provides sensible defaults that work across
% most Linux distributions. Users can override these by creating
% ~/.zarathushtra/config.pl with their own definitions.

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
% answers with the resolved URL. Users can override this in
% ~/.zarathushtra/config.pl
search_engine("https://duckduckgo.com/?q=~w").

% ---- Wake Words ----
% Phrases that activate the Python wake listener. Matching tolerates small
% transcription errors (edit distance ~25% of the phrase length), so close
% variants such as "Zaratustra" still trigger. Override or add in
% ~/.zarathushtra/config.pl, e.g.: wake_word("jarvis").
wake_word("zarathushtra").
wake_word("zarathustra").
wake_word("hey zara").
wake_word("zara").
wake_word("sarah").
wake_word("sara").

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

% ---- Prolog-RLM Direct-Mode Rewrites ----
%
% When enabled, Prolog command fallback rewrites are routed through the
% pinned Prolog-RLM direct runtime (rlm_direct/4) over OpenRouter instead
% of the plain llm_client query path. Requires ZARA_PROLOG_RLM_ROOT to
% point at a Prolog-RLM checkout and OPENROUTER_API_KEY to be set.
prolog_rlm_enabled(false).
prolog_rlm_model("openrouter/free").
