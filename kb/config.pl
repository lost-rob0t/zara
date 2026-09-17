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

        org_browser_setting/2,
        org_browser_effective_setting/2,
        set_org_browser_setting/2,
        clear_org_browser_setting/1,
        org_browser_root/1,
        add_org_browser_root/1,
        clear_org_browser_roots/0,
        org_browser_heading_scale/2,
        org_browser_effective_heading_scale/2,
        set_org_browser_heading_scale/2,
        clear_org_browser_heading_scales/0,
        org_browser_help_source/1,
        org_browser_default_help_source/1,
        org_browser_effective_help_source/1,
        add_org_browser_help_source/1,
        clear_org_browser_help_sources/0,
        reset_org_browser_config/0
    ]).

:- use_module(library(error)).

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
:- dynamic org_browser_setting/2.
:- dynamic org_browser_root/1.
:- dynamic org_browser_heading_scale/2.
:- dynamic org_browser_help_source/1.

% ============================================================
% ZARATHUSTRA DEFAULT CONFIGURATION
% ============================================================
% This configuration provides sensible defaults that work across
% most Linux distributions. Provisioned/base overrides may live in
% ~/.config/zarathushtra/config.pl. Mutable/private operator overrides belong
% in ~/.config/zarathushtra/config.local.pl, which is loaded after config.pl.

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
% Phrases that activate the Python wake listener. Matching tolerates small
% transcription errors (edit distance ~25% of the phrase length), so close
% variants such as "Zaratustra" still trigger. Override or add in
% ~/.config/zarathushtra/config.local.pl, e.g.: wake_word("jarvis").
wake_word("zarathushtra").
wake_word("zarathustra").
wake_word("hey zara").
wake_word("zara").
wake_word("sarah").
wake_word("sara").

% ---- Org browser defaults and executable Prolog configuration ----
% org_browser_setting/2, org_browser_root/1, org_browser_heading_scale/2 and
% org_browser_help_source/1 hold runtime overrides only. This distinction lets
% Python merge built-ins -> TOML -> Prolog without defaults accidentally
% overriding explicit TOML values. Trusted hooks.pl may call the setter API.

org_browser_default_setting(enabled, true).
org_browser_default_setting(memory_sync, true).
org_browser_default_setting(base_font_pt, 12.0).
org_browser_default_setting(max_files, 2000).
org_browser_default_setting(max_file_bytes, 2000000).
org_browser_default_setting(search_limit, 200).
org_browser_default_setting(show_backlinks, true).
org_browser_default_setting(show_properties, true).
org_browser_default_setting(recent_chat_limit, 5).
org_browser_default_setting(python_config_enabled, true).
org_browser_default_setting(default_project, "").

org_browser_default_heading_scale(1, 1.45).
org_browser_default_heading_scale(2, 1.30).
org_browser_default_heading_scale(3, 1.18).
org_browser_default_heading_scale(4, 1.10).
org_browser_default_heading_scale(5, 1.04).

org_browser_default_help_source("README.org").
org_browser_default_help_source("docs/README.org").
org_browser_default_help_source("wiki/android.org").
org_browser_default_help_source("wiki/customization.org").
org_browser_default_help_source("wiki/agent-mode.org").

org_browser_effective_setting(Key, Value) :-
    org_browser_setting(Key, Value),
    !.
org_browser_effective_setting(Key, Value) :-
    org_browser_default_setting(Key, Value).

set_org_browser_setting(Key, Value) :-
    validate_org_browser_setting(Key, Value),
    with_mutex(org_browser_config,
        ( retractall(org_browser_setting(Key, _)),
          assertz(org_browser_setting(Key, Value))
        )).

clear_org_browser_setting(Key) :-
    must_be(atom, Key),
    with_mutex(org_browser_config,
        retractall(org_browser_setting(Key, _))).

add_org_browser_root(Path) :-
    validate_org_browser_text(Path, root),
    with_mutex(org_browser_config,
        ( org_browser_root(Path)
        -> true
        ; assertz(org_browser_root(Path))
        )).

clear_org_browser_roots :-
    with_mutex(org_browser_config,
        retractall(org_browser_root(_))).

org_browser_effective_heading_scale(Level, Scale) :-
    org_browser_heading_scale(Level, Scale),
    !.
org_browser_effective_heading_scale(Level, Scale) :-
    org_browser_default_heading_scale(Level, Scale).

set_org_browser_heading_scale(Level, Scale) :-
    must_be(integer, Level),
    ( between(1, 32, Level),
      number(Scale), Scale >= 0.25, Scale =< 4.0
    -> true
    ; throw(error(domain_error(org_browser_heading_scale, Level-Scale), _))
    ),
    with_mutex(org_browser_config,
        ( retractall(org_browser_heading_scale(Level, _)),
          assertz(org_browser_heading_scale(Level, Scale))
        )).

clear_org_browser_heading_scales :-
    with_mutex(org_browser_config,
        retractall(org_browser_heading_scale(_, _))).

org_browser_effective_help_source(Path) :-
    org_browser_help_source(_),
    !,
    org_browser_help_source(Path).
org_browser_effective_help_source(Path) :-
    org_browser_default_help_source(Path).

add_org_browser_help_source(Path) :-
    validate_org_browser_text(Path, help_source),
    with_mutex(org_browser_config,
        ( org_browser_help_source(Path)
        -> true
        ; assertz(org_browser_help_source(Path))
        )).

clear_org_browser_help_sources :-
    with_mutex(org_browser_config,
        retractall(org_browser_help_source(_))).

reset_org_browser_config :-
    with_mutex(org_browser_config,
        ( retractall(org_browser_setting(_, _)),
          retractall(org_browser_root(_)),
          retractall(org_browser_heading_scale(_, _)),
          retractall(org_browser_help_source(_))
        )).

validate_org_browser_setting(enabled, Value) :- !,
    validate_org_browser_boolean(Value).
validate_org_browser_setting(memory_sync, Value) :- !,
    validate_org_browser_boolean(Value).
validate_org_browser_setting(show_backlinks, Value) :- !,
    validate_org_browser_boolean(Value).
validate_org_browser_setting(show_properties, Value) :- !,
    validate_org_browser_boolean(Value).
validate_org_browser_setting(python_config_enabled, Value) :- !,
    validate_org_browser_boolean(Value).
validate_org_browser_setting(default_project, Value) :- !,
    ( atom(Value) ; string(Value) ).
validate_org_browser_setting(base_font_pt, Value) :- !,
    validate_org_browser_number(Value, 1.0, 96.0).
validate_org_browser_setting(max_files, Value) :- !,
    validate_org_browser_integer(Value, 1, 100000).
validate_org_browser_setting(max_file_bytes, Value) :- !,
    validate_org_browser_integer(Value, 1, 100000000).
validate_org_browser_setting(search_limit, Value) :- !,
    validate_org_browser_integer(Value, 1, 10000).
validate_org_browser_setting(recent_chat_limit, Value) :- !,
    validate_org_browser_integer(Value, 0, 100).
validate_org_browser_setting(Key, Value) :-
    throw(error(domain_error(org_browser_setting, Key-Value), _)).

validate_org_browser_boolean(Value) :-
    ( memberchk(Value, [true, false])
    -> true
    ; throw(error(domain_error(org_browser_setting, Value), _))
    ).

validate_org_browser_number(Value, Minimum, Maximum) :-
    ( number(Value), Value >= Minimum, Value =< Maximum
    -> true
    ; throw(error(domain_error(org_browser_setting, Value), _))
    ).

validate_org_browser_integer(Value, Minimum, Maximum) :-
    ( integer(Value), between(Minimum, Maximum, Value)
    -> true
    ; throw(error(domain_error(org_browser_setting, Value), _))
    ).

validate_org_browser_text(Value, Kind) :-
    ( (atom(Value) ; string(Value)),
      term_string(Value, Text, [quoted(false)]),
      Text \= ""
    -> true
    ; throw(error(domain_error(org_browser_setting, Kind-Value), _))
    ).

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
