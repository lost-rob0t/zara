% ======================================================================
% FILE: kb/config.pl
% ======================================================================
% Shared semantic configuration (issue #158 split).

:- module(kb_config,
    [
        todo_destination/1,
        todo_destination_md/1,
        todo_context_mode/1,
        todo_format/1,
        todo_template/2,
        search_engine/1,
        wake_word/1,
        llm_provider/1,
        llm_model/1,
        llm_endpoint/1,
        org_browser_setting/2,
        org_browser_effective_setting/2,
        set_org_browser_setting/2,
        clear_org_browser_setting/1,
        org_browser_root/1,
        org_browser_roots_overridden/0,
        add_org_browser_root/1,
        clear_org_browser_roots/0,
        org_browser_heading_scale/2,
        org_browser_effective_heading_scale/2,
        set_org_browser_heading_scale/2,
        clear_org_browser_heading_scales/0,
        org_browser_help_source/1,
        org_browser_help_sources_overridden/0,
        org_browser_default_help_source/1,
        org_browser_effective_help_source/1,
        add_org_browser_help_source/1,
        clear_org_browser_help_sources/0,
        reset_org_browser_config/0,
        org_browser_snapshot_path/1,
        write_org_browser_snapshot/0
    ]).

:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(http/json)).

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
:- dynamic org_browser_roots_overridden/0.
:- dynamic org_browser_heading_scale/2.
:- dynamic org_browser_help_source/1.
:- dynamic org_browser_help_sources_overridden/0.

% ============================================================
% ZARATHUSTRA DEFAULT CONFIGURATION
% ============================================================

todo_destination("~/todo.org").
todo_context_mode(infer).
todo_format(org).

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

search_engine("https://search.brave.com/search?q=~w").

wake_word("zarathushtra").
wake_word("zarathustra").
wake_word("hey zara").
wake_word("zara").
wake_word("sarah").
wake_word("sara").

% ---- Org browser defaults and executable Prolog configuration ----
% Runtime overrides remain separate from defaults so Python can merge
% defaults -> TOML -> Prolog -> trusted org_browser.py deterministically.
% Every mutation also publishes an owner-local, disposable JSON projection for
% native clients that must not start a second Prolog engine.

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
          assertz(org_browser_setting(Key, Value)),
          write_org_browser_snapshot_unlocked
        )).

clear_org_browser_setting(Key) :-
    must_be(atom, Key),
    with_mutex(org_browser_config,
        ( retractall(org_browser_setting(Key, _)),
          write_org_browser_snapshot_unlocked
        )).

add_org_browser_root(Path) :-
    validate_org_browser_text(Path, root),
    with_mutex(org_browser_config,
        ( ( org_browser_roots_overridden -> true ; assertz(org_browser_roots_overridden) ),
          ( org_browser_root(Path) -> true ; assertz(org_browser_root(Path)) ),
          write_org_browser_snapshot_unlocked
        )).

clear_org_browser_roots :-
    with_mutex(org_browser_config,
        ( retractall(org_browser_root(_)),
          ( org_browser_roots_overridden -> true ; assertz(org_browser_roots_overridden) ),
          write_org_browser_snapshot_unlocked
        )).

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
          assertz(org_browser_heading_scale(Level, Scale)),
          write_org_browser_snapshot_unlocked
        )).

clear_org_browser_heading_scales :-
    with_mutex(org_browser_config,
        ( retractall(org_browser_heading_scale(_, _)),
          write_org_browser_snapshot_unlocked
        )).

org_browser_effective_help_source(Path) :-
    org_browser_help_sources_overridden,
    !,
    org_browser_help_source(Path).
org_browser_effective_help_source(Path) :-
    org_browser_default_help_source(Path).

add_org_browser_help_source(Path) :-
    validate_org_browser_text(Path, help_source),
    with_mutex(org_browser_config,
        ( ( org_browser_help_sources_overridden -> true ; assertz(org_browser_help_sources_overridden) ),
          ( org_browser_help_source(Path) -> true ; assertz(org_browser_help_source(Path)) ),
          write_org_browser_snapshot_unlocked
        )).

clear_org_browser_help_sources :-
    with_mutex(org_browser_config,
        ( retractall(org_browser_help_source(_)),
          ( org_browser_help_sources_overridden -> true ; assertz(org_browser_help_sources_overridden) ),
          write_org_browser_snapshot_unlocked
        )).

reset_org_browser_config :-
    with_mutex(org_browser_config,
        ( retractall(org_browser_setting(_, _)),
          retractall(org_browser_root(_)),
          retractall(org_browser_roots_overridden),
          retractall(org_browser_heading_scale(_, _)),
          retractall(org_browser_help_source(_)),
          retractall(org_browser_help_sources_overridden),
          write_org_browser_snapshot_unlocked
        )).

org_browser_snapshot_path(Path) :-
    getenv('ZARA_ORG_BROWSER_PROLOG_SNAPSHOT', Explicit),
    Explicit \== '',
    !,
    Path = Explicit.
org_browser_snapshot_path(Path) :-
    getenv('XDG_RUNTIME_DIR', RuntimeDir),
    RuntimeDir \== '',
    directory_file_path(RuntimeDir, 'zarathushtra', ZaraDir),
    make_directory_path(ZaraDir),
    directory_file_path(ZaraDir, 'org-browser-prolog.json', Path).

write_org_browser_snapshot :-
    with_mutex(org_browser_config,
        write_org_browser_snapshot_unlocked).

write_org_browser_snapshot_unlocked :-
    ( org_browser_snapshot_path(Path)
    -> org_browser_snapshot_dict(Snapshot),
       format(atom(TempPath), '~w.tmp', [Path]),
       setup_call_cleanup(
           open(TempPath, write, Stream, [encoding(utf8)]),
           ( json_write_dict(Stream, Snapshot, [width(0)]),
             nl(Stream)
           ),
           close(Stream)
       ),
       rename_file(TempPath, Path)
    ; true
    ).

org_browser_snapshot_dict(Snapshot) :-
    findall(Key-Value, org_browser_setting(Key, Value), RawPairs),
    sort(RawPairs, Pairs),
    dict_pairs(Settings, settings, Pairs),
    findall(Path, org_browser_root(Path), Roots),
    findall(_{level:Level, scale:Scale},
            org_browser_heading_scale(Level, Scale),
            HeadingScales),
    findall(Path, org_browser_help_source(Path), HelpSources),
    ( org_browser_roots_overridden -> RootsOverride = true ; RootsOverride = false ),
    ( org_browser_help_sources_overridden -> HelpOverride = true ; HelpOverride = false ),
    Snapshot = _{
        version:1,
        settings:Settings,
        roots_override:RootsOverride,
        roots:Roots,
        heading_scales:HeadingScales,
        help_sources_override:HelpOverride,
        help_sources:HelpSources
    }.

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
llm_provider(ollama).
llm_model("llama3.2:latest").
llm_endpoint("http://localhost:11434/api/chat").
