:- module(config_loader,
    [
        load_user_config/0,
        reload_user_config/0,
        ensure_user_config/0,
        user_config_path/1,
        search_url/2
    ]).

:- use_module(library(filesex)).

:- dynamic loaded_clause/1.

%% user_config_path(-Path) is det.
%
%  Resolves ~/.config/zarathushtra/config.pl to absolute path
user_config_path(Path) :-
    user_config_dir(Dir),
    directory_file_path(Dir, 'config.pl', Path).

%% user_config_dir(-Dir) is det.
%
%  Gets the user config directory
user_config_dir(Dir) :-
    ( getenv('XDG_CONFIG_HOME', Xdg), Xdg \= ''
    -> directory_file_path(Xdg, 'zarathushtra', Dir)
    ; expand_file_name('~/.config/zarathushtra', [Dir])
    ).

%% ensure_user_config is det.
%
%  Creates user config directory and file from template if they don't exist
ensure_user_config :-
    user_config_dir(Dir),
    user_config_path(Path),

    % Create directory if needed
    ( exists_directory(Dir)
    -> true
    ; make_directory_path(Dir),
      format('Created config directory: ~w~n', [Dir])
    ),

    % Create config file from template if needed
    ( exists_file(Path)
    -> true
    ; create_default_config(Path),
      format('Created default config: ~w~n', [Path])
    ).

%% create_default_config(+Path) is det.
%
%  Writes a default user config file with examples
create_default_config(Path) :-
    open(Path, write, Stream),
    write_default_config(Stream),
    close(Stream).

write_default_config(Stream) :-
    writeln(Stream, '% Zarathushtra User Configuration'),
    writeln(Stream, '% ================================'),
    writeln(Stream, '% Supported facts are validated and loaded into kb_config or kb_intents.'),
    writeln(Stream, '% User facts take precedence over built-in defaults.'),
    writeln(Stream, '%'),
    writeln(Stream, '% Uncomment and modify as needed.'),
    writeln(Stream, ''),
    writeln(Stream, '% ---- Custom TODO Settings ----'),
    writeln(Stream, '% todo_destination("~/my-custom-org/tasks.org").'),
    writeln(Stream, '% todo_context_mode(llm_only).  % Options: infer, infer_with_llm, llm_only'),
    writeln(Stream, ''),
    writeln(Stream, '% ---- Custom App Mappings ----'),
    writeln(Stream, '% Override existing apps or add new ones:'),
    writeln(Stream, '% app_mapping(editor, "code").'),
    writeln(Stream, '% app_mapping(music, "spotify").'),
    writeln(Stream, '% app_mapping(recon, "maltego").'),
    writeln(Stream, '% app_mapping(burp, "burpsuite").'),
    writeln(Stream, '% app_mapping(metasploit, "msfconsole").'),
    writeln(Stream, ''),
    writeln(Stream, '% ---- Custom Direct Apps ----'),
    writeln(Stream, '% direct_app(wireshark).'),
    writeln(Stream, '% direct_app(nmap).'),
    writeln(Stream, ''),
    writeln(Stream, '% ---- Custom Intent Verbs ----'),
    writeln(Stream, '% Add your own voice commands:'),
    writeln(Stream, '% verb_intent(hack, open, 1).'),
    writeln(Stream, '% verb_intent(scan, search, 1).'),
    writeln(Stream, '% verb_intent(exploit, ask, rest).'),
    writeln(Stream, ''),
    writeln(Stream, '% ---- Dictation (zara-dictate) ----'),
    writeln(Stream, '% Configure how zara-dictate is launched:'),
    writeln(Stream, '% dictation_command("zara-dictate small cpu 16 2").'),
    writeln(Stream, ''),
    writeln(Stream, '% ---- Timer and Alarm Sounds ----'),
    writeln(Stream, '% timer_sound("/path/to/timer.wav").'),
    writeln(Stream, '% alarm_sound("/path/to/alarm.wav").'),
    writeln(Stream, ''),
    writeln(Stream, '% ---- LLM Provider (for conversational queries) ----'),
    writeln(Stream, '% Choose provider: ollama (default, local) | openai | anthropic'),
    writeln(Stream, '% llm_provider(ollama).'),
    writeln(Stream, '% llm_model("llama3.2").'),
    writeln(Stream, '% llm_endpoint("http://localhost:11434/api/chat").'),
    writeln(Stream, '%'),
    writeln(Stream, '% For OpenAI (requires OPENAI_API_KEY env var):'),
    writeln(Stream, '% llm_provider(openai).'),
    writeln(Stream, '% llm_model("gpt-4o-mini").'),
    writeln(Stream, '%'),
    writeln(Stream, '% For Anthropic (requires ANTHROPIC_API_KEY env var):'),
    writeln(Stream, '% llm_provider(anthropic).'),
    writeln(Stream, '% llm_model("claude-sonnet-4-20250514").'),
    writeln(Stream, ''),
    writeln(Stream, '% ---- OSINT/Security Shortcuts ----'),
    writeln(Stream, '% app_mapping(shodan, "brave --new-window https://shodan.io").'),
    writeln(Stream, '% app_mapping(censys, "brave --new-window https://search.censys.io").'),
    writeln(Stream, '% app_mapping(virustotal, "brave --new-window https://virustotal.com").'),
    writeln(Stream, '% app_mapping(maltego, "maltego").'),
    writeln(Stream, '% app_mapping(recon_ng, "recon-ng").'),
    writeln(Stream, '% app_mapping(theharvester, "theHarvester").'),
    writeln(Stream, '').

%% load_user_config is det.
%
%  Ensures user config exists, then consults it
load_user_config :-
    ensure_user_config,
    user_config_path(Path),
    ( exists_file(Path)
    -> read_user_config(Path, Facts),
       replace_user_config(Facts)
    ; format('Warning: User config not found at ~w~n', [Path])
    ).

%% reload_user_config is det.
%
%  Utility to reload user config during development
reload_user_config :-
    user_config_path(Path),
    ( exists_file(Path)
    -> read_user_config(Path, Facts),
       replace_user_config(Facts),
       format('User config reloaded: ~w~n', [Path])
    ; format('No user config to reload~n')
    ).

read_user_config(Path, Facts) :-
    setup_call_cleanup(
        open(Path, read, Stream),
        read_user_terms(Stream, Path, Facts),
        close(Stream)
    ).

read_user_terms(Stream, Path, Facts) :-
    read_term(Stream, Term, [syntax_errors(error)]),
    ( Term == end_of_file
    -> Facts = []
    ; validate_user_fact(Term, Module, Fact)
    -> Facts = [Module-Fact | Rest],
       read_user_terms(Stream, Path, Rest)
    ; throw(error(domain_error(zarathushtra_user_config_fact, Term),
                  context(Path, 'unsupported or invalid user configuration fact')))
    ).

replace_user_config(Facts) :-
    forall(retract(loaded_clause(Ref)), erase(Ref)),
    forall(member(Module-Fact, Facts),
           ( asserta(Module:Fact, Ref),
             assertz(loaded_clause(Ref))
           )).

validate_user_fact(Module:Term, Module, Fact) :-
    memberchk(Module, [kb_config, kb_intents]),
    validate_user_fact(Term, Module, Fact).
validate_user_fact(app_mapping(Name, Command), kb_config, app_mapping(Name, Command)) :-
    atom(Name),
    text_value(Command).
validate_user_fact(direct_app(Name), kb_config, direct_app(Name)) :-
    atom(Name).
validate_user_fact(search_engine(Template), kb_config, search_engine(Template)) :-
    text_value(Template).
validate_user_fact(dictation_command(Command), kb_config, dictation_command(Command)) :-
    text_value(Command).
validate_user_fact(timer_sound(Path), kb_config, timer_sound(Path)) :-
    text_value(Path).
validate_user_fact(alarm_sound(Path), kb_config, alarm_sound(Path)) :-
    text_value(Path).
validate_user_fact(llm_provider(Provider), kb_config, llm_provider(Provider)) :-
    memberchk(Provider, [ollama, openai, anthropic]).
validate_user_fact(llm_model(Model), kb_config, llm_model(Model)) :-
    text_value(Model).
validate_user_fact(llm_endpoint(Endpoint), kb_config, llm_endpoint(Endpoint)) :-
    text_value(Endpoint).
validate_user_fact(todo_destination(Path), kb_config, todo_destination(Path)) :-
    text_value(Path).
validate_user_fact(todo_context_mode(Mode), kb_config, todo_context_mode(Mode)) :-
    memberchk(Mode, [infer, infer_with_llm, llm_only]).
validate_user_fact(verb_intent(Surface, Intent, Arity), kb_intents,
                   verb_intent(Surface, Intent, Arity)) :-
    atom(Surface),
    valid_intent(Intent),
    ( Arity == rest ; integer(Arity), Arity >= 0 ).

text_value(Value) :-
    atom(Value) ; string(Value).

valid_intent(Intent) :-
    atom(Intent), !.
valid_intent(python(Skill)) :-
    atom(Skill).

search_url(Query, URL) :-
    % 1. Get search template from user config (with default fallback)
    (   kb_config:search_engine(Template)
    ->  true
    ;   Template = "https://duckduckgo.com/?q=~w"  % fallback
    ),

    % 2. URL-encode the query (crucial for special chars)
    uri_encoded(query_value, Query, Encoded),

    % 3. Format the template with encoded query
    format(atom(URL), Template, [Encoded]).
