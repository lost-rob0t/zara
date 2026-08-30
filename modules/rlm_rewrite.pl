:- module(rlm_rewrite, [
    rewrite_with_rlm/3,
    rewrite_with_rlm/4,
    rewrite_prompt/2,
    rewrite_options/2,
    ensure_runtime_loaded/0,
    reset_rlm_runtime/0
]).

/** <module> Prolog-RLM direct-mode LLM rewrites

Routes Prolog command fallback rewrites through the pinned Prolog-RLM
native direct runtime (`rlm_direct/4`) over OpenRouter. The RLM runtime is
loaded lazily from the directory named by ZARA_PROLOG_RLM_ROOT (injected by
the Nix wrappers). Every failure path is closed: missing root, missing
contract, missing credential, and direct-runtime errors surface as typed
`rlm_rewrite_error/1` exceptions that the command resolution gate converts
into a failed resolution result.

The pinned Prolog-RLM revision is
b654831a0150a593821179da1d3886dfd64deb5c.
*/

:- use_module('../kb/config').

:- dynamic runtime_loaded/1.

rewrite_with_rlm(UserInput, Intent, Args) :-
    rewrite_with_rlm(UserInput, Intent, Args, []).

rewrite_with_rlm(UserInput, Intent, Args, ExtraOptions) :-
    ensure_runtime_loaded,
    require_api_key,
    rewrite_prompt(UserInput, Query),
    rewrite_options(ExtraOptions, Options),
    rlm_direct:rlm_direct(Query, text(""), Options, Outcome),
    require_direct_success(Outcome, Value),
    extract_json_intent(Value, Intent, Args).

rewrite_prompt(UserInput, Prompt) :-
    canonical_intent_list(Intents),
    format(string(Prompt),
"Rewrite the following user request into a single canonical command with:
- intent: one of ~w
- args: array of atoms/strings (1 or 2 as needed)
- Only return compact JSON: {\"intent\":\"...\",\"args\":[...]}
User: ~w", [Intents, UserInput]).

canonical_intent_list([greet, play, pause, stop, resume, next, skip, call,
                       text, open, lock, unlock, search, navigate, ask,
                       dictation_start, dictation_stop]).

rewrite_options(ExtraOptions, Options) :-
    rewrite_model(Model),
    rlm_chain:openrouter_provider(Model, Provider),
    Budget = _{max_model_calls:2,
               max_tool_calls:0,
               max_context_ops:0,
               max_total_tokens:4096,
               max_cost_usd:0.25,
               max_output_bytes:8192,
               time_limit:45.0},
    append([provider(Provider),
            provider_name(openrouter),
            capabilities([]),
            budget(Budget),
            planner_max_tokens(1024),
            temperature(0)],
           ExtraOptions, Options).

rewrite_model(Model) :-
    (   kb_config:prolog_rlm_model(Model), text_value(Model)
    ->  true
    ;   Model = "openrouter/free"
    ).

ensure_runtime_loaded :-
    runtime_loaded(loaded), !.
ensure_runtime_loaded :-
    rlm_root(Root),
    load_rlm_runtime(Root),
    asserta(runtime_loaded(loaded)).

reset_rlm_runtime :-
    retractall(runtime_loaded(_)).

rlm_root(Root) :-
    getenv('ZARA_PROLOG_RLM_ROOT', Value),
    (   Value == ''
    ->  throw(error(rlm_rewrite_error(root_missing), _))
    ;   true
    ),
    !,
    atom_string(Value, Root),
    (   exists_directory(Root)
    ->  true
    ;   throw(error(rlm_rewrite_error(root_invalid(Root)), _))
    ).
rlm_root(_) :-
    throw(error(rlm_rewrite_error(root_missing), _)).

load_rlm_runtime(Root) :-
    directory_file_path(Root, 'prolog/rlm_direct.pl', DirectFile),
    directory_file_path(Root, 'prolog/rlm_chain.pl', ChainFile),
    catch(( use_module(DirectFile),
            use_module(ChainFile) ),
          Error,
          ( format(user_error, "[rlm_rewrite] Prolog-RLM load failed: ~w~n", [Error]),
            throw(error(rlm_rewrite_error(load_failed(Error)), _)) )),
    require_contract.

require_contract :-
    (   current_predicate(rlm_direct:rlm_direct/4),
        current_predicate(rlm_chain:openrouter_provider/2)
    ->  true
    ;   throw(error(rlm_rewrite_error(contract_mismatch), _))
    ).

require_api_key :-
    (   getenv('OPENROUTER_API_KEY', Key), Key \== ''
    ->  true
    ;   throw(error(rlm_rewrite_error(missing_api_key), _))
    ).

require_direct_success(ok(Result), Value) :-
    !,
    (   get_dict(value, Result, Value),
        ( atom(Value) ; string(Value) ),
        Value \== ''
    ->  true
    ;   throw(error(rlm_rewrite_error(direct_missing_value), _))
    ).
require_direct_success(error(Error), _) :-
    throw(error(rlm_rewrite_error(direct(Error)), _)).

extract_json_intent(Resp, Intent, Args) :-
    catch(
        ( atom_json_dict(Resp, Dict, []),
          Intent = Dict.intent,
          Args = Dict.args
        ),
        _,
        (Intent = ask, Args = [Resp])
    ).

text_value(Value) :-
    atom(Value) ; string(Value).
