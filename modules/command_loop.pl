:- module(command_loop, [
    handle_command/1,
    execute_resolved/3
]).

:- use_module(intent_resolver, [convert_number_atoms/2]).
:- use_module(llm_client, [llm_query_with_history/2]).
:- use_module('../kb/intents').
:- use_module('commands').
:- use_module('zara_hooks').

handle_command(String) :-
    zara_hooks:acknowledge,
    format('DEBUG: Input string: "~w"~n', [String]),
    resolution_result(String, Resolution),
    handle_resolution(Resolution).

handle_resolution(resolved(Intent, Args)) :-
    format('DEBUG: Resolved - Intent: ~w, Args: ~w~n', [Intent, Args]),
    execute_resolved(Intent, Args, Result),
    Result = command_result(success, Intent, Args, none).
handle_resolution(Result) :-
    Result = command_result(failure, command_resolution, _, _),
    zara_hooks:reply_result(Result),
    fail.

resolution_result(String, Result) :-
    catch(
        ( once(resolve_command(String, Intent, Args))
        -> Result = resolved(Intent, Args)
        ;  Result = command_result(failure, command_resolution,
                                   [String], failed)
        ),
        Error,
        Result = command_result(failure, command_resolution,
                                [String], exception(Error))
    ).

resolve_command(String, Intent, Args) :-
    intent_resolver:resolve(String, Intent, ArgsRaw),
    !,
    convert_number_atoms(ArgsRaw, Args).
resolve_command(String, Intent, Args) :-
    format('DEBUG: Initial resolution failed, falling back to LLM rewrite~n'),
    rewrite_with_llm(String, Intent, Args).

execute_resolved(Intent, Args, Result) :-
    execution_result(Intent, Args, Result),
    zara_hooks:reply_result(Result).

execution_result(Intent, Args, Result) :-
    catch(
        ( commands:execute(Intent, Args)
        -> Result = command_result(success, Intent, Args, none)
        ;  Result = command_result(failure, Intent, Args, failed)
        ),
        Error,
        Result = command_result(failure, Intent, Args, exception(Error))
    ).

rewrite_with_llm(UserInput, Intent, Args) :-
    format(string(Prompt),
"Rewrite the following user request into a single canonical command with:\n- intent: one of [greet, play, pause, stop, resume, next, skip, call, text, open, lock, unlock, search, navigate, ask, dictation_start, dictation_stop]\n- args: array of atoms/strings (1 or 2 as needed)\n- Only return compact JSON: {\"intent\":\"...\",\"args\":[...]}\nUser: ~w", [UserInput]),
    llm_client:llm_query_with_history(Prompt, Resp),
    extract_json_intent(Resp, Intent, Args).

extract_json_intent(Resp, Intent, Args) :-
    catch(
        ( atom_json_dict(Resp, Dict, []),
          Intent = Dict.intent,
          Args = Dict.args
        ),
        _,
        (Intent = ask, Args = [Resp])
    ).
