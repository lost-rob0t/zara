:- module(command_loop, [handle_command/1]).
:- use_module(intent_resolver, [convert_number_atoms/2]).
:- use_module('../kb/intents').      % still provide DCG path if desired
:- use_module('commands').
:- use_module('llm_client').
:- use_module('alert').
:- use_module('zara_hooks').

handle_command(String) :-
    format('DEBUG: Input string: "~w"~n', [String]),
    once((
        (   intent_resolver:resolve(String, Intent, ArgsRaw)
        ->  convert_number_atoms(ArgsRaw, Args),
            format('DEBUG: Direct resolution - Intent: ~w, Args: ~w~n', [Intent, Args]),
            zara_hooks:zara_reply(Intent),
            (   catch(commands:execute(Intent, Args), Error,
                    (format('Prolog error: Caused by: ~w.~n', [Error]),
                     format('Routing to LLM for conversational response~n'),
                     fail))
            ->  true
            ;   % Execution failed, fall back to LLM
                format('DEBUG: Command execution failed, routing to LLM~n'),
                rewrite_with_llm(String, Intent2, Args2),
                format('DEBUG: LLM resolution - Intent: ~w, Args: ~w~n', [Intent2, Args2]),
                commands:execute(Intent2, Args2)
            )
        ;   % Initial resolution failed, LLM fallback: ask for a canonical command line
            format('DEBUG: Initial resolution failed, falling back to LLM rewrite~n'),
            rewrite_with_llm(String, Intent2, Args2),
            format('DEBUG: LLM resolution - Intent: ~w, Args: ~w~n', [Intent2, Args2]),
            commands:execute(Intent2, Args2)
        )
    )).
% ---- LLM rewriter (Smart tone S) ----
rewrite_with_llm(UserInput, Intent, Args) :-
    format(string(Prompt),
"Rewrite the following user request into a single canonical command with:
- intent: one of [greet, play, pause, stop, resume, next, skip, call, text, open, lock, unlock, search, navigate, ask, dictation_start, dictation_stop]
- args: array of atoms/strings (1 or 2 as needed)
- Only return compact JSON: {\"intent\":\"...\",\"args\":[...]}
User: ~s", [UserInput]),
    llm_client:llm_query(Prompt, Resp),
    extract_json_intent(Resp, Intent, Args).

% Extremely permissive JSON extractor
extract_json_intent(Resp, Intent, Args) :-
    catch((
        atom_json_dict(Resp, D, []),
        Intent = D.intent,
        Args = D.args
    ), _, (Intent=ask, Args=[Resp])).
