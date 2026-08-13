:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(uuid)).

:- dynamic pending_tool/2.

integration_revision("4cdc9854a510a2d07b559e9ae34491d43d81301a").

:- initialization(main, main).

main :-
    current_prolog_flag(argv, Argv),
    catch(main_args(Argv),
          Exception,
          ( safe_term_string(Exception, Safe),
            emit_json(_{type:"fatal",
                        error:_{kind:"sidecar_exception",
                                message:"Prolog-RLM sidecar crashed",
                                detail:Safe}}),
            halt(1)
          )).

main_args(["--probe", Root]) :-
    !,
    load_rlm(Root),
    integration_revision(Revision),
    emit_json(_{type:"ready", revision:Revision}),
    halt(0).
main_args([Root]) :-
    !,
    load_rlm(Root),
    read_protocol_message(Request),
    require_invoke(Request),
    run_protocol(Request),
    halt(0).
main_args(_) :-
    throw(error(domain_error(sidecar_arguments, argv), _)).

load_rlm(Root0) :-
    text_atom(Root0, Root),
    directory_file_path(Root, 'prolog/rlm_completion.pl', Completion),
    directory_file_path(Root, 'prolog/rlm_chain.pl', Chain),
    directory_file_path(Root, 'prolog/rlm_tool.pl', Tool),
    use_module(Completion),
    use_module(Chain),
    use_module(Tool),
    current_predicate(rlm_completion:rlm_completion/4),
    current_predicate(rlm_completion:rlm_cancel/1),
    current_predicate(rlm_chain:openrouter_provider/2),
    current_predicate(rlm_tool:tool_register/4).

require_invoke(Request) :-
    is_dict(Request),
    get_dict(type, Request, "invoke"),
    get_dict(request_id, Request, RequestId),
    string(RequestId),
    RequestId \== "",
    get_dict(query, Request, Query),
    string(Query),
    !.
require_invoke(_) :-
    throw(error(domain_error(protocol_message, invoke), _)).

run_protocol(Request) :-
    RequestId = Request.request_id,
    rlm_completion:rlm_cancellation_token(Token),
    retractall(pending_tool(_, _)),
    thread_self(Main),
    thread_create(run_worker(Main, Request, Token), Worker, []),
    emit_json(_{type:"started", request_id:RequestId}),
    protocol_loop(Worker, Token, RequestId),
    catch(thread_join(Worker, _), _, true),
    retractall(pending_tool(_, _)).

protocol_loop(Worker, Token, RequestId) :-
    (   thread_get_message(Message, [timeout(0.05)])
    ->  handle_worker_message(Message, Worker, Token, RequestId, Continue)
    ;   Continue = true
    ),
    (   Continue == false
    ->  true
    ;   poll_input(InputReady),
        (   InputReady == true
        ->  read_protocol_message(Control),
            handle_control_message(Control, Token, RequestId)
        ;   true
        ),
        protocol_loop(Worker, Token, RequestId)
    ).

poll_input(true) :-
    wait_for_input([user_input], Ready, 0.0),
    Ready \== [],
    !.
poll_input(false).

handle_worker_message(worker_model_started(Role),
                      _, _, RequestId, true) :-
    !,
    atom_string(Role, RoleString),
    emit_json(_{type:"model_started",
                request_id:RequestId,
                role:RoleString}).
handle_worker_message(worker_model_completed(Role),
                      _, _, RequestId, true) :-
    !,
    atom_string(Role, RoleString),
    emit_json(_{type:"model_completed",
                request_id:RequestId,
                role:RoleString}).
handle_worker_message(worker_tool_call(Sender, ToolCallId, Tool, Args),
                      _, _, RequestId, true) :-
    !,
    with_mutex(sidecar_pending_tool,
               ( retractall(pending_tool(ToolCallId, _)),
                 assertz(pending_tool(ToolCallId, Sender))
               )),
    atom_string(Tool, ToolString),
    emit_json(_{type:"tool_call",
                request_id:RequestId,
                tool_call_id:ToolCallId,
                tool:ToolString,
                args:Args}).
handle_worker_message(worker_outcome(Outcome),
                      _, _, RequestId, false) :-
    !,
    emit_terminal(Outcome, RequestId).
handle_worker_message(_, _, _, _, true).

handle_control_message(Control, Token, RequestId) :-
    is_dict(Control),
    get_dict(request_id, Control, RequestId),
    get_dict(type, Control, Type),
    !,
    handle_control_type(Type, Control, Token).
handle_control_message(_, _, _).

handle_control_type("cancel", _, Token) :-
    !,
    rlm_completion:rlm_cancel(Token).
handle_control_type("tool_result", Control, _) :-
    !,
    get_dict(tool_call_id, Control, ToolCallId),
    with_mutex(sidecar_pending_tool,
               retract(pending_tool(ToolCallId, Sender))),
    thread_send_message(Sender, tool_reply(Control)).
handle_control_type(_, _, _).

run_worker(Main, Request, Token) :-
    catch(worker_outcome(Request, Token, Main, Outcome),
          Exception,
          ( safe_term_string(Exception, Safe),
            Outcome = error(_{phase:runtime,
                              kind:sidecar_worker_exception,
                              message:"Prolog-RLM worker raised an exception",
                              detail:Safe})
          )),
    thread_send_message(Main, worker_outcome(Outcome)).

worker_outcome(Request, Token, Main, Outcome) :-
    request_options(Request, Options),
    text_atom(Options.model, Model),
    rlm_chain:openrouter_provider(Model, Provider),
    (   Options.mode == "direct"
    ->  direct_outcome(Request, Options, Token, Main, Provider, Outcome)
    ;   rlm_outcome(Request, Options, Token, Main, Provider, Outcome)
    ).

direct_outcome(Request, Options, Token, Main, Provider, Outcome) :-
    Handler = sidecar_direct_handler(Main, Provider),
    Budget = _{max_iterations:4,
               max_recursion_depth:0,
               max_concurrent_subcalls:1,
               max_model_calls:Options.max_model_calls,
               max_tool_calls:0,
               max_context_ops:0,
               max_total_tokens:Options.max_total_tokens,
               max_cost_usd:Options.max_cost_usd,
               max_output_bytes:32768,
               time_limit:Options.time_limit},
    DirectOptions = [ provider(Provider),
                      provider_name(openrouter),
                      model_handler(Handler),
                      planner_max_tokens(Options.planner_max_tokens),
                      budget(Budget),
                      cancel_token(Token)
                    ],
    rlm_completion:llm_query(Request.query, DirectOptions, Outcome).

rlm_outcome(Request, Options, Token, Main, Provider, Outcome) :-
    rlm_tool:tool_registry_create(Registry),
    setup_call_cleanup(
        register_calculator(Registry, Main),
        call_rlm(Request, Options, Token, Main, Provider, Registry, Outcome),
        rlm_tool:tool_registry_destroy(Registry)).

call_rlm(Request, Options, Token, Main, Provider, Registry, Outcome) :-
    root_capabilities(Options.max_recursion_depth, Capabilities),
    PlannerHandler = sidecar_planner_handler(Main, Provider),
    Budget = _{max_iterations:16,
               max_recursion_depth:Options.max_recursion_depth,
               max_concurrent_subcalls:2,
               max_model_calls:Options.max_model_calls,
               max_tool_calls:2,
               max_context_ops:4,
               max_total_tokens:Options.max_total_tokens,
               max_cost_usd:Options.max_cost_usd,
               max_output_bytes:32768,
               time_limit:Options.time_limit},
    RLMOptions = [ provider(Provider),
                   provider_name(openrouter),
                   planner_handler(PlannerHandler),
                   capabilities(Capabilities),
                   child_capabilities([model(openrouter)]),
                   tool_registry(Registry),
                   planner_instruction(Options.planner_instruction),
                   planner_max_tokens(Options.planner_max_tokens),
                   budget(Budget),
                   cancel_token(Token)
                 ],
    context_source(Request, Context),
    rlm_completion:rlm_completion(Request.query, Context, RLMOptions, Outcome).

root_capabilities(Depth, [rlm, model(openrouter), tool(calculator)]) :-
    Depth > 0,
    !.
root_capabilities(_, [model(openrouter), tool(calculator)]).

context_source(Request, text(Context)) :-
    (   get_dict(context, Request, Value),
        string(Value)
    ->  Context = Value
    ;   Context = ""
    ).

sidecar_planner_handler(Main, Provider, ModelRequest, Outcome) :-
    thread_send_message(Main, worker_model_started(root_planner)),
    call_cleanup(
        rlm_chain:model_complete(Provider, ModelRequest, Outcome),
        thread_send_message(Main, worker_model_completed(root_planner))).

sidecar_direct_handler(Main, Provider, ModelRequest, Outcome) :-
    thread_send_message(Main, worker_model_started(direct_model)),
    call_cleanup(
        rlm_chain:model_complete(Provider, ModelRequest, Outcome),
        thread_send_message(Main, worker_model_completed(direct_model))).

register_calculator(Registry, Main) :-
    Schema = tool_schema{
                 name:calculator,
                 description:"Evaluate one bounded arithmetic expression through Zara",
                 capability:tool(calculator),
                 arguments:_{
                     type:object,
                     required:[expression],
                     additional_properties:false,
                     properties:_{expression:_{type:string}}
                 },
                 result:_{type:string},
                 limits:tool_limits{
                     time_limit:5.0,
                     max_output_bytes:512
                 }
             },
    Handler = sidecar_calculator_handler(Main),
    rlm_tool:tool_register(Registry, Schema, Handler, Registration),
    (   Registration = ok(_)
    ->  true
    ;   throw(error(calculator_registration_failed(Registration), _))
    ).

sidecar_calculator_handler(Main, Args, Value) :-
    uuid(Id, [version(4)]),
    atom_string(Id, ToolCallId),
    thread_self(Self),
    thread_send_message(
        Main,
        worker_tool_call(Self, ToolCallId, calculator, Args)),
    thread_get_message(tool_reply(Reply)),
    calculator_reply(Reply, Value).

calculator_reply(Reply, Value) :-
    get_dict(status, Reply, "ok"),
    !,
    get_dict(value, Reply, Value),
    string(Value).
calculator_reply(Reply, _) :-
    (   get_dict(error, Reply, Error)
    ->  safe_term_string(Error, Safe)
    ;   Safe = "calculator bridge failed"
    ),
    throw(error(zara_tool_error(Safe), _)).

request_options(Request, Options) :-
    (   get_dict(options, Request, Raw), is_dict(Raw)
    ->  true
    ;   Raw = _{}
    ),
    dict_string(Raw, mode, "rlm", Mode),
    memberchk(Mode, ["rlm", "direct"]),
    dict_string(Raw, model, "openrouter/free", Model),
    dict_string(Raw, planner_instruction, "", Instruction),
    dict_positive_integer(Raw, planner_max_tokens, 512, PlannerMaxTokens),
    dict_nonnegative_integer(Raw, max_recursion_depth, 0, MaxDepth),
    dict_positive_integer(Raw, max_model_calls, 4, MaxModelCalls),
    dict_positive_integer(Raw, max_total_tokens, 8192, MaxTokens),
    dict_nonnegative_number(Raw, max_cost_usd, 0.25, MaxCost),
    dict_positive_number(Raw, time_limit, 45.0, TimeLimit),
    Options = _{mode:Mode,
                model:Model,
                planner_instruction:Instruction,
                planner_max_tokens:PlannerMaxTokens,
                max_recursion_depth:MaxDepth,
                max_model_calls:MaxModelCalls,
                max_total_tokens:MaxTokens,
                max_cost_usd:MaxCost,
                time_limit:TimeLimit}.

dict_string(Dict, Key, Default, Value) :-
    (   get_dict(Key, Dict, Found)
    ->  string(Found), Value = Found
    ;   Value = Default
    ).

dict_positive_integer(Dict, Key, Default, Value) :-
    dict_default(Dict, Key, Default, Value),
    integer(Value),
    Value > 0.

dict_nonnegative_integer(Dict, Key, Default, Value) :-
    dict_default(Dict, Key, Default, Value),
    integer(Value),
    Value >= 0.

dict_positive_number(Dict, Key, Default, Value) :-
    dict_default(Dict, Key, Default, Value),
    number(Value),
    Value > 0.

dict_nonnegative_number(Dict, Key, Default, Value) :-
    dict_default(Dict, Key, Default, Value),
    number(Value),
    Value >= 0.

dict_default(Dict, Key, Default, Value) :-
    (   get_dict(Key, Dict, Found)
    ->  Value = Found
    ;   Value = Default
    ).

emit_terminal(ok(Result), RequestId) :-
    !,
    success_payload(Result, Payload),
    emit_json(_{type:"completed",
                request_id:RequestId,
                result:Payload}).
emit_terminal(error(Error), RequestId) :-
    !,
    error_payload(Error, Payload),
    (   Payload.kind == "cancelled"
    ->  emit_json(_{type:"cancelled",
                    request_id:RequestId,
                    error:Payload})
    ;   emit_json(_{type:"failed",
                    request_id:RequestId,
                    error:Payload})
    ).
emit_terminal(Other, RequestId) :-
    safe_term_string(Other, Safe),
    emit_json(_{type:"failed",
                request_id:RequestId,
                error:_{kind:"invalid_outcome",
                         phase:"runtime",
                         message:"Prolog-RLM returned an invalid outcome",
                         detail:Safe}}).

success_payload(Result, Payload) :-
    response_text(Result, Text),
    result_usage(Result, Usage),
    result_recursion(Result, Recursion),
    result_model_events(Result, ModelEvents),
    result_transition_count(Result, TransitionCount),
    Payload = _{text:Text,
                usage:Usage,
                recursion:Recursion,
                model_events:ModelEvents,
                transition_count:TransitionCount}.

response_text(Result, Text) :-
    is_dict(Result),
    get_dict(response, Result, Response),
    !,
    response_value_text(Response, Text).
response_text(Result, Text) :-
    is_dict(Result),
    get_dict(value, Result, Value),
    !,
    response_value_text(Value, Text).
response_text(Value, Text) :-
    response_value_text(Value, Text).

response_value_text(Value, Text) :-
    string(Value),
    !,
    Text = Value.
response_value_text(Value, Text) :-
    atom(Value),
    !,
    atom_string(Value, Text).
response_value_text(Value, Text) :-
    is_dict(Value),
    get_dict(text, Value, Found),
    string(Found),
    !,
    Text = Found.
response_value_text(Value, Text) :-
    is_dict(Value),
    get_dict(content, Value, Found),
    string(Found),
    !,
    Text = Found.
response_value_text(Value, Text) :-
    is_dict(Value),
    get_dict(assistant, Value, Assistant),
    is_dict(Assistant),
    get_dict(content, Assistant, Found),
    string(Found),
    !,
    Text = Found.
response_value_text(Value, Text) :-
    is_dict(Value),
    get_dict(value, Value, Inner),
    !,
    response_value_text(Inner, Text).
response_value_text(Value, Text) :-
    safe_term_string(Value, Text).

result_usage(Result, Usage) :-
    is_dict(Result),
    get_dict(usage, Result, Raw),
    is_dict(Raw),
    !,
    scalar_dict(Raw, Usage).
result_usage(_, _{}).

result_recursion(Result, Recursion) :-
    is_dict(Result),
    get_dict(recursion, Result, Raw),
    is_dict(Raw),
    !,
    dict_number(Raw, recursive_calls, 0, Calls),
    dict_number(Raw, max_depth, 0, Depth),
    Recursion = _{recursive_calls:Calls, max_depth:Depth}.
result_recursion(_, _{recursive_calls:0, max_depth:0}).

result_model_events(Result, Events) :-
    is_dict(Result),
    get_dict(trajectory, Result, Trajectory),
    is_dict(Trajectory),
    get_dict(events, Trajectory, RawEvents),
    is_list(RawEvents),
    !,
    maplist(model_event_payload, RawEvents, Events).
result_model_events(Result, Events) :-
    is_dict(Result),
    get_dict(trajectory, Result, RawEvents),
    is_list(RawEvents),
    !,
    maplist(model_event_payload, RawEvents, Events).
result_model_events(_, []).

model_event_payload(Event, Payload) :-
    is_dict(Event),
    !,
    dict_text(Event, provider, "unknown", Provider),
    dict_text(Event, selected_model, "unknown", SelectedModel),
    dict_number(Event, depth, 0, Depth),
    dict_number(Event, http_status, 0, Status),
    Payload = _{provider:Provider,
                selected_model:SelectedModel,
                depth:Depth,
                http_status:Status}.
model_event_payload(Event, _{detail:Safe}) :-
    safe_term_string(Event, Safe).

result_transition_count(Result, Count) :-
    is_dict(Result),
    get_dict(transitions, Result, Transitions),
    is_list(Transitions),
    !,
    length(Transitions, Count).
result_transition_count(_, 0).

error_payload(Error, Payload) :-
    is_dict(Error),
    !,
    dict_text(Error, kind, "rlm_error", Kind),
    dict_text(Error, phase, "runtime", Phase),
    dict_text(Error, message, "Prolog-RLM request failed", Message),
    error_detail(Error, Detail),
    Payload = _{kind:Kind,
                phase:Phase,
                message:Message,
                detail:Detail}.
error_payload(Error, _{kind:"rlm_error",
                       phase:"runtime",
                       message:"Prolog-RLM request failed",
                       detail:Safe}) :-
    safe_term_string(Error, Safe).

error_detail(Error, Detail) :-
    (   get_dict(detail, Error, Raw)
    ->  safe_json_text(Raw, Detail)
    ;   get_dict(cause, Error, Raw)
    ->  safe_json_text(Raw, Detail)
    ;   get_dict(exception, Error, Raw)
    ->  safe_json_text(Raw, Detail)
    ;   Detail = ""
    ).

scalar_dict(Dict, Out) :-
    dict_pairs(Dict, _, Pairs),
    include(scalar_pair, Pairs, Scalars),
    dict_pairs(Out, _, Scalars).

scalar_pair(_-Value) :-
    ( number(Value)
    ; string(Value)
    ; atom(Value)
    ).

dict_text(Dict, Key, Default, Text) :-
    (   get_dict(Key, Dict, Value)
    ->  safe_json_text(Value, Text)
    ;   Text = Default
    ).

dict_number(Dict, Key, Default, Number) :-
    (   get_dict(Key, Dict, Value), number(Value)
    ->  Number = Value
    ;   Number = Default
    ).

safe_json_text(Value, Text) :-
    string(Value),
    !,
    Text = Value.
safe_json_text(Value, Text) :-
    atom(Value),
    !,
    atom_string(Value, Text).
safe_json_text(Value, Text) :-
    safe_term_string(Value, Text).

safe_term_string(Term, Text) :-
    term_string(Term, Text, [quoted(true), numbervars(true), max_depth(8)]).

read_protocol_message(Dict) :-
    json_read_dict(user_input, Dict, [value_string_as(string)]).

emit_json(Dict) :-
    json_write_dict(current_output, Dict, [width(0)]),
    nl,
    flush_output.

text_atom(Text, Atom) :-
    atom(Text),
    !,
    Atom = Text.
text_atom(Text, Atom) :-
    string(Text),
    !,
    atom_string(Atom, Text).
text_atom(Value, _) :-
    throw(error(type_error(text, Value), _)).
