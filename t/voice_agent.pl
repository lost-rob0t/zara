:- begin_tests(voice_agent).
:- use_module('../modules/voice_agent').

test(chatter) :- route("That is pretty cool", chat, "").
test(negation) :- route("Do not stop working", chat, "").
test(quoted_stop) :- route("\"stop\"", chat, "").
test(stop_mention) :- route("What does stop all agents mean?", chat, "").
test(conditional_request) :- route("If I say stop all agents, what happens?", chat, "").
test(speech_only) :- route("Zara, please stop talking!", speech_only, "").
test(bare_stop_is_reversible) :- route("Stop", pause_all, "").
test(stop_fleet) :- route("Stop all agents", cancel_all, "").
test(pause_fleet) :- route("Pause all work", pause_all, "").
test(resume_fleet) :- route("Resume all work", resume_all, "").
test(specific_task) :- route("Cancel task task-abc123", cancel_task, "task-abc123").
test(current_task) :- route("Pause this task", pause_task, "current").
test(status) :- route("What are my agents doing?", status, "").
test(new_goal_preserves_case_and_punctuation) :-
    Text = "Also review /Home/MyProject's README.md", route(Text, new_task, Text).
test(explicit_agent) :-
    Text = "Have an agent check the tests", route(Text, new_task, Text).
test(empty, [fail]) :- route("", _, _).
test(unfinished_task) :- route("Start a task to", chat, "").
test(untrusted_term_is_not_executed) :-
    route("'), assertz(bad), ('", chat, "").
:- end_tests(voice_agent).
