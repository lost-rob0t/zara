:- module(agent_profiles,
    [
        resolve_mention/7
    ]).

:- use_module('../kb/agent_profiles').

resolve_mention(Mention, Id, Display, Prompt, Tools, KBs, MemoryScope) :-
    mention_key(Mention, Key),
    once(profile_id_for_key(Key, Id)),
    once(kb_agent_profiles:agent_profile(Id, Display)),
    profile_prompt(Id, Prompt),
    profile_tools(Id, Tools),
    profile_kbs(Id, KBs),
    profile_memory_scope(Id, MemoryScope).

profile_id_for_key(Key, Id) :-
    kb_agent_profiles:agent_profile(Id, Display),
    ( profile_key(Id, Key)
    ; profile_key(Display, Key)
    ).
profile_id_for_key(Key, Id) :-
    kb_agent_profiles:agent_profile_alias(Id, Alias),
    profile_key(Alias, Key),
    kb_agent_profiles:agent_profile(Id, _).

profile_prompt(Id, Prompt) :-
    ( once(kb_agent_profiles:agent_profile_prompt(Id, Prompt0))
    -> Prompt = Prompt0
    ; Prompt = ""
    ).

profile_tools(Id, Tools) :-
    ( once(kb_agent_profiles:agent_profile_tools(Id, Tools0))
    -> Tools = Tools0
    ; Tools = all
    ).

profile_kbs(Id, KBs) :-
    ( once(kb_agent_profiles:agent_profile_kbs(Id, KBs0))
    -> KBs = KBs0
    ; KBs = []
    ).

profile_memory_scope(Id, Scope) :-
    ( once(kb_agent_profiles:agent_profile_memory_scope(Id, Scope0))
    -> Scope = Scope0
    ; Scope = shared
    ).

mention_key(Mention, Key) :-
    text_string(Mention, Raw),
    normalize_space(string(Trimmed), Raw),
    ( sub_string(Trimmed, 0, 1, _, "@")
    -> sub_string(Trimmed, 1, _, 0, Bare)
    ; Bare = Trimmed
    ),
    profile_key(Bare, Key).

profile_key(Value, Key) :-
    text_string(Value, Raw),
    string_lower(Raw, Lower),
    split_string(Lower, " _-", " _-", Parts),
    Parts \= [],
    atomics_to_string(Parts, "-", Key).

text_string(Value, String) :-
    ( string(Value)
    -> String = Value
    ; atom(Value)
    -> atom_string(Value, String)
    ).
