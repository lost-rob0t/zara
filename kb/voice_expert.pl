% ======================================================================
% FILE: kb/voice_expert.pl
% ======================================================================
% Prolog authority for Zara voice selection.
%
% The LLM/Python layer may propose semantic speaker roles and requested
% voices, but this module owns the final deterministic voice decision.
% Python executes the selected plan; it must not invent a fallback voice.
%
% Mutable operator overrides are validated by modules/config_loader.pl and
% belong in ~/.config/zarathushtra/config.local.pl.

:- module(kb_voice_expert,
    [
        voice_expert_version/1,
        voice_default/1,
        voice_role/2,
        voice_policy/2,
        resolve_voice/5
    ]).

:- dynamic voice_default/1.
:- dynamic voice_role/2.
:- dynamic voice_policy/2.

:- discontiguous kb_voice_expert:voice_default/1.
:- discontiguous kb_voice_expert:voice_role/2.
:- discontiguous kb_voice_expert:voice_policy/2.

voice_expert_version(1).

% Default is deliberately single-voice. User config is installed with
% asserta/2, so an operator override wins over these packaged fallbacks.
voice_default("zara").
voice_role(narrator, "zara").

% Roles that semantically represent a distinct speaker may consume another
% available voice. Unknown roles remain on the configured/default voice.
voice_policy(narrator, prefer_default).
voice_policy(dialogue, distinct_if_available).
voice_policy(character, distinct_if_available).
voice_policy(quoted_speaker, distinct_if_available).
voice_policy(interviewer, distinct_if_available).
voice_policy(interviewee, distinct_if_available).

%% resolve_voice(+RoleText, +RequestedVoice, +Available, +Used, -Voice) is semidet.
%
% Priority:
%   1. an explicit requested voice, if it actually exists;
%   2. a configured role mapping, if available;
%   3. for a role requiring distinction, the first not-yet-used voice;
%   4. configured default voice;
%   5. first available voice.
%
% Available and Used contain strings. RoleText may be an atom or string.
resolve_voice(RoleText, RequestedVoice, Available, Used, Voice) :-
    is_list(Available),
    Available = [_|_],
    is_list(Used),
    role_atom(RoleText, Role),
    (   usable_requested_voice(RequestedVoice, Available, Voice)
    ->  true
    ;   voice_role(Role, Configured),
        memberchk(Configured, Available)
    ->  Voice = Configured
    ;   voice_policy(Role, distinct_if_available),
        first_unused_voice(Available, Used, Distinct)
    ->  Voice = Distinct
    ;   voice_default(Default),
        memberchk(Default, Available)
    ->  Voice = Default
    ;   Available = [Voice|_]
    ).

usable_requested_voice(RequestedVoice, Available, RequestedVoice) :-
    nonempty_text(RequestedVoice),
    memberchk(RequestedVoice, Available).

first_unused_voice([Voice|_], Used, Voice) :-
    \+ memberchk(Voice, Used),
    !.
first_unused_voice([_|Voices], Used, Voice) :-
    first_unused_voice(Voices, Used, Voice).

role_atom(Role, Atom) :-
    atom(Role),
    !,
    downcase_atom(Role, Atom).
role_atom(Role, Atom) :-
    string(Role),
    Role \= "",
    atom_string(Raw, Role),
    downcase_atom(Raw, Atom).

nonempty_text(Value) :-
    string(Value),
    !,
    Value \= "".
nonempty_text(Value) :-
    atom(Value),
    Value \= ''.
