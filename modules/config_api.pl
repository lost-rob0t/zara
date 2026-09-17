:- module(config_api,
    [ defvar/2,
      defcustom/3,
      setq/2,
      symbol_value/2,
      boundp/1,
      makunbound/1,
      variable_doc/2,
      variable_options/2,
      add_hook/2,
      remove_hook/2,
      clear_hook/1,
      hook_functions/2,
      run_hooks/1,
      defcommand/2,
      defcommand/3,
      undefcommand/1,
      commandp/1,
      call_command/1,
      call_command/2,
      commands/1,
      command_doc/2,
      provide/1,
      unprovide/1,
      featurep/1,
      require_feature/1,
      add_load_path/1,
      remove_load_path/1,
      load_path/1,
      load_library/1,
      reset_config_api/0
    ]).

:- use_module(library(apply)).
:- use_module(library(error)).

:- meta_predicate add_hook(+, 0).
:- meta_predicate remove_hook(+, 0).
:- meta_predicate defcommand(+, 0).
:- meta_predicate defcommand(+, 0, +).

:- dynamic config_value/2.
:- dynamic config_spec/3.
:- dynamic config_hook/3.
:- dynamic config_command/4.
:- dynamic provided_feature/1.
:- dynamic config_load_path/1.

%% defvar(+Name, +Default) is det.
%
%  Define a Lisp-style runtime variable without replacing an existing value.
defvar(Name, Default) :-
    must_be(atom, Name),
    with_mutex(zara_config_api,
        ( config_spec(Name, _, _)
        -> true
        ; assertz(config_spec(Name, Default, []))
        )),
    with_mutex(zara_config_api,
        ( config_value(Name, _)
        -> true
        ; assertz(config_value(Name, Default))
        )).

%% defcustom(+Name, +Default, +Options) is det.
%
%  Register a user-facing variable. Options are deliberately open-ended so
%  executable config can carry metadata such as doc/1, type/1 and group/1.
defcustom(Name, Default, Options) :-
    must_be(atom, Name),
    must_be(list, Options),
    with_mutex(zara_config_api,
        ( retractall(config_spec(Name, _, _)),
          assertz(config_spec(Name, Default, Options)),
          ( config_value(Name, _)
          -> true
          ; assertz(config_value(Name, Default))
          )
        )).

%% setq(+Name, +Value) is det.
setq(Name, Value) :-
    must_be(atom, Name),
    with_mutex(zara_config_api,
        ( retractall(config_value(Name, _)),
          assertz(config_value(Name, Value))
        )).

%% symbol_value(+Name, -Value) is det.
symbol_value(Name, Value) :-
    must_be(atom, Name),
    ( config_value(Name, Value)
    -> true
    ; throw(error(existence_error(config_variable, Name), _))
    ).

%% boundp(+Name) is semidet.
boundp(Name) :-
    must_be(atom, Name),
    config_value(Name, _).

%% makunbound(+Name) is det.
makunbound(Name) :-
    must_be(atom, Name),
    with_mutex(zara_config_api, retractall(config_value(Name, _))).

variable_doc(Name, Doc) :-
    must_be(atom, Name),
    config_spec(Name, _, Options),
    ( memberchk(doc(Doc), Options)
    -> true
    ; Doc = ""
    ).

variable_options(Name, Options) :-
    must_be(atom, Name),
    ( config_spec(Name, _, Options)
    -> true
    ; throw(error(existence_error(config_variable_spec, Name), _))
    ).

%% add_hook(+Hook, :Goal) is det.
%
%  Add Goal once, preserving the caller module. Like Emacs add-hook, adding the
%  same function repeatedly is idempotent, which keeps config reloads clean.
add_hook(Hook, Goal0) :-
    must_be(atom, Hook),
    strip_module(Goal0, Module, Goal),
    must_be(callable, Goal),
    with_mutex(zara_config_api,
        ( hook_member_variant(Hook, Module, Goal)
        -> true
        ; assertz(config_hook(Hook, Module, Goal))
        )).

remove_hook(Hook, Goal0) :-
    must_be(atom, Hook),
    strip_module(Goal0, Module, Goal),
    must_be(callable, Goal),
    with_mutex(zara_config_api, remove_hook_variant(Hook, Module, Goal)).

clear_hook(Hook) :-
    must_be(atom, Hook),
    with_mutex(zara_config_api, retractall(config_hook(Hook, _, _))).

hook_functions(Hook, Goals) :-
    must_be(atom, Hook),
    findall(Module:Goal, config_hook(Hook, Module, Goal), Goals).

run_hooks(Hook) :-
    hook_functions(Hook, Goals),
    maplist(call, Goals).

hook_member_variant(Hook, Module, Goal) :-
    config_hook(Hook, StoredModule, StoredGoal),
    StoredModule == Module,
    StoredGoal =@= Goal,
    !.

remove_hook_variant(Hook, Module, Goal) :-
    findall(Ref,
        ( clause(config_hook(Hook, StoredModule, StoredGoal), true, Ref),
          StoredModule == Module,
          StoredGoal =@= Goal
        ),
        Refs),
    maplist(erase, Refs).

%% defcommand(+Name, :Goal) is det.
defcommand(Name, Goal) :-
    defcommand(Name, Goal, "").

%% defcommand(+Name, :Goal, +Doc) is det.
%
%  Define an M-x style named command. call_command/2 applies Args to Goal, so
%  a command can be a zero-arity predicate or a closure expecting arguments.
defcommand(Name, Goal0, Doc) :-
    must_be(atom, Name),
    must_be(text, Doc),
    strip_module(Goal0, Module, Goal),
    must_be(callable, Goal),
    with_mutex(zara_config_api,
        ( retractall(config_command(Name, _, _, _)),
          assertz(config_command(Name, Module, Goal, Doc))
        )).

undefcommand(Name) :-
    must_be(atom, Name),
    with_mutex(zara_config_api, retractall(config_command(Name, _, _, _))).

commandp(Name) :-
    must_be(atom, Name),
    config_command(Name, _, _, _).

call_command(Name) :-
    call_command(Name, []).

call_command(Name, Args) :-
    must_be(atom, Name),
    must_be(list, Args),
    ( config_command(Name, Module, Goal, _)
    -> apply(Module:Goal, Args)
    ; throw(error(existence_error(config_command, Name), _))
    ).

commands(Names) :-
    findall(Name, config_command(Name, _, _, _), Raw),
    sort(Raw, Names).

command_doc(Name, Doc) :-
    must_be(atom, Name),
    ( config_command(Name, _, _, Doc)
    -> true
    ; throw(error(existence_error(config_command, Name), _))
    ).

%% provide(+Feature) is det.
provide(Feature) :-
    must_be(atom, Feature),
    with_mutex(zara_config_api,
        ( provided_feature(Feature)
        -> true
        ; assertz(provided_feature(Feature))
        )).

unprovide(Feature) :-
    must_be(atom, Feature),
    with_mutex(zara_config_api, retractall(provided_feature(Feature))).

featurep(Feature) :-
    must_be(atom, Feature),
    provided_feature(Feature).

%% require_feature(+Feature) is det.
%
%  Emacs-style require: if Feature is absent, load a same-named .pl file from
%  load_path/1 and require that it calls provide/1.
require_feature(Feature) :-
    must_be(atom, Feature),
    ( featurep(Feature)
    -> true
    ; load_library(Feature),
      ( featurep(Feature)
      -> true
      ; throw(error(existence_error(provided_feature, Feature), _))
      )
    ).

add_load_path(Path0) :-
    normalize_directory(Path0, Path),
    with_mutex(zara_config_api,
        ( config_load_path(Path)
        -> true
        ; assertz(config_load_path(Path))
        )).

remove_load_path(Path0) :-
    normalize_path_text(Path0, PathText),
    ( absolute_file_name(PathText, Path,
          [ file_type(directory),
            access(read),
            file_errors(fail)
          ])
    -> with_mutex(zara_config_api, retractall(config_load_path(Path)))
    ; true
    ).

load_path(Path) :-
    config_load_path(Path).

load_library(Name) :-
    must_be(atom, Name),
    atom_concat(Name, '.pl', FileName),
    ( once((config_load_path(Dir),
            directory_file_path(Dir, FileName, File),
            exists_file(File)))
    -> load_files(user:File, [silent(true), if(changed)])
    ; throw(error(existence_error(config_library, Name), _))
    ).

normalize_directory(Path0, Path) :-
    normalize_path_text(Path0, PathText),
    ( absolute_file_name(PathText, Path,
          [ file_type(directory),
            access(read),
            file_errors(fail)
          ])
    -> true
    ; throw(error(existence_error(directory, PathText), _))
    ).

normalize_path_text(Path, Path) :-
    atom(Path), !.
normalize_path_text(Path, Atom) :-
    string(Path), !,
    atom_string(Atom, Path).
normalize_path_text(Path, _) :-
    throw(error(type_error(text, Path), _)).

%% reset_config_api is det.
%
%  Primarily useful for deterministic tests and explicit full reinitialization.
reset_config_api :-
    with_mutex(zara_config_api,
        ( retractall(config_value(_, _)),
          retractall(config_spec(_, _, _)),
          retractall(config_hook(_, _, _)),
          retractall(config_command(_, _, _, _)),
          retractall(provided_feature(_)),
          retractall(config_load_path(_))
        )).
