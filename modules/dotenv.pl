:- module(dotenv, [load_env/0, load_env/1]).

:- use_module(library(readutil)).

load_env(File) :-
    exists_file(File),
    open(File, read, Stream),
    read_env_lines(Stream),
    close(Stream).

read_env_lines(Stream) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  true
    ;   parse_env_line(Line),
        read_env_lines(Stream)
    ).

parse_env_line(Line) :-
    split_string(Line, "=", "", [Key, Value]),
    atom_string(KeyAtom, Key),
    setenv(KeyAtom, Value).
parse_env_line(_).  % Skip malformed lines

load_env :-
    load_env('.env').
