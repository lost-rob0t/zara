#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(http/json)).
:- use_module(library(process)).
:- use_module('kb/intents').
:- use_module('kb/config').
:- use_module('modules/commands').
:- use_module('modules/parser').

handle_input(Input) :-
    catch(
        (parser:handle_command(Input), !),
        Error,
        format('Error: ~w~n', [Error])
    ).

zara_voice_main :-
    (   record_audio(Wav)
    ->  transcribe_json(Wav, RawLines),
        clean_lines(RawLines, Lines),
        run_cmds(Lines),
        halt(0)
    ;   writeln('Recording failed'),
        halt(1)
    ).

record_audio(Wav) :-
    tmp_file_stream(binary, Wav, S), close(S),
    Duration = 4, Rate = 16000, Chan = 1,
    format("Recording ~d seconds...~n", [Duration]),
    (   exists_in_path(arecord)
    ->  format(atom(Cmd), 'arecord -f S16_LE -r ~d -c ~d -d ~d ~w 2>/dev/null',
               [Rate, Chan, Duration, Wav])
    ;   exists_in_path(rec)
    ->  format(atom(Cmd), 'rec -q -r ~d -c ~d ~w trim 0 ~d',
               [Rate, Chan, Wav, Duration])
    ;   exists_in_path(ffmpeg)
    ->  format(atom(Cmd), 'ffmpeg -f pulse -i default -ac ~d -ar ~d -t ~d -y ~w 2>/dev/null',
               [Chan, Rate, Duration, Wav])
    ;   writeln("No recorder found"), fail
    ),
    shell(Cmd, Status),
    (   Status =:= 0, exists_file(Wav)
    ->  format("Recorded: ~w~n", [Wav])
    ;   writeln("Recording failed"), fail
    ).

exists_in_path(Cmd) :-
    format(atom(Check), 'command -v ~w >/dev/null 2>&1', [Cmd]),
    catch(shell(Check, 0), _, fail).

transcribe_json(Wav, Lines) :-
    (   exists_in_path(whisper)
    ->  writeln("Transcribing...")
    ;   writeln("whisper not found"), fail
    ),
    (   current_prolog_flag(cpu_count, Cores)
    ->  true
    ;   Cores = 2
    ),
    file_directory_name(Wav, Dir),
    atomic_list_concat([
        'whisper "', Wav, '" ',
        '--output_format json ',
        '--output_dir "', Dir, '" ',
        '--language en ',
        '--model tiny ',
        '--threads ', Cores, ' ',
        '--fp16 False ',
        '--no_speech_threshold 0.1 ',
        '--condition_on_previous_text False ',
        '2>/dev/null'
    ], Cmd),
    shell(Cmd, Status),
    file_name_extension(Base, _, Wav),
    atom_concat(Base, '.json', JsonFile),
    (   Status =:= 0, exists_file(JsonFile)
    ->  read_json_segments(JsonFile, Lines),
        delete_file_safe(Wav),
        delete_file_safe(JsonFile),
        length(Lines, Len),
        format("Transcribed ~d segments~n", [Len])
    ;   writeln("Transcription failed"),
        delete_file_safe(Wav),
        fail
    ).

read_json_segments(JsonFile, Lines) :-
    setup_call_cleanup(
        open(JsonFile, read, Stream),
        json_read_dict(Stream, Dict),
        close(Stream)
    ),
    (   get_dict(segments, Dict, Segments)
    ->  findall(Text,
                (member(Seg, Segments),
                 get_dict(text, Seg, Text)),
                Lines)
    ;   Lines = []
    ).

delete_file_safe(File) :-
    catch(delete_file(File), _, true).

clean_lines(Raw, Cleaned) :-
    maplist(strip_timestamps, Raw, S1),
    maplist(trim_string, S1, S2),
    exclude(empty_string, S2, S3),
    dedupe(S3, Cleaned).

strip_timestamps(In, Out) :-
    (   string(In)
    ->  split_string(In, "[", "]", Parts),
        include(not_timestamp, Parts, ValidParts),
        atomic_list_concat(ValidParts, ' ', Tmp),
        trim_string(Tmp, Out)
    ;   atom(In)
    ->  atom_string(In, InStr),
        strip_timestamps(InStr, Out)
    ;   Out = In
    ).

not_timestamp(S) :- \+ sub_string(S, _, _, _, "-->").

trim_string(In, Out) :-
    normalize_space(atom(Out), In).

empty_string("").
empty_string(S) :- string(S), string_length(S, 0).
empty_string(A) :- atom(A), atom_length(A, 0).

dedupe(List, Unique) :- dedupe(List, [], Unique).
dedupe([], _, []).
dedupe([H|T], Seen, Result) :-
    (   memberchk(H, Seen)
    ->  dedupe(T, Seen, Result)
    ;   Result = [H|Rest],
        dedupe(T, [H|Seen], Rest)
    ).

run_cmds([]) :-
    writeln("No commands heard."), !.

run_cmds([Cmd|Rest]) :-
    (   atom(Cmd)
    ->  CmdAtom = Cmd
    ;   string(Cmd)
    ->  atom_string(CmdAtom, Cmd)
    ;   CmdAtom = Cmd
    ),
    format('~nProcessing: ~w~n', [CmdAtom]),
    ignore(handle_input(CmdAtom)),
    run_cmds(Rest).

:- initialization(zara_voice_main, main).
