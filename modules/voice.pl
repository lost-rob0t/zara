:- module(voice, [record_and_process/0, transcribe_and_process/1, record_audio/1, transcribe_audio/2]).

:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(llm_client).
:- use_module('parser').

% Main voice recording and processing pipeline
record_and_process :-
    get_temp_audio_file(AudioFile),
    format('Recording audio to ~w...~n', [AudioFile]),
    ( record_audio(AudioFile) ->
        format('Recording complete. Transcribing...~n'),
        transcribe_and_process(AudioFile),
        delete_file(AudioFile)
    ; format('Recording failed.~n'),
      halt(1)
    ).

% Transcribe and process a specific audio file
transcribe_and_process(AudioFile) :-
    ( transcribe_audio(AudioFile, Text) ->
        format('Transcribed: "~w"~n', [Text]),
        parser:handle_command(Text)
    ; format('Transcription failed.~n'),
      halt(1)
    ).

% Record audio using system tools
record_audio(AudioFile) :-
    % Try different recording tools in order of preference
    ( which('arecord') ->
        % ALSA (Linux default)
        format(atom(Cmd), 'timeout 5 arecord -f cd -t wav -d 5 "~w" 2>/dev/null', [AudioFile])
    ; which('sox') ->
        % SoX (cross-platform)
        format(atom(Cmd), 'timeout 5 rec -q "~w" trim 0 5', [AudioFile])
    ; which('ffmpeg') ->
        % FFmpeg fallback
        format(atom(Cmd), 'timeout 5 ffmpeg -f pulse -i default -t 5 -y "~w" 2>/dev/null', [AudioFile])
    ; format('No audio recording tool found (arecord, sox, or ffmpeg required)~n'),
      fail
    ),
    catch(shell(Cmd, Status), _, Status = 1),
    Status =:= 0.

% Transcribe audio using OpenAI Whisper API via LLM client
transcribe_audio(AudioFile, Text) :-
    % Convert to base64 for API transmission
    format(atom(Base64Cmd), 'base64 -w 0 "~w"', [AudioFile]),
    catch(
        (setup_call_cleanup(
            open(pipe(Base64Cmd), read, Stream),
            read_stream_to_codes(Stream, Codes),
            close(Stream)
        ),
        atom_codes(Base64Audio, Codes)),
        _,
        fail
    ),

    % Use Whisper via OpenAI API (through our LLM client)
    format(string(Prompt),
           'Transcribe this audio (base64 encoded): ~w. Only return the transcribed text, no explanations.',
           [Base64Audio]),

    catch(
        (llm_client:llm_query(Prompt, 'You are a speech-to-text transcriber. Return only the transcribed text.', Response),
         atom_string(Response, Text)),
        Error,
        (format('Transcription error: ~w~n', [Error]), fail)
    ).

% Alternative: Use local Whisper if available
transcribe_audio_local(AudioFile, Text) :-
    which('whisper'),
    format(atom(Cmd), 'whisper "~w" --output_format txt --output_dir /tmp --language en 2>/dev/null', [AudioFile]),
    catch(shell(Cmd, 0), _, fail),

    % Read the generated transcript file
    file_base_name(AudioFile, BaseName),
    file_name_extension(BaseNoExt, _, BaseName),
    format(atom(TxtFile), '/tmp/~w.txt', [BaseNoExt]),
    catch(
        (read_file_to_string(TxtFile, Text),
         delete_file(TxtFile)),
        _,
        fail
    ).

% Utility predicates
get_temp_audio_file(File) :-
    get_time(Time),
    format(atom(File), '/tmp/zarathushtra_~w.wav', [Time]).

which(Command) :-
    format(atom(WhichCmd), 'which ~w >/dev/null 2>&1', [Command]),
    catch(shell(WhichCmd, 0), _, fail).

% For debugging - save last transcription
save_transcription(Text) :-
    open('/tmp/zarathushtra_last_transcription.txt', write, Stream),
    write(Stream, Text),
    close(Stream).