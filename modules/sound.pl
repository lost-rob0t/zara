:- module(sound, [play_notification_sound/1]).

:- use_module(library(process)).
:- use_module('../kb/device_providers').

:- dynamic sound_player/2.

play_notification_sound(Kind) :-
    configured_sound(Kind, disabled),
    !.
play_notification_sound(Kind) :-
    configured_sound(Kind, ConfiguredPath),
    absolute_file_name(ConfiguredPath, Path,
                       [access(read), file_errors(fail)]),
    sound_player(Kind, Path).

configured_sound(timer, Path) :-
    once(kb_device_providers:timer_sound(Path)).
configured_sound(alarm, Path) :-
    once(kb_device_providers:alarm_sound(Path)).

sound_player(_, Path) :-
    catch(
        process_create(path(mpv),
                       ['--no-video', '--really-quiet', '--', Path],
                       [detached(true)]),
        Error,
        ( format(user_error, 'Sound playback failed: ~w~n', [Error]),
          fail
        )
    ).
