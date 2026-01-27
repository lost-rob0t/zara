:- dynamic sound_base/1.

% Initialize on load
:- initialization(init_sound_base).

init_sound_base :-
    absolute_file_name('~/.zarathushtra/assets/sounds', Base,
                       [file_type(directory), access(read)]),
    assertz(sound_base(Base)).

% Now your sounds are just filenames
sound_file(timer, 'timer/timer-beeping.wav').

load_sound(Name, FullPath) :-
    sound_base(Base),
    sound_file(Name, RelPath),
    atomic_list_concat([Base, '/', RelPath], FullPath),
    exists_file(FullPath).
