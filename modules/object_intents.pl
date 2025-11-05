:- module(object_intents, [
    extract_object/2,
    refine_intent/3
]).

:- use_module('../kb/object_words.pl', [object_word/1]).
:- use_module('../modules/normalizer.pl', [is_filler/1]).

extract_object([], '').

extract_object([Word|Rest], Object) :-
    is_filler(Word),
    extract_object(Rest, Object).

extract_object([Word|_], Word) :-
    object_word(Word).

extract_object([_|Rest], Object) :-
    extract_object(Rest, Object).

object_intent(timer, timer).
object_intent(alarm, alarm).
object_intent(reminder, reminder).
object_intent(todo, todo).
object_intent(note, todo).
object_intent(volume, volume_control).
object_intent(brightness, brightness_control).

/*
  refine_intent(+RoughIntent, +Tokens, -FinalIntent)
  If an object in Tokens determines a more specific intent,
  use it. Otherwise keep the RoughIntent.
*/

refine_intent(Rough, Tokens, Final) :-
    extract_object(Tokens, Object),
    object_intent(Object, Final).

refine_intent(Rough, _Tokens, Rough).
