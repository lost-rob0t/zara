% ======================================================================
% FILE: kb/todo_context.pl
% ======================================================================

:- module(kb_todo_context,
    [
        infer_context/4,        % +TaskTokens, -Tag, -Category, -Confidence
        enable_llm_fallback/1   % +Boolean
    ]).

/*
   GTD Context & Category Inference
   --------------------------------
   infer_context/4 attempts to infer a GTD context tag and category
   from a list of tokenized words describing a task.

   Confidence is intentionally "chunky" and practical:
   - 0.0 when there are zero keyword hits
   - otherwise scales with number of unique hits

   Optional LLM fallback is supported (won't crash if llm_client missing).
*/

:- dynamic use_llm/1.
use_llm(false).

enable_llm_fallback(true)  :- retractall(use_llm(_)), asserta(use_llm(true)).
enable_llm_fallback(false) :- retractall(use_llm(_)), asserta(use_llm(false)).

% ----------------------------------------------------------------------
% RULE-BASED KEYWORDS
% ----------------------------------------------------------------------

% tag_keywords(Tag, KeywordsList, BaseConfidence).
tag_keywords(call,    [call, phone, dial, text, message, meet, meeting, reply, invite, zoom, talk, speak], 0.85).
tag_keywords(home,    [clean, laundry, dishes, cook, cooking, vacuum, organize, tidy, home, kitchen, bedroom, chore], 0.75).
tag_keywords(computer,[email, code, coding, program, write, typing, online, computer, laptop, research, browser, edit, configure], 0.80).
tag_keywords(errand,  [buy, purchase, store, pickup, pick, drop, mail, package, pharmacy, groceries, shop], 0.78).
tag_keywords(self,    [meditate, meditation, gym, workout, water, sleep, relax, selfcare, hygiene, shower], 0.70).
tag_keywords(learn,   [study, read, reading, learn, lesson, course, research, practice], 0.72).
tag_keywords(creative,[art, draw, drawing, paint, writing, poem, poetry, music, record, film, creative, design], 0.70).
tag_keywords(admin,   [tax, taxes, renew, license, bill, bills, paperwork, budget, banking, admin, official], 0.82).
tag_keywords(social,  [friend, friends, mom, dad, sister, brother, family, party, socialize], 0.80).

% fallback if nothing else hits
tag_keywords(general, [], 0.30).

% Category mapping (Tag → Category string for Org PROPERTY)
tag_category(call,     "SOCIAL").
tag_category(home,     "HOME").
tag_category(computer, "WORK").
tag_category(errand,   "ERRAND").
tag_category(self,     "SELFCARE").
tag_category(learn,    "LEARNING").
tag_category(creative, "CREATIVE").
tag_category(admin,    "ADMIN").
tag_category(social,   "SOCIAL").
tag_category(general,  "GENERAL").

% ----------------------------------------------------------------------
% PUBLIC API
% ----------------------------------------------------------------------

infer_context(TaskTokens, Tag, Category, Confidence) :-
    infer_rule_based(TaskTokens, Tag0, Conf0),
    maybe_llm_refine(TaskTokens, Tag0, Conf0, Tag, Category, Confidence).

% ----------------------------------------------------------------------
% RULE-BASED INFERENCE
% ----------------------------------------------------------------------

infer_rule_based(Toks, BestTag, BestConf) :-
    findall(Conf-Tag,
        (
            tag_keywords(Tag, Keywords, Base),
            score_tokens(Toks, Keywords, Base, Conf)
        ),
        Scores),
    sort(Scores, Sorted),
    reverse(Sorted, [BestConf-BestTag|_]).

% If Keywords empty: fixed fallback Base
% If at least 1 hit: confidence is Base scaled by #unique hits.
score_tokens(_Toks, [], Base, Base) :- !.
score_tokens(Toks, Keywords, Base, Score) :-
    include({Keywords}/[T]>>member(T, Keywords), Toks, Hits0),
    sort(Hits0, Hits),
    length(Hits, Count),
    (   Count =:= 0
    ->  Score = 0.0
    ;   Mult0 is 0.80 + 0.10*(Count-1),
        Mult  is min(1.0, Mult0),
        Score is min(1.0, Base * Mult)
    ).

% ----------------------------------------------------------------------
% LLM FALLBACK
% ----------------------------------------------------------------------

maybe_llm_refine(_, Tag0, Conf0, Tag, Category, Conf) :-
    Conf0 >= 0.60, !,
    Tag = Tag0,
    tag_category(Tag, Category),
    Conf = Conf0.

maybe_llm_refine(Toks, Tag0, Conf0, Tag, Category, Conf) :-
    use_llm(true),
    llm_suggest_tag(Toks, Tag1, Conf1),
    (
        Conf1 > Conf0
    ->  Tag = Tag1, Conf = Conf1
    ;   Tag = Tag0, Conf = Conf0
    ),
    tag_category(Tag, Category), !.

maybe_llm_refine(_, Tag0, Conf0, Tag, Category, Conf) :-
    Tag = Tag0,
    tag_category(Tag, Category),
    Conf = Conf0.

% ----------------------------------------------------------------------
% LLM TAG SUGGESTION (Optional)
% ----------------------------------------------------------------------

llm_suggest_tag(Toks, Tag, Conf) :-
    current_predicate(llm_client:llm_query/2),
    atomic_list_concat(Toks, ' ', Task),
    format(string(Prompt),
"Suggest the most fitting GTD context tag for: \"%s\"
Choose ONE from: call, home, computer, errand, self, learn, creative, admin, social, general
Return JSON ONLY: {\"tag\":\"TAG\",\"confidence\":0.0-1.0}", [Task]),
    catch(
        ( llm_client:llm_query(Prompt, Resp),
          atom_json_dict(Resp, Dict, []),
          Tag = Dict.tag,
          Conf = Dict.confidence
        ),
        _, fail
    ).
