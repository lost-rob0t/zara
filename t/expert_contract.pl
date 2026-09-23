% ZARA-EXPERT/1 portable codec tests (issue #1233, phase 2).
%
% Pins the modules/expert_contract.pl projection of zara/experts.py and the
% shared contracts/zara-expert-v1/descriptors.tsv fixture: descriptor
% inventory, protocol-major gating, selectable semantics, the exact
% activation transition table, closed verdict/error vocabularies, pure
% keyword matching with typed ambiguity, the sorted ground corpus and
% validated alias assertion.

:- begin_tests(expert_contract).

:- use_module('../modules/expert_contract').

% --- descriptor inventory and projections -------------------------------------

test(descriptor_inventory_is_the_four_fixture_experts) :-
    findall(Id, expert_contract:expert_descriptor(Id, _, _, _, _), Ids),
    Ids == ['zara:expert/android-troubleshooting',
            'zara:expert/future',
            'zara:expert/prolog-rlm',
            'zara:expert/todo'].

test(todo_descriptor_projection_pinned) :-
    expert_contract:expert_descriptor('zara:expert/todo', '1.0.0', zara,
        symbolic, 'sha256:todo.expert.v1').

test(unknown_expert_descriptor_fails) :-
    \+ expert_contract:expert_descriptor('zara:expert/ghost', _, _, _, _).

test(protocol_constant_is_zara_expert_1) :-
    expert_contract:expert_protocol('ZARA-EXPERT/1').

test(incompatible_major_stays_visible_with_declared_protocol) :-
    expert_contract:expert_descriptor_protocol('zara:expert/future',
        'ZARA-EXPERT/2').

test(availability_encoded_consistently_with_tsv) :-
    findall(Id-Availability,
        expert_contract:expert_availability(Id, Availability), Pairs),
    Pairs == ['zara:expert/android-troubleshooting'-available,
              'zara:expert/future'-ready,
              'zara:expert/prolog-rlm'-absent,
              'zara:expert/todo'-ready].

test(todo_operations_projected_in_order) :-
    findall(Operation,
        expert_contract:expert_operation('zara:expert/todo', Operation, [], []),
        Operations),
    Operations == ['route.diagnose', 'route.explain'].

test(operation_inventory_spans_all_fixture_experts) :-
    findall(Id-Operation,
        expert_contract:expert_operation(Id, Operation, [], []), Pairs),
    Pairs == ['zara:expert/android-troubleshooting'-'adb.diagnose',
              'zara:expert/android-troubleshooting'-'logs.explain',
              'zara:expert/future'-'demo.ping',
              'zara:expert/prolog-rlm'-'rlm.explain',
              'zara:expert/prolog-rlm'-'rlm.query',
              'zara:expert/todo'-'route.diagnose',
              'zara:expert/todo'-'route.explain'].

% --- protocol-major gating ------------------------------------------------------

test(protocol_compatible_accepts_major_one) :-
    expert_contract:expert_protocol_compatible('ZARA-EXPERT/1').

test(unknown_protocol_major_fails_closed, [forall(bad_protocol(P))]) :-
    \+ expert_contract:expert_protocol_compatible(P).

bad_protocol('ZARA-EXPERT/2').
bad_protocol('ZARA-EXPERT/12').
bad_protocol('ZARA-EXPERT').
bad_protocol('zara-expert/1').
bad_protocol('ZARA-RUNTIME/1').

% --- selectable semantics (mirrors zara/experts.py) -----------------------------

test(selectable_experts_are_compatible_and_selectable_availability) :-
    findall(Id, expert_contract:selectable_expert(Id), Ids),
    Ids == ['zara:expert/android-troubleshooting', 'zara:expert/todo'].

test(absent_expert_is_never_selectable) :-
    \+ expert_contract:selectable_expert('zara:expert/prolog-rlm').

test(incompatible_major_expert_is_never_selectable) :-
    \+ expert_contract:selectable_expert('zara:expert/future').

test(installed_availability_is_selectable,
     [setup(temporary_expert('zara:expert/tmp-installed', installed)),
      cleanup(retract_temporary_expert('zara:expert/tmp-installed'))]) :-
    expert_contract:selectable_expert('zara:expert/tmp-installed').

test(unavailable_availability_is_not_selectable,
     [setup(temporary_expert('zara:expert/tmp-unavailable', unavailable)),
      cleanup(retract_temporary_expert('zara:expert/tmp-unavailable'))]) :-
    \+ expert_contract:selectable_expert('zara:expert/tmp-unavailable').

temporary_expert(Id, Availability) :-
    assertz(expert_contract:expert_descriptor(Id, '9.9.9', zara, symbolic,
        'sha256:tmp.v1')),
    assertz(expert_contract:expert_descriptor_protocol(Id, 'ZARA-EXPERT/1')),
    assertz(expert_contract:expert_availability(Id, Availability)).

retract_temporary_expert(Id) :-
    retractall(expert_contract:expert_descriptor(Id, _, _, _, _)),
    retractall(expert_contract:expert_descriptor_protocol(Id, _)),
    retractall(expert_contract:expert_availability(Id, _)).

% --- activation transitions ------------------------------------------------------

test(activation_transition_table_is_exact) :-
    findall(From-To,
        expert_contract:expert_activation_transition(From, To), Pairs),
    Pairs == [inactive-activating, activating-active, activating-failed,
              active-draining, active-unavailable, active-failed,
              draining-inactive, unavailable-active, failed-inactive].

test(reverse_activation_transition_fails) :-
    \+ expert_contract:expert_activation_transition(active, activating).

test(unknown_lifecycle_state_has_no_transitions) :-
    \+ expert_contract:expert_activation_transition(dormant, _).

% --- closed vocabularies ---------------------------------------------------------

test(verdicts_form_the_closed_seven) :-
    findall(Verdict, expert_contract:expert_verdict(Verdict), Verdicts),
    Verdicts == [succeeded, failed, unknown, blocked, unsupported,
                 cancelled, error].

test(error_codes_mirror_python_declaration_order) :-
    findall(Code, expert_contract:expert_error_code(Code), Codes),
    Codes == [invalid_input, ambiguity, unsupported_operation,
              unsupported_backend, incompatible_protocol, denied,
              approval_required, stale_generation, unavailable,
              deadline_exceeded, budget_exceeded, cancelled, interrupted,
              unknown_external_outcome].

% --- flat catalog projection -----------------------------------------------------

test(catalog_rows_are_flat_sorted_projection) :-
    findall(Id-Version-Kind-Availability,
        expert_contract:expert_catalog_row(Id, Version, Kind, Availability),
        Rows),
    Rows == ['zara:expert/android-troubleshooting'-'0.3.0'-hybrid-available,
             'zara:expert/future'-'2.0.0'-symbolic-ready,
             'zara:expert/prolog-rlm'-'0.9.1'-service-absent,
             'zara:expert/todo'-'1.0.0'-symbolic-ready].

% --- pure keyword matching -------------------------------------------------------

test(match_picks_unique_todo_winner_with_sorted_reason) :-
    expert_contract:expert_match('todos milk', Id, Reason),
    Id == 'zara:expert/todo',
    Reason == [todo].

test(match_android_multi_keyword_scoring) :-
    expert_contract:expert_match('Android build and wear failures', Id, Reason),
    Id == 'zara:expert/android-troubleshooting',
    Reason == [android, build, wear].

test(match_prefix_token_rule_mirrors_python) :-
    expert_contract:expert_match('wearos pairing broken', Id, Reason),
    Id == 'zara:expert/android-troubleshooting',
    Reason == [wear].

test(match_tie_fails_closed_and_reports_ambiguity) :-
    Task = 'todos android',
    \+ expert_contract:expert_match(Task, _, _),
    expert_contract:expert_match_ambiguous(Task, Ids),
    Ids == ['zara:expert/android-troubleshooting', 'zara:expert/todo'].

test(match_without_keywords_fails) :-
    \+ expert_contract:expert_match('quantum chess', _, _),
    \+ expert_contract:expert_match_ambiguous('quantum chess', _).

test(absent_expert_keywords_never_match) :-
    \+ expert_contract:expert_match('prolog reasoning service', _, _).

test(match_requires_atom_task) :-
    \+ expert_contract:expert_match(42, _, _).

% --- deterministic corpus emission -----------------------------------------------

test(corpus_lines_are_sorted_ground_fact_lines) :-
    expert_contract:expert_corpus_lines(Lines),
    Lines \== [],
    sort(Lines, Sorted),
    Sorted == Lines,
    forall(member(Line, Lines),
           ( ground(Line),
             sub_atom(Line, _, 1, 0, '.') )).

test(corpus_line_count_matches_registry_facts) :-
    expert_contract:expert_corpus_lines(Lines),
    findall(Id, expert_contract:expert_descriptor(Id, _, _, _, _), Descriptors),
    findall(Operation, expert_contract:expert_operation(_, Operation, _, _),
        Operations),
    findall(Keyword, expert_contract:expert_keyword(_, Keyword), Keywords),
    length(Lines, Count),
    length(Descriptors, DescriptorCount),
    length(Operations, OperationCount),
    length(Keywords, KeywordCount),
    Count =:= DescriptorCount + OperationCount + KeywordCount.

test(corpus_pins_one_ground_fact_line_per_kind) :-
    expert_contract:expert_corpus_lines(Lines),
    memberchk('expert_descriptor(\'zara:expert/todo\',\'1.0.0\',zara,symbolic,\'sha256:todo.expert.v1\').', Lines),
    memberchk('expert_operation(\'zara:expert/todo\',\'route.diagnose\',[],[]).', Lines),
    memberchk('expert_keyword(\'zara:expert/todo\',todo).', Lines).

% --- validated alias assertion ----------------------------------------------------

test(alias_assert_publishes_binding,
     [cleanup(retractall(expert_contract:expert_alias(publish, _, _)))]) :-
    expert_contract:assert_expert_alias(publish, android,
        'zara:expert/android-troubleshooting'),
    expert_contract:expert_alias(publish, android,
        'zara:expert/android-troubleshooting').

test(alias_same_binding_is_idempotent,
     [cleanup(retractall(expert_contract:expert_alias(idem, _, _)))]) :-
    expert_contract:assert_expert_alias(idem, todo, 'zara:expert/todo'),
    expert_contract:assert_expert_alias(idem, todo, 'zara:expert/todo'),
    findall(Id, expert_contract:expert_alias(idem, todo, Id), Ids),
    Ids == ['zara:expert/todo'].

test(alias_duplicate_binding_rejected,
     [setup(expert_contract:assert_expert_alias(dup, todo, 'zara:expert/todo')),
      cleanup(retractall(expert_contract:expert_alias(dup, _, _))),
      throws(error(domain_error(zara_expert_alias, _), _))]) :-
    expert_contract:assert_expert_alias(dup, todo,
        'zara:expert/android-troubleshooting').

test(alias_to_unknown_expert_rejected,
     [throws(error(domain_error(zara_expert_alias, _), _))]) :-
    expert_contract:assert_expert_alias(scope, ghost, 'zara:expert/nonexistent').

:- end_tests(expert_contract).
