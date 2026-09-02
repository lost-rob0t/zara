#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

swipl -q -g "
    use_module('modules/command_loop.pl'),
    use_module('modules/hooks_loader.pl'),
    current_predicate(command_loop:advice_command_loop/3),
    current_predicate(command_loop:command_handler/2),

    assertz(command_loop:command_handler(rage_override, [ok])),

    hooks_loader:set_hook_policy(false, false),
    \+ command_loop:advice_command_loop(rage_override, [ok], _),

    hooks_loader:set_hook_policy(true, false),
    \+ command_loop:advice_command_loop(rage_override, [ok], _),

    hooks_loader:set_hook_policy(true, true),
    command_loop:advice_command_loop(rage_override, [ok], Success),
    Success = command_result(success, rage_override, [ok], none),

    assertz((command_loop:command_handler(rage_fail, [_]) :- fail)),
    \+ command_loop:advice_command_loop(rage_fail, [x], _),

    assertz((command_loop:command_handler(rage_throw, [_]) :- throw(error(rage_test_failure, context(command_handler/2, test))))),
    command_loop:advice_command_loop(rage_throw, [x], Failure),
    Failure = command_result(failure, rage_throw, [x], exception(error(rage_test_failure, _))),

    hooks_loader:reset_hook_policy,
    halt
" -t "halt(1)"

echo "Prolog command advice tests passed."
