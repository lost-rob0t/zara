% ======================================================================
% FILE: server_main.pl
% ======================================================================
% Server-safe Prolog boot (issue #158).
%
% Consulted by the server-side plan engine. This boot loads ONLY shared
% semantic configuration, the intent vocabulary, and the capability/
% provider selection layer plus the server api_service registry. It never
% consults kb/device_providers.pl, so Linux shell mappings can never
% become server execution targets, and it loads user configuration with
% the server scope: server-inappropriate facts fail the boot loudly.

:- module(server_main, []).

:- use_module('kb/config').
:- use_module('kb/intents').
:- use_module('kb/server_providers').
:- use_module('modules/capability_plans').
:- use_module('modules/config_loader').

:- initialization(config_loader:load_server_config).
