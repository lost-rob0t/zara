% ======================================================================
% FILE: server_main.pl
% ======================================================================
% Server-safe Prolog boot (issue #158).
%
% Consulted by the server-side plan engine. This boot loads ONLY the intent
% vocabulary, the capability/provider selection layer, the server
% api_service registry, and the config loader. It never consults
% kb/device_providers.pl, so Linux shell mappings can never become server
% execution targets, and it loads user configuration with the server scope:
% server-inappropriate facts fail the boot loudly.
%
% kb/config.pl is deliberately NOT consulted: its module name collides with
% any other kb_config definition in the same SWI process (observed as load
% errors and pyswip crashes when pytest mixes both), and the server plan
% flow needs no semantic defaults. User semantic overrides ride
% load_server_config, which asserts them into an auto-created kb_config.

:- module(server_main, []).

:- use_module('kb/intents').
:- use_module('kb/server_providers').
:- use_module('modules/capability_plans').
:- use_module('modules/config_loader').

:- initialization(config_loader:load_server_config).
