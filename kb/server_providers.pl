% ======================================================================
% FILE: kb/server_providers.pl
% ======================================================================
% Server api_service provider registry (issue #158).
%
% This is server provider CONFIGURATION: what the RuntimeHost-owned
% Python registry registers as reachable server execution services.
% Capability properties and argument bindings stay in kb/capabilities.pl;
% this file only decides which declared server providers the host offers.
%
% Flat portable subset only (atoms + integers; no lists, no dicts) so the
% registry reads cleanly through pyswip and stays Trealla-compatible:
%   api_service_registry_version(Version)
%   api_service_provider(ProviderId, Kind, TimeoutSeconds)
%
% Desktop-hardware server providers (open_desktop, screen_server,
% pause_server) are declared in kb/capabilities.pl but deliberately NOT
% registered here: a headless server never becomes the execution target
% for desktop commands.

:- module(kb_server_providers, [
    api_service_registry_version/1,
    api_service_provider/3
]).

api_service_registry_version(1).

api_service_provider(search_server, builtin, 5).
api_service_provider(timer_server, builtin, 5).
api_service_provider(admin_restart, builtin, 5).
