% Capability provider knowledge base (issue #157).
%
% Declarative facts only: what providers exist, their properties, and their
% declared argument schema. Selection logic lives in modules/capability_plans.pl
% (pure; the KB is the single authority on declared capabilities).
%
% capability_provider(IntentNS, IntentName, ProviderId, Priority)
%   Higher Priority wins; equal top priorities resolve to typed ambiguity.
% capability_property(ProviderId, Property)
%   location(server|device)      structural availability source
%   side_effect(none|local|external)   side-effect class carried on the plan
%   requires_auth(none|CapabilityId)   server-granted authorization vocabulary
%                                      (zara/security.py Capability values)
%   capability(CapabilityAtom)         the abstract capability a device must
%                                      advertise to serve a device-location
%                                      provider (server providers ignore ads)
% capability_binding(ProviderId, ArgName, SlotName)
%   Declared argument schema: binds a provider argument to a frame slot name.
%
% No shell strings, no executable names, no package ids: concrete launch
% targets are adapter/platform state, never KB or plan content.

:- module(kb_capabilities, [
    capability_provider/4,
    capability_property/2,
    capability_binding/3
]).

% --- app opening (contract examples 6/13) ------------------------------------
% Device provider outranks the desktop provider: the initiating device wins
% when it advertises the capability; the desktop alias gate is the fallback.

capability_provider(app, open, open_app, 100).
capability_property(open_app, location(device)).
capability_property(open_app, side_effect(external)).
capability_property(open_app, requires_auth(none)).
capability_property(open_app, capability('app.open')).
capability_binding(open_app, app, target).

capability_provider(app, open, open_desktop, 50).
capability_property(open_desktop, location(server)).
capability_property(open_desktop, side_effect(external)).
capability_property(open_desktop, requires_auth(none)).
capability_property(open_desktop, capability('app.open')).
capability_binding(open_desktop, app, target).

% --- web search (contract example 9) ------------------------------------------

capability_provider(web, search, search_server, 100).
capability_property(search_server, location(server)).
capability_property(search_server, side_effect(external)).
capability_property(search_server, requires_auth(none)).
capability_property(search_server, capability('web.search')).
capability_binding(search_server, query, query).

% --- timers (server or device per explicit policy and availability) -----------

capability_provider(device, 'timer.set', timer_server, 100).
capability_property(timer_server, location(server)).
capability_property(timer_server, side_effect(local)).
capability_property(timer_server, requires_auth(none)).
capability_property(timer_server, capability('timer.set')).
capability_binding(timer_server, duration, duration).
capability_binding(timer_server, label, label).

capability_provider(device, 'timer.set', timer_device, 50).
capability_property(timer_device, location(device)).
capability_property(timer_device, side_effect(local)).
capability_property(timer_device, requires_auth(none)).
capability_property(timer_device, capability('timer.set')).
capability_binding(timer_device, duration, duration).
capability_binding(timer_device, label, label).

% --- screenshots (contract example 10) ----------------------------------------

capability_provider(device, 'screen.capture', screen_device, 100).
capability_property(screen_device, location(device)).
capability_property(screen_device, side_effect(local)).
capability_property(screen_device, requires_auth(none)).
capability_property(screen_device, capability('screen.capture')).
capability_binding(screen_device, image, target).

capability_provider(device, 'screen.capture', screen_server, 50).
capability_property(screen_server, location(server)).
capability_property(screen_server, side_effect(local)).
capability_property(screen_server, requires_auth(none)).
capability_property(screen_server, capability('screen.capture')).

% --- server administration (authorization independent of authentication) ------

capability_provider(skill, 'admin.restart', admin_restart, 100).
capability_property(admin_restart, location(server)).
capability_property(admin_restart, side_effect(external)).
capability_property(admin_restart, requires_auth('daemon.admin')).
capability_property(admin_restart, capability('daemon.admin')).

% --- media control (equal-priority tie pin for deterministic ambiguity) -------

capability_provider(media, pause, pause_device, 100).
capability_property(pause_device, location(device)).
capability_property(pause_device, side_effect(none)).
capability_property(pause_device, requires_auth(none)).
capability_property(pause_device, capability('media.pause')).

capability_provider(media, pause, pause_server, 100).
capability_property(pause_server, location(server)).
capability_property(pause_server, side_effect(none)).
capability_property(pause_server, requires_auth(none)).
capability_property(pause_server, capability('media.pause')).
