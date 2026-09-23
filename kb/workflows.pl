% Default workflow knowledge base.
%
% These facts are declarative only. They describe useful assistant workflows,
% triggers, ordered typed-intent steps, and cross-device handoff preferences.
% They do NOT execute shell commands, Android intents, or device operations.
% Runtime execution must continue through Zara's canonical capability/provider
% and programmable-symbol authority.
%
% The interaction model intentionally follows proven assistant UX patterns:
% short voice aliases, multi-step routines, event/time triggers, and explicit
% cross-device continuation. No proprietary Siri/Alexa implementation is copied.

:- module(kb_workflows, [
    workflow_version/1,
    workflow/5,
    workflow_alias/2,
    workflow_trigger/4,
    workflow_condition/4,
    workflow_action/6,
    workflow_handoff/4,
    workflow_surface/1,
    workflow_target/1,
    resolve_workflow/2,
    workflow_plan/2,
    workflow_actions_for_executor/3
]).

workflow_version(1).

workflow_surface(linux).
workflow_surface(android).
workflow_surface(wear).
workflow_surface(server).

workflow_target(any).
workflow_target(best).
workflow_target(initiator).
workflow_target(all).
workflow_target(linux).
workflow_target(android).
workflow_target(wear).
workflow_target(server).

% workflow(Id, Title, Category, Activation, Description).
% Activation is manual | suggested | opt_in. "opt_in" is discoverable but must
% not become an ambient event automation merely because this KB was loaded.

workflow(morning_briefing, "Morning briefing", daily, manual,
    "Weather, agenda, priorities, notifications, and device state in one brief.").
workflow(bedtime, "Bedtime", daily, manual,
    "Summarize tomorrow, quiet devices, preserve active context, and lock down.").
workflow(leaving_home, "Leaving home", presence, opt_in,
    "Portable-device check, commute context, reminders, and desktop handoff.").
workflow(arriving_home, "Arriving home", presence, opt_in,
    "Restore home context and offer continuation on the best nearby surface.").
workflow(commute_start, "Start commute", travel, manual,
    "Route, travel-time, media, and portable task context.").
workflow(focus_start, "Start focus", productivity, manual,
    "Enable focus state, surface the next task, and reduce interruption noise.").
workflow(focus_stop, "Stop focus", productivity, manual,
    "Leave focus state and summarize what changed while focused.").
workflow(workday_start, "Start workday", productivity, manual,
    "Open work context, agenda, priorities, and current project state.").
workflow(workday_end, "End workday", productivity, manual,
    "Capture loose ends, summarize work, and prepare the next work block.").
workflow(meeting_prep, "Meeting prep", productivity, manual,
    "Gather the next meeting, participants, related notes, and open actions.").
workflow(quick_capture, "Quick capture", capture, manual,
    "Capture a thought from any surface and persist it into the canonical inbox.").
workflow(remember_this, "Remember this", memory, manual,
    "Store current context as a typed memory with provenance.").
workflow(whats_next, "What's next", productivity, manual,
    "Return the next useful action from agenda, todos, and active context.").
workflow(find_phone, "Find phone", device, manual,
    "Locate and ring the paired Android device from another Zara surface.").
workflow(find_watch, "Find watch", device, manual,
    "Locate and signal the paired Wear device when reachable.").
workflow(announce_everywhere, "Announce everywhere", communication, manual,
    "Send one announcement to eligible Zara surfaces.").
workflow(continue_on_desktop, "Continue on desktop", continuity, manual,
    "Move the current conversation/task context to Linux.").
workflow(continue_on_phone, "Continue on phone", continuity, manual,
    "Move the current conversation/task context to Android.").
workflow(continue_on_watch, "Continue on watch", continuity, manual,
    "Project a compact continuation of the current task to Wear.").
workflow(send_clipboard_to_desktop, "Send clipboard to desktop", continuity, manual,
    "Transfer the current portable clipboard payload to Linux.").
workflow(send_clipboard_to_phone, "Send clipboard to phone", continuity, manual,
    "Transfer the current portable clipboard payload to Android.").
workflow(media_follow_me, "Media follow me", continuity, manual,
    "Move supported media playback intent to the selected nearby device.").
workflow(timer_follow_me, "Timer follow me", continuity, manual,
    "Mirror an active timer state onto the selected nearby device.").
workflow(home_status, "Home status", home, manual,
    "Summarize known home/device state without changing it.").
workflow(device_status, "Device status", device, manual,
    "Summarize connected Zara devices, reachability, battery, and capabilities.").
workflow(battery_sweep, "Battery sweep", device, manual,
    "Summarize battery state across phone, watch, and Linux hosts.").
workflow(update_check, "Update check", system, manual,
    "Check Zara and host update state without automatically installing anything.").
workflow(reading_mode, "Reading mode", productivity, manual,
    "Reduce interruptions and move reading material to the best display.").
workflow(driving_mode, "Driving mode", travel, manual,
    "Prefer audio/voice surfaces and compact portable interactions.").
workflow(desktop_resume, "Resume desktop session", continuity, manual,
    "Restore the last resumable Linux task and associated conversation context.").
workflow(watch_command_center, "Watch command center", continuity, manual,
    "Expose compact status and safe remote actions for the paired Linux session.").
workflow(phone_command_center, "Phone command center", continuity, manual,
    "Expose richer Linux session status and typed remote actions on Android.").

% Voice aliases: short, natural phrases inspired by mainstream assistant UX.

workflow_alias(good_morning, morning_briefing).
workflow_alias(start_my_day, morning_briefing).
workflow_alias(morning_brief, morning_briefing).
workflow_alias(good_night, bedtime).
workflow_alias(bedtime_mode, bedtime).
workflow_alias(im_leaving, leaving_home).
workflow_alias(leaving, leaving_home).
workflow_alias(im_home, arriving_home).
workflow_alias(home_now, arriving_home).
workflow_alias(commute, commute_start).
workflow_alias(head_to_work, commute_start).
workflow_alias(focus, focus_start).
workflow_alias(deep_work, focus_start).
workflow_alias(stop_focus, focus_stop).
workflow_alias(back_to_normal, focus_stop).
workflow_alias(start_work, workday_start).
workflow_alias(work_mode, workday_start).
workflow_alias(done_working, workday_end).
workflow_alias(wrap_up, workday_end).
workflow_alias(prep_meeting, meeting_prep).
workflow_alias(whats_my_next_meeting, meeting_prep).
workflow_alias(note_this, quick_capture).
workflow_alias(capture_this, quick_capture).
workflow_alias(remember_this, remember_this).
workflow_alias(save_context, remember_this).
workflow_alias(whats_next, whats_next).
workflow_alias(next_task, whats_next).
workflow_alias(find_my_phone, find_phone).
workflow_alias(ring_my_phone, find_phone).
workflow_alias(find_my_watch, find_watch).
workflow_alias(ping_my_watch, find_watch).
workflow_alias(announce, announce_everywhere).
workflow_alias(tell_everywhere, announce_everywhere).
workflow_alias(desktop, continue_on_desktop).
workflow_alias(send_to_desktop, continue_on_desktop).
workflow_alias(continue_on_linux, continue_on_desktop).
workflow_alias(phone, continue_on_phone).
workflow_alias(send_to_phone, continue_on_phone).
workflow_alias(watch, continue_on_watch).
workflow_alias(send_to_watch, continue_on_watch).
workflow_alias(clipboard_to_desktop, send_clipboard_to_desktop).
workflow_alias(clipboard_to_phone, send_clipboard_to_phone).
workflow_alias(move_music, media_follow_me).
workflow_alias(move_timer, timer_follow_me).
workflow_alias(home_status, home_status).
workflow_alias(device_status, device_status).
workflow_alias(batteries, battery_sweep).
workflow_alias(check_updates, update_check).
workflow_alias(reading_mode, reading_mode).
workflow_alias(driving_mode, driving_mode).
workflow_alias(resume_desktop, desktop_resume).
workflow_alias(watch_controls, watch_command_center).
workflow_alias(phone_controls, phone_command_center).

% Triggers. Voice triggers are available immediately as knowledge. Schedule,
% presence, connectivity, and device-state triggers are descriptions for the
% canonical automation runtime; they are not independently scheduled here.

workflow_trigger(morning_briefing, voice, phrase, "good morning").
workflow_trigger(morning_briefing, schedule, local_time, "07:00").
workflow_trigger(bedtime, voice, phrase, "good night").
workflow_trigger(bedtime, schedule, local_time, "22:00").
workflow_trigger(leaving_home, voice, phrase, "I'm leaving").
workflow_trigger(leaving_home, presence, state, left_home).
workflow_trigger(arriving_home, voice, phrase, "I'm home").
workflow_trigger(arriving_home, presence, state, arrived_home).
workflow_trigger(commute_start, voice, phrase, "start my commute").
workflow_trigger(focus_start, voice, phrase, "focus").
workflow_trigger(focus_stop, voice, phrase, "stop focus").
workflow_trigger(workday_start, voice, phrase, "start work").
workflow_trigger(workday_end, voice, phrase, "wrap up").
workflow_trigger(meeting_prep, event, calendar, next_meeting).
workflow_trigger(quick_capture, voice, phrase, "note this").
workflow_trigger(remember_this, voice, phrase, "remember this").
workflow_trigger(whats_next, voice, phrase, "what's next").
workflow_trigger(find_phone, voice, phrase, "find my phone").
workflow_trigger(find_watch, voice, phrase, "find my watch").
workflow_trigger(announce_everywhere, voice, phrase, "announce").
workflow_trigger(continue_on_desktop, voice, phrase, "continue on desktop").
workflow_trigger(continue_on_phone, voice, phrase, "continue on phone").
workflow_trigger(continue_on_watch, voice, phrase, "continue on watch").
workflow_trigger(send_clipboard_to_desktop, voice, phrase, "clipboard to desktop").
workflow_trigger(send_clipboard_to_phone, voice, phrase, "clipboard to phone").
workflow_trigger(media_follow_me, voice, phrase, "move media here").
workflow_trigger(timer_follow_me, voice, phrase, "move timer here").
workflow_trigger(home_status, voice, phrase, "home status").
workflow_trigger(device_status, voice, phrase, "device status").
workflow_trigger(battery_sweep, voice, phrase, "batteries").
workflow_trigger(update_check, voice, phrase, "check updates").
workflow_trigger(reading_mode, voice, phrase, "reading mode").
workflow_trigger(driving_mode, voice, phrase, "driving mode").
workflow_trigger(desktop_resume, voice, phrase, "resume desktop").
workflow_trigger(watch_command_center, manual, surface, wear).
workflow_trigger(phone_command_center, manual, surface, android).

% Conditions are symbolic predicates for the automation layer.

workflow_condition(leaving_home, device, reachable, android).
workflow_condition(arriving_home, device, reachable, linux).
workflow_condition(find_phone, device, reachable, android).
workflow_condition(find_watch, device, reachable, wear).
workflow_condition(continue_on_desktop, device, reachable, linux).
workflow_condition(continue_on_phone, device, reachable, android).
workflow_condition(continue_on_watch, device, reachable, wear).
workflow_condition(send_clipboard_to_desktop, device, reachable, linux).
workflow_condition(send_clipboard_to_phone, device, reachable, android).
workflow_condition(watch_command_center, device, paired, wear).
workflow_condition(phone_command_center, device, paired, android).

% workflow_action(Workflow, Sequence, Target, Namespace, Name, Arguments).
% Arguments remain inert typed data. The target is an execution preference,
% never authority.

workflow_action(morning_briefing, 10, server, weather, current, []).
workflow_action(morning_briefing, 20, server, calendar, agenda, [arg(window, today)]).
workflow_action(morning_briefing, 30, server, todo, brief, [arg(limit, 5)]).
workflow_action(morning_briefing, 40, server, notification, summary, [arg(window, overnight)]).
workflow_action(morning_briefing, 50, best, device, status, [arg(scope, nearby)]).
workflow_action(morning_briefing, 60, initiator, conversation, present_brief, []).

workflow_action(bedtime, 10, server, calendar, agenda, [arg(window, tomorrow)]).
workflow_action(bedtime, 20, server, todo, capture_loose_ends, []).
workflow_action(bedtime, 30, all, device, quiet_mode, [arg(enabled, true)]).
workflow_action(bedtime, 40, server, context, checkpoint, [arg(scope, active)]).
workflow_action(bedtime, 50, linux, device, lock, []).

workflow_action(leaving_home, 10, android, device, portable_readiness, []).
workflow_action(leaving_home, 20, server, calendar, next_destination, []).
workflow_action(leaving_home, 30, server, todo, location_reminders, [arg(location, away)]).
workflow_action(leaving_home, 40, linux, context, checkpoint, [arg(scope, active)]).
workflow_action(leaving_home, 50, android, handoff, context, [arg(source, linux)]).

workflow_action(arriving_home, 10, server, home, status, []).
workflow_action(arriving_home, 20, server, context, resumable, []).
workflow_action(arriving_home, 30, best, handoff, offer_resume, [arg(prefer, linux)]).

workflow_action(commute_start, 10, server, calendar, next_destination, []).
workflow_action(commute_start, 20, android, navigation, route, [arg(source, next_destination)]).
workflow_action(commute_start, 30, android, media, resume, [arg(mode, audio)]).
workflow_action(commute_start, 40, server, todo, brief, [arg(filter, portable)]).

workflow_action(focus_start, 10, all, device, focus_mode, [arg(enabled, true)]).
workflow_action(focus_start, 20, server, todo, next_action, []).
workflow_action(focus_start, 30, linux, context, open_active_project, []).
workflow_action(focus_start, 40, initiator, notification, reduce_interruptions, []).

workflow_action(focus_stop, 10, all, device, focus_mode, [arg(enabled, false)]).
workflow_action(focus_stop, 20, server, notification, summary, [arg(window, focus_session)]).
workflow_action(focus_stop, 30, server, context, summarize_session, []).

workflow_action(workday_start, 10, server, calendar, agenda, [arg(window, workday)]).
workflow_action(workday_start, 20, server, todo, brief, [arg(filter, work)]).
workflow_action(workday_start, 30, linux, context, open_active_project, []).
workflow_action(workday_start, 40, linux, system, workspace_status, []).

workflow_action(workday_end, 10, server, context, summarize_session, [arg(scope, work)]).
workflow_action(workday_end, 20, server, todo, capture_loose_ends, [arg(scope, work)]).
workflow_action(workday_end, 30, server, calendar, agenda, [arg(window, next_workday)]).
workflow_action(workday_end, 40, linux, context, checkpoint, [arg(scope, work)]).

workflow_action(meeting_prep, 10, server, calendar, next_meeting, []).
workflow_action(meeting_prep, 20, server, context, related_documents, [arg(source, next_meeting)]).
workflow_action(meeting_prep, 30, server, todo, related_actions, [arg(source, next_meeting)]).
workflow_action(meeting_prep, 40, initiator, conversation, present_brief, [arg(kind, meeting)]).

workflow_action(quick_capture, 10, initiator, context, capture_current, []).
workflow_action(quick_capture, 20, server, todo, inbox_capture, [arg(source, current_context)]).

workflow_action(remember_this, 10, initiator, context, capture_current, []).
workflow_action(remember_this, 20, server, memory, remember, [arg(source, current_context)]).

workflow_action(whats_next, 10, server, calendar, next_commitment, []).
workflow_action(whats_next, 20, server, todo, next_action, []).
workflow_action(whats_next, 30, server, context, active_task, []).
workflow_action(whats_next, 40, initiator, conversation, choose_next, []).

workflow_action(find_phone, 10, server, device, locate, [arg(target, android)]).
workflow_action(find_phone, 20, android, device, ring, [arg(reason, find_device)]).

workflow_action(find_watch, 10, server, device, locate, [arg(target, wear)]).
workflow_action(find_watch, 20, wear, device, signal, [arg(reason, find_device)]).

workflow_action(announce_everywhere, 10, all, communication, announce, [arg(message, required)]).

workflow_action(continue_on_desktop, 10, initiator, context, checkpoint, [arg(scope, active)]).
workflow_action(continue_on_desktop, 20, linux, handoff, context, [arg(source, initiator)]).
workflow_action(continue_on_desktop, 30, linux, conversation, resume, [arg(source, initiator)]).

workflow_action(continue_on_phone, 10, initiator, context, checkpoint, [arg(scope, active)]).
workflow_action(continue_on_phone, 20, android, handoff, context, [arg(source, initiator)]).
workflow_action(continue_on_phone, 30, android, conversation, resume, [arg(source, initiator)]).

workflow_action(continue_on_watch, 10, initiator, context, checkpoint, [arg(scope, compact)]).
workflow_action(continue_on_watch, 20, wear, handoff, context, [arg(source, initiator), arg(mode, compact)]).
workflow_action(continue_on_watch, 30, wear, conversation, resume, [arg(mode, compact)]).

workflow_action(send_clipboard_to_desktop, 10, initiator, clipboard, read_portable, []).
workflow_action(send_clipboard_to_desktop, 20, linux, clipboard, write_portable, [arg(source, initiator)]).

workflow_action(send_clipboard_to_phone, 10, initiator, clipboard, read_portable, []).
workflow_action(send_clipboard_to_phone, 20, android, clipboard, write_portable, [arg(source, initiator)]).

workflow_action(media_follow_me, 10, initiator, media, snapshot, []).
workflow_action(media_follow_me, 20, best, handoff, media, [arg(source, initiator)]).
workflow_action(media_follow_me, 30, best, media, resume, [arg(source, handoff)]).

workflow_action(timer_follow_me, 10, server, timer, active, []).
workflow_action(timer_follow_me, 20, best, handoff, timer, [arg(source, server)]).
workflow_action(timer_follow_me, 30, best, timer, mirror, []).

workflow_action(home_status, 10, server, home, status, []).
workflow_action(home_status, 20, server, device, status, [arg(scope, home)]).
workflow_action(home_status, 30, initiator, conversation, present_brief, [arg(kind, home_status)]).

workflow_action(device_status, 10, server, device, inventory, [arg(scope, zara)]).
workflow_action(device_status, 20, server, device, capability_summary, []).
workflow_action(device_status, 30, initiator, conversation, present_brief, [arg(kind, device_status)]).

workflow_action(battery_sweep, 10, server, device, battery_summary, [arg(scope, all)]).
workflow_action(battery_sweep, 20, initiator, conversation, present_brief, [arg(kind, battery)]).

workflow_action(update_check, 10, server, system, update_check, [arg(scope, zara)]).
workflow_action(update_check, 20, linux, system, update_check, [arg(scope, host)]).
workflow_action(update_check, 30, android, system, update_check, [arg(scope, app)]).
workflow_action(update_check, 40, wear, system, update_check, [arg(scope, app)]).
workflow_action(update_check, 50, initiator, conversation, present_brief, [arg(kind, updates)]).

workflow_action(reading_mode, 10, all, device, focus_mode, [arg(enabled, true), arg(profile, reading)]).
workflow_action(reading_mode, 20, best, handoff, document, [arg(source, current_context)]).
workflow_action(reading_mode, 30, best, display, reading_surface, []).

workflow_action(driving_mode, 10, android, device, interaction_profile, [arg(profile, driving)]).
workflow_action(driving_mode, 20, wear, device, interaction_profile, [arg(profile, glance_only)]).
workflow_action(driving_mode, 30, server, conversation, output_profile, [arg(profile, voice_first)]).

workflow_action(desktop_resume, 10, server, context, resumable, [arg(target, linux)]).
workflow_action(desktop_resume, 20, linux, handoff, context, [arg(source, server)]).
workflow_action(desktop_resume, 30, linux, conversation, resume, [arg(source, checkpoint)]).

workflow_action(watch_command_center, 10, server, device, status, [arg(target, linux)]).
workflow_action(watch_command_center, 20, wear, ui, command_center, [arg(target, linux), arg(mode, compact)]).

workflow_action(phone_command_center, 10, server, device, status, [arg(target, linux)]).
workflow_action(phone_command_center, 20, android, ui, command_center, [arg(target, linux), arg(mode, rich)]).

% Explicit continuity preferences. Modes describe data shape, not transport.
workflow_handoff(continue_on_desktop, initiator, linux, full_context).
workflow_handoff(continue_on_phone, initiator, android, full_context).
workflow_handoff(continue_on_watch, initiator, wear, compact_context).
workflow_handoff(send_clipboard_to_desktop, initiator, linux, portable_clipboard).
workflow_handoff(send_clipboard_to_phone, initiator, android, portable_clipboard).
workflow_handoff(media_follow_me, initiator, best, media_state).
workflow_handoff(timer_follow_me, server, best, timer_state).
workflow_handoff(leaving_home, linux, android, portable_context).
workflow_handoff(arriving_home, server, linux, resumable_context).

resolve_workflow(Name, Id) :-
    atom(Name),
    workflow(Name, _, _, _, _),
    Id = Name,
    !.
resolve_workflow(Alias, Id) :-
    workflow_alias(Alias, Id).

workflow_plan(Id, Actions) :-
    workflow(Id, _, _, _, _),
    findall(
        Seq-action(Target, Namespace, Name, Arguments),
        workflow_action(Id, Seq, Target, Namespace, Name, Arguments),
        Pairs
    ),
    keysort(Pairs, Sorted),
    strip_sequences(Sorted, Actions).

% Project only targets that are already concrete for this executor. Dynamic
% `initiator` and `best` targets deliberately remain unresolved in workflow_plan/2
% so the canonical runtime can bind them using authenticated surface/session
% context instead of this inert KB guessing an execution peer.
workflow_actions_for_executor(Id, Executor, Actions) :-
    workflow_surface(Executor),
    workflow_plan(Id, Plan),
    filter_actions_for_executor(Plan, Executor, Actions).

filter_actions_for_executor([], _, []).
filter_actions_for_executor([Action|Rest], Executor, Actions) :-
    ( action_for_executor(Executor, Action)
    -> Actions = [Action|Kept],
       filter_actions_for_executor(Rest, Executor, Kept)
    ;  filter_actions_for_executor(Rest, Executor, Actions)
    ).

action_for_executor(Executor, action(Target, _, _, _)) :-
    ( Target == Executor
    ; Target == any
    ; Target == all
    ).

strip_sequences([], []) .
strip_sequences([_-Action|Rest], [Action|Actions]) :-
    strip_sequences(Rest, Actions).
