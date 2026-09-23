% Zara Android home-screen widgets. This is the complete styling source.
% Import a replacement .pl from Themes; no Kotlin, XML, or APK rebuild is needed.
zara_widget_stylesheet(1).
widget_style(zara_default).
widget_theme(zara_default, current).

widget_color(zara_default, all, background, semantic(background)).
widget_color(zara_default, all, surface, semantic(surface)).
widget_color(zara_default, all, border, semantic(border)).
widget_color(zara_default, all, title, semantic(text)).
widget_color(zara_default, all, body, semantic(text_muted)).
widget_color(zara_default, all, label, semantic(accent_cyan)).
widget_color(zara_default, all, status, semantic(success)).
widget_color(zara_default, all, action_background, semantic(surface_elevated)).
widget_color(zara_default, all, action_text, semantic(text)).
widget_color(zara_default, all, sigil, semantic(accent_magenta)).

widget_metric(zara_default, all, outer_padding_dp, 16).
widget_metric(zara_default, all, content_gap_dp, 6).
widget_metric(zara_default, all, corner_radius_dp, 20).
widget_metric(zara_default, all, border_width_dp, 1).
widget_metric(zara_default, all, title_sp, 16).
widget_metric(zara_default, all, body_sp, 12).
widget_metric(zara_default, all, label_sp, 10).
widget_metric(zara_default, all, action_sp, 11).
widget_metric(zara_default, all, sigil_size_dp, 34).
widget_metric(zara_default, all, action_corner_radius_dp, 12).

widget_text(zara_default, assistant, eyebrow, 'LOCAL · PRIVATE · EXTENSIBLE').
widget_text(zara_default, assistant, title, 'Symbolic intelligence').
widget_text(zara_default, assistant, subtitle, 'On your terms').
widget_text(zara_default, assistant, primary_label, 'CHAT').
widget_text(zara_default, assistant, secondary_label, 'VOICE').
widget_text(zara_default, assistant, tertiary_label, 'LOGIC').

widget_text(zara_default, runtime, eyebrow, 'ZARA RUNTIME').
widget_text(zara_default, runtime, title, 'Runtime').
widget_text(zara_default, runtime, subtitle, 'Last known device state').
widget_text(zara_default, runtime, primary_label, 'REMOTE').
widget_text(zara_default, runtime, secondary_label, 'DIAGNOSTICS').
widget_text(zara_default, runtime, tertiary_label, 'LOGIC').

widget_text(zara_default, actions, eyebrow, 'QUICK ACTIONS').
widget_text(zara_default, actions, title, 'Open Zara').
widget_text(zara_default, actions, subtitle, 'Bounded routes into the symbolic workspace').
widget_text(zara_default, actions, primary_label, 'CHAT').
widget_text(zara_default, actions, secondary_label, 'VOICE').
widget_text(zara_default, actions, tertiary_label, 'LOGIC').

widget_flag(zara_default, all, show_sigil, true).
widget_flag(zara_default, all, show_subtitle, true).
widget_flag(zara_default, all, show_status, true).
widget_flag(zara_default, all, show_secondary, true).
widget_flag(zara_default, all, show_tertiary, true).
widget_alignment(zara_default, all, start).

widget_action(zara_default, all, primary, chat).
widget_action(zara_default, all, secondary, voice).
widget_action(zara_default, all, tertiary, logic).
widget_action(zara_default, runtime, primary, remote).
widget_action(zara_default, runtime, secondary, diagnostics).
