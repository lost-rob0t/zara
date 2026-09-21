:- module(kb_desktop_design,
    [ supported_theme/1,
      desktop_role_rule/2,
      design_theme_provenance/2
    ]).

supported_theme(outrun).
supported_theme(starintel).
supported_theme(midnight).
supported_theme(terminal).
supported_theme(light).

design_theme_provenance(
    Theme,
    'android/shared-ui/src/main/java/ai/zara/ui/theme/ZaraTheme.kt'
) :-
    supported_theme(Theme).

desktop_role_rule(ground, direct(background)).
desktop_role_rule(panel_deep, direct(surfaceInput)).
desktop_role_rule(panel, direct(surface)).
desktop_role_rule(panel_lift, direct(surfaceElevated)).
desktop_role_rule(line, direct(border)).
desktop_role_rule(line_strong, direct(borderActive)).
desktop_role_rule(text, direct(text)).
desktop_role_rule(text_muted, direct(textMuted)).
desktop_role_rule(primary, direct(primary)).
desktop_role_rule(primary_hover, direct(focus)).
desktop_role_rule(primary_deep, direct(ambientGlow)).
desktop_role_rule(on_primary, contrast_text(primary)).
desktop_role_rule(active, direct(warning)).
desktop_role_rule(danger, direct(error)).
desktop_role_rule(danger_deep, mix(surfaceInput, error, 18)).
