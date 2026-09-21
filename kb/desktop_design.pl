:- module(kb_desktop_design,
    [ supported_theme/1,
      desktop_role_binding/2,
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

desktop_role_binding(ground, background).
desktop_role_binding(panel_deep, surfaceInput).
desktop_role_binding(panel, surface).
desktop_role_binding(panel_lift, surfaceElevated).
desktop_role_binding(line, border).
desktop_role_binding(line_strong, borderActive).
desktop_role_binding(text, text).
desktop_role_binding(text_muted, textMuted).
desktop_role_binding(primary, primary).
desktop_role_binding(primary_hover, focus).
desktop_role_binding(primary_deep, ambientGlow).
desktop_role_binding(on_primary, primary).
desktop_role_binding(active, warning).
desktop_role_binding(danger, error).
desktop_role_binding(danger_deep, surfaceInput).
