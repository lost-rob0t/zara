% Zara Android portable theme contract.
%
% The model intentionally mirrors Emacs themes:
%   - zara_theme/2              ~= deftheme
%   - zara_theme_parent/2       ~= derived theme / inheritance
%   - zara_theme_face/3         ~= custom-theme-set-faces
%   - zara_theme_effective_face ~= resolved face lookup
%
% User theme files stay ordinary Prolog. A minimal custom theme can contain:
%
%   zara_theme(my_outrun, 'My Outrun').
%   zara_theme_parent(my_outrun, outrun_oled).
%   zara_theme_face(my_outrun, primary, '#ff2bd6').
%   zara_theme_face(my_outrun, accent_cyan, '#00f5ff').
%
% The Android importer validates the exported facts before turning them into
% Compose semantic tokens. Other Prolog code may coexist in the file; the
% theme contract itself is defined entirely by the predicates below.

zara_theme_contract_version(1).

% Semantic faces understood by the Android renderer.
zara_theme_face_name(background).
zara_theme_face_name(surface).
zara_theme_face_name(surface_elevated).
zara_theme_face_name(surface_input).
zara_theme_face_name(border).
zara_theme_face_name(border_active).
zara_theme_face_name(primary).
zara_theme_face_name(secondary).
zara_theme_face_name(accent_magenta).
zara_theme_face_name(accent_cyan).
zara_theme_face_name(text).
zara_theme_face_name(text_muted).
zara_theme_face_name(success).
zara_theme_face_name(warning).
zara_theme_face_name(error).
zara_theme_face_name(focus).
zara_theme_face_name(ambient_glow).

% Built-in theme metadata. These names are also valid parents for user themes.
zara_theme(outrun, 'Outrun').
zara_theme(outrun_oled, 'Outrun OLED').
zara_theme(starintel, 'StarIntel').
zara_theme(starintel_oled, 'StarIntel OLED').
zara_theme(midnight, 'Midnight').
zara_theme(midnight_oled, 'Midnight OLED').
zara_theme(terminal, 'Terminal').
zara_theme(terminal_oled, 'Terminal OLED').
zara_theme(light, 'Light').

zara_theme_parent(outrun_oled, outrun).
zara_theme_parent(starintel, outrun).
zara_theme_parent(starintel_oled, starintel).
zara_theme_parent(midnight, outrun).
zara_theme_parent(midnight_oled, midnight).
zara_theme_parent(terminal, outrun).
zara_theme_parent(terminal_oled, terminal).
zara_theme_parent(light, outrun).

% Outrun is the complete root palette.
zara_theme_face(outrun, background, '#02040b').
zara_theme_face(outrun, surface, '#07101b').
zara_theme_face(outrun, surface_elevated, '#0a1324').
zara_theme_face(outrun, surface_input, '#080f1e').
zara_theme_face(outrun, border, '#1a2a49').
zara_theme_face(outrun, border_active, '#775cff').
zara_theme_face(outrun, primary, '#e21cf2').
zara_theme_face(outrun, secondary, '#16d9ff').
zara_theme_face(outrun, accent_magenta, '#f000ff').
zara_theme_face(outrun, accent_cyan, '#00d7ff').
zara_theme_face(outrun, text, '#eaf2ff').
zara_theme_face(outrun, text_muted, '#8d9dba').
zara_theme_face(outrun, success, '#6ce7a6').
zara_theme_face(outrun, warning, '#ffd166').
zara_theme_face(outrun, error, '#ff6b8b').
zara_theme_face(outrun, focus, '#b56dff').
zara_theme_face(outrun, ambient_glow, '#3a0d5e').

% StarIntel overrides.
zara_theme_face(starintel, background, '#080807').
zara_theme_face(starintel, surface, '#14130f').
zara_theme_face(starintel, surface_elevated, '#201d15').
zara_theme_face(starintel, surface_input, '#10100d').
zara_theme_face(starintel, border, '#4c4329').
zara_theme_face(starintel, border_active, '#e8c56a').
zara_theme_face(starintel, primary, '#e8c56a').
zara_theme_face(starintel, secondary, '#f1da9a').
zara_theme_face(starintel, accent_magenta, '#d4af37').
zara_theme_face(starintel, accent_cyan, '#f1da9a').
zara_theme_face(starintel, text, '#f8f3e6').
zara_theme_face(starintel, text_muted, '#beb5a1').
zara_theme_face(starintel, focus, '#ffd971').
zara_theme_face(starintel, ambient_glow, '#342a10').

% Midnight overrides.
zara_theme_face(midnight, background, '#080919').
zara_theme_face(midnight, surface, '#11132a').
zara_theme_face(midnight, surface_elevated, '#1b1d3c').
zara_theme_face(midnight, surface_input, '#0d1024').
zara_theme_face(midnight, border, '#343b68').
zara_theme_face(midnight, border_active, '#9c92ff').
zara_theme_face(midnight, primary, '#b3a4ff').
zara_theme_face(midnight, secondary, '#8abfff').
zara_theme_face(midnight, accent_magenta, '#b3a4ff').
zara_theme_face(midnight, accent_cyan, '#8abfff').
zara_theme_face(midnight, focus, '#cec4ff').
zara_theme_face(midnight, ambient_glow, '#24204e').

% Terminal overrides.
zara_theme_face(terminal, background, '#030805').
zara_theme_face(terminal, surface, '#08120c').
zara_theme_face(terminal, surface_elevated, '#102117').
zara_theme_face(terminal, surface_input, '#050d08').
zara_theme_face(terminal, border, '#294e36').
zara_theme_face(terminal, border_active, '#8ef0a8').
zara_theme_face(terminal, primary, '#8ef0a8').
zara_theme_face(terminal, secondary, '#adebc0').
zara_theme_face(terminal, accent_magenta, '#8ef0a8').
zara_theme_face(terminal, accent_cyan, '#adebc0').
zara_theme_face(terminal, text, '#e3f8e9').
zara_theme_face(terminal, text_muted, '#9cbba6').
zara_theme_face(terminal, focus, '#bfffcc').
zara_theme_face(terminal, ambient_glow, '#12321d').

% Light overrides.
zara_theme_face(light, background, '#f7f7fa').
zara_theme_face(light, surface, '#ffffff').
zara_theme_face(light, surface_elevated, '#ececf3').
zara_theme_face(light, surface_input, '#f2f2f7').
zara_theme_face(light, border, '#b8bac8').
zara_theme_face(light, border_active, '#6450a8').
zara_theme_face(light, primary, '#7a247d').
zara_theme_face(light, secondary, '#006478').
zara_theme_face(light, accent_magenta, '#88258c').
zara_theme_face(light, accent_cyan, '#006478').
zara_theme_face(light, text, '#1c2030').
zara_theme_face(light, text_muted, '#555c70').
zara_theme_face(light, success, '#17623b').
zara_theme_face(light, warning, '#765100').
zara_theme_face(light, error, '#ac2044').
zara_theme_face(light, focus, '#6034a0').
zara_theme_face(light, ambient_glow, '#eae1f3').

% OLED derivatives override only large-area surfaces and glow.
zara_theme_face(outrun_oled, background, '#000000').
zara_theme_face(outrun_oled, surface, '#010101').
zara_theme_face(outrun_oled, surface_elevated, '#050505').
zara_theme_face(outrun_oled, surface_input, '#020202').
zara_theme_face(outrun_oled, ambient_glow, '#00000000').

zara_theme_face(starintel_oled, background, '#000000').
zara_theme_face(starintel_oled, surface, '#010101').
zara_theme_face(starintel_oled, surface_elevated, '#050505').
zara_theme_face(starintel_oled, surface_input, '#020202').
zara_theme_face(starintel_oled, ambient_glow, '#00000000').

zara_theme_face(midnight_oled, background, '#000000').
zara_theme_face(midnight_oled, surface, '#010101').
zara_theme_face(midnight_oled, surface_elevated, '#050505').
zara_theme_face(midnight_oled, surface_input, '#020202').
zara_theme_face(midnight_oled, ambient_glow, '#00000000').

zara_theme_face(terminal_oled, background, '#000000').
zara_theme_face(terminal_oled, surface, '#010101').
zara_theme_face(terminal_oled, surface_elevated, '#050505').
zara_theme_face(terminal_oled, surface_input, '#020202').
zara_theme_face(terminal_oled, ambient_glow, '#00000000').

% Resolve local override first, then walk the parent chain. This is the same
% semantic idea as an Emacs theme inheriting faces from its base theme.
zara_theme_effective_face(Theme, Face, Value) :-
    zara_theme_face(Theme, Face, Value), !.
zara_theme_effective_face(Theme, Face, Value) :-
    zara_theme_parent(Theme, Parent),
    zara_theme_effective_face(Parent, Face, Value).

zara_theme_complete(Theme) :-
    zara_theme(Theme, _),
    \+ (zara_theme_face_name(Face), \+ zara_theme_effective_face(Theme, Face, _)).
