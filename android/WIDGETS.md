# Zara Android home-screen widgets

Zara ships three resizeable widgets that use the frozen Android hierarchy and the same semantic theme source as the phone and Wear clients:

- **Symbolic Assistant** — compact entry into chat, voice, and logic.
- **Runtime** — last known bounded local/remote lifecycle state; it never invents a live connection.
- **Quick Actions** — explicit routes into Zara surfaces.

Outrun remains the default. StarIntel, Midnight, Terminal, Light, and System resolve through `shared-ui` semantic tokens. Widget-specific styling lives entirely in valid Prolog. Android XML supplies only the fixed `RemoteViews` accessibility/view skeleton.

## Import and export

Open **Themes → Home-screen widgets** to import, export, or reset a `.pl` stylesheet. An import is compiled and validated before atomically replacing the current style. A rejected import leaves the last green stylesheet untouched. Export produces the exact active Prolog source; when no custom source is installed, it exports the built-in source from `app/src/main/assets/prolog/widget_styles.pl`.

The stylesheet is data-only Prolog. Directives, rules, unknown predicates, duplicate properties, arbitrary Android intents, reflection, shell operations, and unrestricted goals are rejected.

## Complete stylesheet surface

Every file contains one version and one style:

```prolog
zara_widget_stylesheet(1).
widget_style(my_style).
widget_theme(my_style, current).
```

`widget_theme/2` accepts `current`, `outrun`, `starintel`, `midnight`, `terminal`, `light`, or `system`.

Selectors are `all`, `assistant`, `runtime`, and `actions`. A specific selector overrides `all` for only that widget.

```prolog
widget_color(my_style, all, background, semantic(background)).
widget_color(my_style, assistant, border, '#FFE21CF2').
widget_color(my_style, runtime, surface, '#CC07101B').
```

Colors accept `#RRGGBB`, `#AARRGGBB`, or these semantic roles:

`background`, `surface`, `surface_elevated`, `border`, `border_active`, `primary`, `secondary`, `accent_magenta`, `accent_cyan`, `text`, `text_muted`, `success`, `warning`, `error`, `focus`, `ambient_glow`.

Widget color properties are:

`background`, `surface`, `border`, `title`, `body`, `label`, `status`, `action_background`, `action_text`, `sigil`.

All dimensions and typography are also Prolog facts:

```prolog
widget_metric(my_style, all, outer_padding_dp, 16).
widget_metric(my_style, all, content_gap_dp, 6).
widget_metric(my_style, all, corner_radius_dp, 20).
widget_metric(my_style, all, border_width_dp, 1).
widget_metric(my_style, all, title_sp, 16).
widget_metric(my_style, all, body_sp, 12).
widget_metric(my_style, all, label_sp, 10).
widget_metric(my_style, all, action_sp, 11).
widget_metric(my_style, all, sigil_size_dp, 34).
widget_metric(my_style, all, action_corner_radius_dp, 12).
```

Copy, visibility, alignment, and safe action routing are controlled the same way:

```prolog
widget_text(my_style, assistant, eyebrow, 'LOCAL · PRIVATE').
widget_text(my_style, assistant, title, 'Symbolic intelligence').
widget_text(my_style, assistant, subtitle, 'On your terms').
widget_text(my_style, assistant, primary_label, 'CHAT').
widget_text(my_style, assistant, secondary_label, 'VOICE').
widget_text(my_style, assistant, tertiary_label, 'LOGIC').

widget_flag(my_style, assistant, show_sigil, true).
widget_flag(my_style, assistant, show_subtitle, true).
widget_flag(my_style, assistant, show_status, true).
widget_flag(my_style, assistant, show_secondary, true).
widget_flag(my_style, assistant, show_tertiary, true).

widget_alignment(my_style, assistant, start).
widget_action(my_style, assistant, primary, chat).
widget_action(my_style, assistant, secondary, voice).
widget_action(my_style, assistant, tertiary, logic).
```

Alignment accepts `start`, `center`, or `end`. Routes are closed over `chat`, `logic`, `voice`, `remote`, `diagnostics`, and `themes`. Resize behavior may hide lower-priority material when the launcher grants too little room; that keeps content legible and touch targets usable without changing the imported style.
