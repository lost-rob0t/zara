---
name: "Zara Desktop — Signal Cabin"
description: "A native conversation instrument where every request follows one visible route."
colors:
  ground: "#0A1012"
  panel-deep: "#0D1518"
  panel: "#111A1E"
  panel-lift: "#172226"
  line: "#2A393E"
  line-strong: "#3C5358"
  text: "#F2E9D8"
  text-muted: "#A8B7B3"
  ready: "#61D095"
  ready-hover: "#7ADDA8"
  ready-deep: "#17382B"
  active: "#E7B84B"
  danger: "#E6544D"
  danger-deep: "#562727"
typography:
  title:
    fontFamily: "Nimbus Sans Narrow"
    fontSize: "23px"
    fontWeight: 700
  brand:
    fontFamily: "Nimbus Sans Narrow"
    fontSize: "19px"
    fontWeight: 700
    letterSpacing: "3px"
  body:
    fontFamily: "Adwaita Sans"
    fontSize: "14px"
    fontWeight: 400
  control:
    fontFamily: "Adwaita Sans"
    fontSize: "14px"
    fontWeight: 600
  telemetry:
    fontFamily: "Hack Nerd Font Mono"
    fontSize: "11px"
    fontWeight: 700
    letterSpacing: "1px"
rounded:
  none: "0px"
  lamp: "4px"
  code: "7px"
  control: "9px"
  field: "10px"
  composer: "15px"
spacing:
  trace: "4px"
  compact: "6px"
  control: "8px"
  regular: "10px"
  section: "12px"
  frame: "18px"
  conversation: "22px"
components:
  button-primary:
    backgroundColor: "{colors.ready}"
    textColor: "{colors.ground}"
    typography: "{typography.control}"
    rounded: "{rounded.control}"
    padding: "0 13px"
    height: "34px"
  button-primary-hover:
    backgroundColor: "{colors.ready-hover}"
    textColor: "{colors.ground}"
    typography: "{typography.control}"
    rounded: "{rounded.control}"
    padding: "0 13px"
    height: "34px"
  button-secondary:
    backgroundColor: "{colors.panel-lift}"
    textColor: "{colors.text}"
    typography: "{typography.control}"
    rounded: "{rounded.control}"
    padding: "0 13px"
    height: "34px"
  button-secondary-hover:
    backgroundColor: "{colors.line}"
    textColor: "{colors.text}"
    typography: "{typography.control}"
    rounded: "{rounded.control}"
    padding: "0 13px"
    height: "34px"
  button-danger:
    backgroundColor: "transparent"
    textColor: "{colors.danger}"
    typography: "{typography.control}"
    rounded: "{rounded.control}"
    padding: "0 13px"
    height: "34px"
  button-danger-hover:
    backgroundColor: "{colors.danger-deep}"
    textColor: "{colors.text}"
    typography: "{typography.control}"
    rounded: "{rounded.control}"
    padding: "0 13px"
    height: "34px"
  input-field:
    backgroundColor: "{colors.panel-deep}"
    textColor: "{colors.text}"
    typography: "{typography.body}"
    rounded: "{rounded.field}"
    padding: "9px 11px"
  runtime-rail:
    backgroundColor: "{colors.panel}"
    textColor: "{colors.text}"
    typography: "{typography.body}"
    rounded: "{rounded.field}"
    padding: "8px 12px"
  composer-shell:
    backgroundColor: "{colors.panel-lift}"
    textColor: "{colors.text}"
    typography: "{typography.body}"
    rounded: "{rounded.composer}"
    padding: "8px"
  message-route:
    backgroundColor: "transparent"
    textColor: "{colors.text}"
    typography: "{typography.body}"
    rounded: "{rounded.none}"
    padding: "9px 5px 11px 12px"
  error-panel:
    backgroundColor: "{colors.danger-deep}"
    textColor: "{colors.text}"
    typography: "{typography.body}"
    rounded: "{rounded.control}"
    padding: "9px 11px"
---

# Design System: Zara Desktop — Signal Cabin

## Overview

**Creative North Star: "Signal Cabin"**

Signal Cabin treats Zara as an operational conversation instrument: charcoal enamel grounds the workspace, warm ivory carries the exchange, and small illuminated signals report runtime truth. The atmosphere is calm, exact, and native to a Linux desktop rather than theatrical or ornamental.

One conversation follows one visible route from Quick Copilot into Full Chat. Status anchors the top, the transcript owns the field, and the composer forms a grounded control bed; the system explicitly rejects generic AI-card chrome, decorative gradients, and fabricated access cues.

**Key Characteristics:**

- Continuous route-divider transcript instead of message cards.
- Compact status rails with restrained, fixed signal semantics.
- The same material and state language at Quick and Full working scales.
- Native Qt density, keyboard focus, and honest runtime copy.

## Colors

The palette reads as a charcoal control board with warm enamel lettering and three illuminated operational signals.

### Primary

- **Route Mint** (`ready`): readiness, completed state, user-route emphasis, links, selection, focus, and the primary action.
- **Lit Mint** (`ready-hover`): the brighter hover response for the primary action only.
- **Occupied Green** (`ready-deep`): selected conversation history without turning the sidebar into a bright accent field.

### Secondary

- **Working Amber** (`active`): starting, thinking, tool-running, pending, and streaming states.

### Tertiary

- **Signal Red** (`danger`): stop controls, errors, and disconnected states.
- **Stop Well** (`danger-deep`): the contained background for error feedback and the danger hover state.

### Neutral

- **Cabin Black** (`ground`): the continuous application field and code-editor ground.
- **Recessed Charcoal** (`panel-deep`): sidebar, fields, and code-block recesses.
- **Enamel Panel** (`panel`): runtime rail, disabled controls, and secondary hover surfaces.
- **Raised Enamel** (`panel-lift`): composer shell, controls, and tooltips.
- **Track Line** (`line`): ordinary dividers, borders, and message boundaries.
- **Route Divider** (`line-strong`): the transcript spine, composer outline, code enclosure, and stronger control boundaries.
- **Warm Ivory** (`text`): primary reading and control text.
- **Weathered Sage** (`text-muted`): provider, runtime detail, placeholders, inactive history, and secondary metadata.

**The Signal Semantics Rule.** Mint means ready or complete, amber means work in progress, and red means stop or failure; these meanings never drift between surfaces.

## Typography

**Display Font:** Nimbus Sans Narrow
**Body Font:** Adwaita Sans
**Label/Mono Font:** Hack Nerd Font Mono

**Character:** Condensed titles evoke enamel cabin labels without becoming nostalgic decoration. Adwaita Sans keeps conversation and controls native and highly readable, while the monospaced telemetry face makes route state feel measured and exact.

### Hierarchy

- **Title** (700, `title`): conversation and window titles.
- **Brand** (700, `brand`): the spaced uppercase ZARA wordmark used as a compact equipment label.
- **Body** (400, `body`): transcript text, provider detail, placeholders, and supporting copy.
- **Control** (600, `control`): buttons and interactive command labels.
- **Telemetry** (700, `telemetry`): runtime state, message role, and message lifecycle status.

**The Three-Gauge Rule.** Narrow type names the instrument, Adwaita carries human language, and mono reports machine state; do not swap those roles for novelty.

## Layout

The desktop surfaces use one vertical route: header, status rail, transcript, composer. Quick Copilot is a compact always-on-top tool at a default 680 × 460 and a 480 × 320 minimum; it shows the six most recent messages and keeps the composer between 48 and 80 pixels high. Full Chat starts at 980 × 700 with a 760 × 520 minimum, adds a 240-pixel history rail, and gives the conversation the flexible pane.

The outer rhythm is compact and regular: 10–12-pixel gaps inside rails and message sequences, 16–22-pixel frame margins, and 8-pixel composer insets. Conversation content stretches with the window; history remains narrow, and the transcript keeps an unbroken left spine rather than reflowing into cards.

## Elevation & Depth

Signal Cabin uses no shadows. Depth is structural: Cabin Black is the field, Recessed Charcoal sits behind inputs and the history rail, Enamel Panel marks status and disabled controls, and Raised Enamel forms the composer and buttons. One-pixel Track Line and Route Divider borders make the hierarchy legible without floating surfaces.

**The Tonal Depth Rule.** A surface earns depth through charcoal tone and a precise border, never through blur, glow, glass, or drop shadow.

## Shapes

The silhouette is restrained and instrument-like. Route messages remain square and open (`none`), the status lamp is a compact rounded square (`lamp`), embedded code uses a small inset corner (`code`), buttons and alerts use gently curved controls (`control`), fields and rails use a stable enclosure (`field`), and the composer alone receives the broadest grounded curve (`composer`). Borders stay one pixel; pills and fully rounded chat bubbles do not belong to this system.

## Components

### Buttons

- **Shape:** Gently curved rectangular controls (`control`) with a 34-pixel minimum height and compact horizontal padding.
- **Primary:** Route Mint on Cabin Black; used for Send and New chat.
- **Hover / Focus:** Primary brightens to Lit Mint; all controls focus by changing the border to Route Mint.
- **Secondary:** Raised Enamel with a Route Divider border; hover moves one tone to Track Line.
- **Danger:** Transparent with Signal Red text and border; hover fills the Stop Well and restores Warm Ivory text.
- **Disabled:** Enamel Panel, Weathered Sage text, and Track Line border; disabled danger remains transparent.

### Cards / Containers

- **Runtime Rail:** Enamel Panel, Track Line border, stable field curve, and an 8-pixel lamp whose hue tracks runtime state.
- **Code Block:** Recessed Charcoal with a Route Divider outline; the editor falls back to Cabin Black and monospaced type.
- **Error Panel:** Stop Well with a Signal Red border and Warm Ivory copy.
- **Shadow Strategy:** None; use the tonal depth hierarchy.

### Inputs / Fields

- **Style:** Recessed Charcoal, Warm Ivory text, Track Line border, stable field curve, and compact inset padding.
- **Focus:** The border alone changes to Route Mint.
- **Composer:** A transparent multiline editor inside a Raised Enamel control bed with a Route Divider outline and the system's broadest curve.
- **Keyboard:** Enter submits, Shift+Enter inserts a newline, and Escape hides Quick Copilot.

### Navigation

The Full Chat history is a fixed left rail separated by one Track Line. Items are borderless rows with Weathered Sage text; hover reveals Enamel Panel, while the current conversation uses Occupied Green and Warm Ivory. Quick Copilot exposes New chat and Full chat as secondary controls in its header so the same trace can move to the durable surface without a visual reset.

### Route Transcript

Messages are not cards. A continuous Route Divider spine holds the transcript while each turn begins with a horizontal track line; user turns switch that line and role label to Route Mint. Role and lifecycle sit in monospaced telemetry above the body, and status color changes in place as work progresses.

## Do's and Don'ts

### Do:

- **Do** keep Quick Copilot and Full Chat visibly made from the same materials, tokens, and route grammar.
- **Do** reserve saturated color for runtime meaning, selection, focus, and decisive actions.
- **Do** keep the conversation field open and continuous, with role and state attached to the route.
- **Do** preserve native Qt controls, keyboard behavior, readable contrast, and explicit runtime wording.

### Don't:

- **Don't** turn messages, tools, or status into a stack of generic rounded AI cards.
- **Don't** add gradients, glass, ambient glow, drop shadows, or decorative raster texture.
- **Don't** use mint, amber, or red outside their fixed operational meanings.
- **Don't** imply screen, clipboard, file, or application access through decoration or unsupported copy.
