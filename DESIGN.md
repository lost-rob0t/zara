---
name: "Zara Desktop — Signal Cabin"
description: "A native semantic theme system where one visible route carries conversation and configuration."
colors:
  ground: "#0A1012"
  panel-deep: "#0D1518"
  panel: "#111A1E"
  panel-lift: "#172226"
  line: "#2A393E"
  line-strong: "#3C5358"
  text: "#F2E9D8"
  text-muted: "#A8B7B3"
  primary: "#61D095"
  primary-hover: "#7ADDA8"
  primary-deep: "#17382B"
  on-primary: "#0A1012"
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
  section-title:
    fontFamily: "Adwaita Sans"
    fontSize: "21px"
    fontWeight: 700
  body:
    fontFamily: "Adwaita Sans"
    fontSize: "14px"
    fontWeight: 400
  control:
    fontFamily: "Adwaita Sans"
    fontSize: "14px"
    fontWeight: 600
  editor:
    fontFamily: "Hack Nerd Font Mono"
    fontSize: "13px"
    fontWeight: 400
  telemetry:
    fontFamily: "Hack Nerd Font Mono"
    fontSize: "11px"
    fontWeight: 700
    letterSpacing: "1px"
rounded:
  none: "0px"
  lamp: "4px"
  checkbox: "5px"
  code: "7px"
  control: "9px"
  field: "10px"
  composer-action: "12px"
  composer: "15px"
spacing:
  trace: "4px"
  control: "8px"
  regular: "10px"
  section: "12px"
  rail: "14px"
  frame: "18px"
  page: "24px"
  workspace: "28px"
components:
  button-primary:
    backgroundColor: "{colors.primary}"
    textColor: "{colors.on-primary}"
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
  button-danger:
    backgroundColor: "transparent"
    textColor: "{colors.danger}"
    typography: "{typography.control}"
    rounded: "{rounded.control}"
    padding: "0 13px"
    height: "34px"
  composer-action-send:
    backgroundColor: "{colors.primary}"
    textColor: "{colors.on-primary}"
    rounded: "{rounded.composer-action}"
    height: "38px"
    width: "38px"
  composer-action-stop:
    backgroundColor: "{colors.danger}"
    textColor: "{colors.text}"
    rounded: "{rounded.composer-action}"
    height: "38px"
    width: "38px"
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
  rail-item-selected:
    backgroundColor: "{colors.primary-deep}"
    textColor: "{colors.text}"
    typography: "{typography.body}"
    rounded: "{rounded.control}"
    padding: "10px 12px"
  source-editor:
    backgroundColor: "{colors.panel-deep}"
    textColor: "{colors.text}"
    typography: "{typography.editor}"
    rounded: "{rounded.field}"
    padding: "9px 11px"
  theme-preview:
    backgroundColor: "{colors.ground}"
    textColor: "{colors.text}"
    typography: "{typography.control}"
    rounded: "{rounded.field}"
    height: "72px"
    width: "112px"
  route-transcript:
    backgroundColor: "transparent"
    textColor: "{colors.text}"
    typography: "{typography.body}"
    rounded: "{rounded.none}"
    padding: "9px 5px 11px 12px"
---

# Design System: Zara Desktop — Signal Cabin

## Overview

**Creative North Star: "Signal Cabin"**

Signal Cabin is Zara's native Linux operating shell: a precise route of status rails, open work fields, and grounded controls. Its charcoal enamel, warm type, and illuminated signals remain the default identity even as complete alternate themes reskin the same semantic component system.

Conversation and configuration share one grammar. Quick Copilot, Full Chat, and Settings expose real runtime or file state without generic AI-card chrome, decorative effects, or invented capability cues.

**Key Characteristics:**

- One semantic component system across five complete themes.
- Continuous route-divider transcripts and rail-based navigation.
- One platform-standard composer action that changes from send to stop.
- Open native forms, a real source editor, and a guided managed-fact rail.

## Colors

The frontmatter is the default Signal Cabin instance. Every shipped theme supplies the same fifteen semantic roles: ground, three panel levels, two line strengths, primary and its states, text and muted text, active, danger, and danger-deep.

### Primary

- **Route Signal** (`primary`): focus, selection, links, ready or complete state, and decisive actions.
- **Lit Route Signal** (`primary-hover`): the primary hover response.
- **Occupied Route** (`primary-deep`): selected rail and history rows.
- **Signal Ink** (`on-primary`): text and icons placed on primary.

### Secondary

- **Active Signal** (`active`): starting, thinking, tool-running, pending, and streaming work.

### Tertiary

- **Danger Signal** (`danger`): stop, error, and disconnected state.
- **Danger Well** (`danger-deep`): contained error surfaces and danger hover.

### Neutral

- **Ground** (`ground`): the uninterrupted application field.
- **Deep Panel** (`panel-deep`): rails, fields, editors, and recessed surfaces.
- **Panel** (`panel`): runtime rails, disabled controls, and quiet hover states.
- **Lifted Panel** (`panel-lift`): controls, composer beds, menus, and tooltips.
- **Line** (`line`): ordinary one-pixel borders and dividers.
- **Strong Line** (`line-strong`): route spines, composer outlines, and split boundaries.
- **Text** (`text`): primary reading and control text.
- **Muted Text** (`text-muted`): provider detail, hints, placeholders, and inactive navigation.

### Complete Theme Registry

- **Signal Cabin:** charcoal enamel, warm ivory, mint primary, amber activity, and signal red.
- **Dotfiles Outrun:** deep violet surfaces, electric cyan primary, amber activity, and rose danger.
- **Nord:** polar-night surfaces, frost-blue primary, pale amber activity, and muted red danger.
- **Dracula:** ink-violet surfaces, terminal-green primary, pale-yellow activity, and bright red danger.
- **ChatGPT Neutral:** white and soft-gray surfaces, conversational green primary, ochre activity, and accessible red danger.

**The Role, Not Hue Rule.** Components consume semantic roles, never theme-specific color literals; switching theme must reskin every open Zara surface without changing behavior or hierarchy.

## Typography

**Display Font:** Nimbus Sans Narrow
**Body Font:** Adwaita Sans
**Label/Mono Font:** Hack Nerd Font Mono

**Character:** Narrow titles retain Signal Cabin's equipment-label identity. Adwaita Sans keeps controls and long settings forms native, while Hack Nerd Font Mono separates runtime telemetry and editable source from human conversation.

### Hierarchy

- **Title** (700, `title`): conversation and window titles.
- **Brand** (700, `brand`): the spaced uppercase ZARA equipment label.
- **Section Title** (700, `section-title`): Settings task headings.
- **Body** (400, `body`): transcript, forms, descriptions, and feedback.
- **Control** (600, `control`): buttons and command labels.
- **Editor** (400, `editor`): Prolog and canonical TOML source.
- **Telemetry** (700, `telemetry`): runtime state, role, and message lifecycle.

**The Three-Gauge Rule.** Narrow type names the instrument, Adwaita carries human language, and mono reports or edits machine state.

## Layout

Quick Copilot keeps the compact header–status–transcript–composer route at a 680 × 460 default and 480 × 320 minimum. Full Chat adds a 240-pixel history rail at a 980 × 700 default and 760 × 520 minimum. Settings opens at 1120 × 760 with a 900 × 640 minimum, a fixed 190-pixel category rail, one flexible task surface, and persistent save/restart feedback in the footer.

Settings pages use open forms rather than nested cards: 24–28-pixel page insets, 13-pixel form rhythm, and fields that grow with the task surface. The Prolog page becomes a split workspace, giving roughly two thirds to approved source and one third to the managed-fact list.

## Elevation & Depth

The system uses no shadows or gradients. Depth comes from the theme's ground and panel ladder plus one-pixel line boundaries; every palette must preserve readable separation between those roles. Focus is explicit through a primary border, never glow.

**The Tonal Depth Rule.** Surfaces earn depth through semantic panel tone and precise lines, never blur, glass, glow, gradient, or drop shadow.

## Shapes

The form language is restrained and instrument-like: route messages stay square and open, navigation and buttons use a 9-pixel curve, fields and theme previews use 10 pixels, the 38-pixel composer action uses 12 pixels, and the composer bed alone uses the broad 15-pixel curve. The status lamp is an 8-pixel rounded square; controls never become pills or chat bubbles.

## Components

### Buttons

- **Primary:** Primary with on-primary content; hover uses primary-hover and focus changes the border to primary.
- **Secondary:** Lifted panel with a strong-line border; hover steps to line.
- **Danger:** Transparent with danger text and border; hover fills danger-deep.
- **Disabled:** Panel, muted text, and line; danger remains transparent.

### Inputs / Fields

Text, choice, number, and multiline fields use deep panel, text, line, and a 9–10-pixel curve. Focus changes only the border to primary. Checkboxes use a 17-pixel native indicator filled with primary when checked.

### Navigation

Conversation history, Settings categories, and managed facts are rails rather than card stacks. Rows begin transparent with muted text, move to panel on hover, and use primary-deep with text when selected. Settings keeps Appearance, Assistant, Voice & Speech, Tools & Privacy, Prolog, and Advanced visible in one fixed rail.

### Unified Composer Action

The composer owns one 38 × 38 control. At rest it uses Qt's platform-standard upward-arrow icon with the accessible name “Send message”; during an active turn the same control becomes Qt's platform-standard stop square, changes to danger, and announces “Stop generating.” No text glyph or Unicode icon substitutes for the native icon.

### Theme Preview

Each preview is a 112 × 72 painted sample of its entire palette: theme ground, lifted panel, text label, and primary/active/danger lamps. Checked or keyboard-focused previews receive a two-pixel primary outline; previewing updates every open surface immediately, while persistence remains an explicit save action.

### Route Transcript

Messages remain one continuous trace. A strong-line spine holds the sequence, each turn starts with a line, user turns switch that line and role label to primary, and lifecycle telemetry changes between active, primary, and danger in place.

### Source Editor & Managed Facts

The Prolog workspace places an allowlisted, no-wrap monospaced source editor beside the managed-fact rail. Syntax highlighting follows the active theme; the adjacent list offers Add, Edit, and Delete through native controls, while footer feedback truthfully states whether content reloaded, needs restart, failed validation, or was restored.

## Do's and Don'ts

### Do:

- **Do** resolve every component through the shared semantic roles in all five themes.
- **Do** preserve visible contrast, primary focus borders, and accessible control names in every palette.
- **Do** use Qt platform-standard icons for send and stop while keeping text labels in tooltips and accessibility metadata.
- **Do** keep Settings as one category rail, one open task surface, and one truthful save/restart footer.
- **Do** keep approved source and managed facts visibly connected to the real configuration path.

### Don't:

- **Don't** recolor only the accent while leaving panels, lines, text, or state feedback from another theme.
- **Don't** add gradients, glass, ambient glow, drop shadows, or nested preference cards.
- **Don't** use Unicode arrows, squares, or other text glyphs as UI icons.
- **Don't** hide restart requirements, failed validation, restoration, or live-reload state behind generic success copy.
- **Don't** break the continuous route transcript into generic assistant cards or bubbles.
