# Zarathushtra Pets

Zarathushtra Pets are optional animated desktop companions that reflect the
live state of your Zara runtime — model generation, tool execution, agent
work, and so on — so you can follow activity at a glance without opening the
console.

## What pets are

A pet is a small frameless, transparent, always-on-top window that plays a
sprite-sheet animation. The animation changes as Zara's runtime state
changes. Pets are **data, not code**: a pet package is a manifest plus a
sprite image, with no executable scripts.

Pets are built natively in Python with PySide6/Qt6. They do not require
Electron, Node.js, or ChatGPT to be running.

## Enabling and disabling

Open the Pets settings dialog:

    zara --pets-settings

Toggle **Enabled**, choose a pet, set scale and reduced-motion preference,
and save. To launch the overlay directly:

    zara --pets

To tuck the pet away, right-click it and choose **Tuck Away**, or disable
Pets in the settings dialog. Your selection and window position persist
across restarts.

## State meanings

The pet derives a single aggregate state from all live runtime activities.
When several activities overlap, the highest-priority state wins.

| State | Meaning | Priority |
|-------|---------|----------|
| `needs-input` | Waiting for approval, confirmation, permission, clarification, or a user response | 4 (highest) |
| `blocked` | Fatal failure, provider unavailable, tool failure needing intervention | 3 |
| `ready` | Operation finished with unseen/unread output | 2 |
| `running` | Model generation, streaming, tool/agent execution, indexing, search, background task | 1 |
| `idle` | No active operation | 0 |

The priority policy is centralized in `zara/pets/state.py` and unit-tested.

## Runtime mapping

| Runtime activity | Pet state |
|------------------|-----------|
| Prolog executes a command | (no pet event; handled inline) |
| Agent conversation starts | `running` |
| LLM model call | `running` |
| Streaming tokens | `running` |
| Tool execution | `running` |
| Tool fails | `blocked` |
| Model fails / provider unavailable | `blocked` |
| Work completes with output | `ready` |
| User reads/views output | back to `idle` |
| Task cancelled | re-derived from remaining activities (not forced idle) |

## Importing pets

### Import a Zarathushtra-native pet

A native pet is a directory with `pet.json` and a sprite image:

    pets/
      my-pet/
        pet.json
        spritesheet.png

Use **Import Pet...** in the settings dialog and select the `pet.json`.

### Import a ChatGPT/Codex pet

Use **Import ChatGPT Pet...** and select a compatible sprite file. Two
formats are supported:

- **V1 (web upload)**: a single transparent PNG or WebP, exactly
  **1536×1872** pixels, 8 columns × 9 rows of 192×208 cells, ≤ 20 MiB.
- **V2 (desktop package)**: a `pet.json` plus a sprite sheet
  (`1536×2288`, 8 columns × 11 rows). The `pet.json` must contain
  `"spriteVersionNumber": 2`.

The importer inspects file magic numbers (not extensions), validates
dimensions and size, copies the asset into Zarathushtra-managed storage,
and writes a normalized native manifest. The original ChatGPT file is
never modified.

### Import from ChatGPT (discovery)

**Import from ChatGPT...** scans documented local application-data
locations (read-only) for ChatGPT/Codex pets and lists them for one-click
import. Scanned locations:

- `~/.codex/pets/`
- `~/.config/ChatGPT/pets/` (Linux)
- `~/Library/Application Support/ChatGPT/pets/` (macOS)
- `%APPDATA%/ChatGPT/pets/` (Windows)

Discovery never modifies, deletes, or depends on ChatGPT running. If the
ChatGPT storage layout changes in the future, discovery degrades
gracefully to manual import.

## Storage locations

Managed pet packages live under:

    $XDG_DATA_HOME/zarathushtra/pets/<pet-id>/
      pet.json
      spritesheet.png   # or .webp

Persisted runtime state (selected pet, position, scale, reduced-motion,
enabled) lives under:

    $XDG_CONFIG_HOME/zarathushtra/pet-state.json

Static defaults (enabled, scale, reduced-motion) are read from the
`[pets]` section of `config.toml`.

Removing an imported pet deletes **only** Zarathushtra's managed copy. The
original ChatGPT source asset is never touched.

## Zarathushtra pet manifest specification

A pet package is a directory containing `pet.json` and a sprite image.
The manifest is forward-compatible: a `version` field gates schema
evolution, and unknown keys are rejected (not silently dropped) so
future versions fail loudly rather than misbehave.

### Fields

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Safe id (`^[a-z0-9][a-z0-9-]{0,63}$`); used as the directory name |
| `name` | string | Display name |
| `version` | int | Manifest schema version (currently `1`) |
| `source` | string | Provenance label (e.g. `native`, `chatgpt-v1`) |
| `source_format` | string | One of `native`, `chatgpt-v1`, `chatgpt-v2` |
| `sprite_asset` | string | Sprite filename within the package (no path traversal) |
| `frame_geometry` | object | `{width, height, columns, rows}` of one sprite cell |
| `animations` | list | Named animations: `{name, row, frames, fps, loop}` |
| `anchor` | `[x, y]` | Anchor point (defaults to bottom-center) |
| `scale` | float | Default display scale |
| `metadata` | object | Free-form provenance/extra data |

### Animations

State animations are `idle`, `running`, `needs-input`, `ready`, and
`blocked`. ChatGPT/Codex imports also retain `drag`, `drag-left`, `wave`, and
`jump`. V2 pets use rows 9 and 10 for the 16 clockwise look directions, so
the pet follows the pointer while idle. Clicking plays `wave`; dragging uses
the directional run rows; dropping plays `jump`.

The Pets settings dialog also controls the assistant display name. It
defaults to `Zara` independently of the selected artwork package.

No executable scripts, hooks, or network references are permitted in a
pet package. Pet files are untrusted data.

## Creating custom pets

Create a sprite sheet as a PNG or WebP with 192×208 cells arranged in a
grid. For full ChatGPT compatibility, use the V1 geometry (8 columns × 9
rows = 1536×1872). Then write a `pet.json` with the five state animations
mapped to rows of your choice. Import the package via the settings dialog.

The bundled `scripts/generate-pet-fixtures.py` script generates synthetic
fixtures you can use as a geometry reference.

## Reduced motion

Pets respect the OS reduced-motion setting (GNOME `enable-animations`
gsettings, or `GTK_DISABLE_ANIMATIONS`). When reduced motion is enabled,
the pet renders a representative still frame instead of animating. You can
also force this with the **Reduced Motion** setting (`on`/`off`/`system`).

## Platform caveats

### Linux

- **Wayland**: transparent frameless always-on-top windows work on most
  compositors (KWin, Mutter) but may be restricted by some (Sway
  historically limited `WindowStaysOnTopHint`). Dragging may be
  server-side decorated on some compositors.
- **X11**: full support for transparency, always-on-top, cross-monitor
  dragging, and all-workspace visibility. On qtile the pet uses an unmanaged
  overlay window so it is not assigned to a single group.
- **Click-through on transparent regions**: not reliable cross-platform.
  The overlay is interactive on the whole window rectangle; transparent
  regions still intercept clicks on most compositors. The drag-threshold
  logic distinguishes clicks from drags; right-click opens the context
  menu. This is documented as a known limitation.
- **System tray**: not currently implemented; the pet uses its own
  window + context menu instead of a tray icon.

### macOS / Windows

The overlay code is Qt-based and should run, but only Linux is tested in
CI. Tray integration is not wired up.

## Troubleshooting

- **Pet does not appear**: run `zara --pets-settings` and ensure a pet is
  selected and Enabled is checked; then run `zara --pets`.
- **Imported pet does not animate**: check that the sprite dimensions match
  V1 (1536×1872) or V2 (1536×2288). The importer rejects mismatched
  dimensions.
- **Position resets on restart**: the saved position is validated against
  current monitors; if the saved monitor disappeared the pet falls back
  to the primary screen.
- **PySide6 not found**: install with `nix develop` (PySide6 is in
  `flake.nix`) or `pip install pyside6`.

## Compatibility limitations

- V1 pets do not include the V2 pointer-look rows, but retain all nine
  standard state, drag, wave, and jump rows.
- Only PNG and WebP are accepted; other image formats are rejected at
  import.

## External format/version caveats

ChatGPT/Codex's pet format may change. The importer is adapter-based so a
new format can be added without touching the core, but the current V1/V2
mappings are based on the public docs and the community-published V2
package structure at the time of writing. If ChatGPT changes its format,
imports of the old format continue to work; the new format will need a new
adapter.
