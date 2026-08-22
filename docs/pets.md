# Zarathushtra Pets

Zarathushtra Pets are optional animated desktop companions that show Zara's runtime state without making you keep a console open. They are native PySide6/Qt6 overlays. A Pet package is data, not code: a manifest plus a PNG/WebP sprite sheet, with no executable hooks hiding in the cute thing on your desktop.

**Related docs:** [docs index](index.org) · [feature map](features.org) · [runtime architecture](architecture.org) · [CLI](cli.org) · [literate main config](config/main.org) · [short operational guide](../wiki/pets.org)

## Running Pets

Open settings/import UI:

```sh
zara --pets-settings
```

Launch the overlay:

```sh
zara --pets
```

Wake mode can start the companion too:

```sh
zara --wake --pets
```

The settings dialog controls enabled state, selected Pet, scale, and reduced-motion behavior. Window position and runtime selection persist across restarts. Right-click a Pet and choose **Tuck Away** to hide the overlay without deleting its package.

Static defaults live under `[pets]` in the main TOML. See [`docs/config/main.org`](config/main.org).

## What state a Pet shows

Pets reduce concurrent runtime activity into one visible state. Higher-priority states win while activities overlap.

| State | Meaning | Priority |
| --- | --- | ---: |
| `needs-input` | Waiting for approval, confirmation, permission, clarification, or another user response | 4 |
| `blocked` | Fatal failure, unavailable provider, or failure that needs intervention | 3 |
| `ready` | Work finished with output the user has not viewed | 2 |
| `running` | Model/tool/agent/indexing/search/background work is active | 1 |
| `idle` | No active operation | 0 |

The reducer lives in `zara/pets/state.py` and is unit-tested. Pets are consumers of runtime state; they do not decide whether a command or tool succeeded.

Typical mapping:

| Runtime activity | Pet state |
| --- | --- |
| Prolog command executes inline | no separate Pet activity event |
| Agent conversation/model/tool starts | `running` |
| Tool or provider fails and needs intervention | `blocked` |
| Work completes with unseen output | `ready` |
| User views output | re-derived, normally `idle` |
| Task is cancelled | re-derived from whatever else is still active |

For the provider-neutral event boundary that feeds this state, read [`architecture.org`](architecture.org).

## Importing Pets

### Zara-native packages

A native package is a directory containing `pet.json` and one sprite asset:

```text
pets/
  my-pet/
    pet.json
    spritesheet.png
```

Choose **Import Pet...** in settings and select the manifest.

### ChatGPT/Codex packages

**Import ChatGPT Pet...** accepts the compatible formats Zara currently knows about:

- **V1:** one transparent PNG/WebP, exactly **1536×1872**, arranged as 8 columns × 9 rows of 192×208 cells, at most 20 MiB.
- **V2:** a `pet.json` plus a **1536×2288** sprite sheet, 8 columns × 11 rows. The manifest must report `"spriteVersionNumber": 2`.

The importer checks file magic, dimensions, and size, then copies the asset into Zara-managed storage and writes a normalized native manifest. It never edits the source asset.

**Import from ChatGPT...** can discover compatible packages read-only in known local locations:

- `~/.codex/pets/`
- `~/.config/ChatGPT/pets/` on Linux
- `~/Library/Application Support/ChatGPT/pets/` on macOS
- `%APPDATA%/ChatGPT/pets/` on Windows

Discovery is optional convenience. If an external application changes its directory layout, manual import still works instead of Zara pretending it owns somebody else's storage format.

## Storage

Managed packages:

```text
$XDG_DATA_HOME/zarathushtra/pets/<pet-id>/
  pet.json
  spritesheet.png   # or .webp
```

Runtime Pet state:

```text
$XDG_CONFIG_HOME/zarathushtra/pet-state.json
```

Static fallback/default settings come from `[pets]` in Zara's main config.

Removing an imported Pet deletes only Zara's managed copy. The source ChatGPT/Codex/native asset remains untouched.

## Native manifest format

The current Zara manifest schema version is `1`. Unknown keys are rejected instead of silently ignored, so a future schema mismatch fails visibly rather than producing a surprisingly possessed desktop companion.

| Field | Type | Meaning |
| --- | --- | --- |
| `id` | string | Safe package id matching `^[a-z0-9][a-z0-9-]{0,63}$`; also used as directory name |
| `name` | string | Display name |
| `version` | integer | Zara manifest schema version |
| `source` | string | Provenance label such as `native` or `chatgpt-v1` |
| `source_format` | string | `native`, `chatgpt-v1`, or `chatgpt-v2` |
| `sprite_asset` | string | Package-local sprite filename; path traversal is rejected |
| `frame_geometry` | object | Cell width/height, columns, and rows |
| `animations` | list | `{name, row, frames, fps, loop}` records |
| `anchor` | `[x, y]` | Anchor point; bottom-center by default |
| `scale` | float | Default display scale |
| `metadata` | object | Provenance/extra data |

State animations are `idle`, `running`, `needs-input`, `ready`, and `blocked`. Compatible imports may also retain `drag`, `drag-left`, `wave`, and `jump` animations. V2 packages use rows 9 and 10 for 16 clockwise pointer-look directions. Clicking can play `wave`, dragging uses directional run frames, and dropping can play `jump`.

The assistant display name is configured independently from the selected artwork and defaults to `Zara`.

Pet packages cannot contain executable scripts, hooks, or network references. Treat package files as untrusted data anyway; validation is the boundary, not the artwork's facial expression.

## Creating a custom Pet

Create a PNG or WebP sprite sheet using 192×208 cells. For V1 compatibility, use an 8×9 grid, giving 1536×1872 total pixels. Then create a `pet.json` mapping Zara's five state animations to rows/frames and import the package in settings.

`scripts/generate-pet-fixtures.py` generates synthetic fixtures that are useful as a geometry reference.

## Reduced motion

Pets can follow the system reduced-motion preference or be forced on/off. The Linux path checks GNOME animation settings / `GTK_DISABLE_ANIMATIONS` where available. With reduced motion enabled, Zara displays representative still frames instead of continuously animating.

Main config values are `system`, `on`, or `off`. See [`config/main.org`](config/main.org).

## Linux display behavior

Linux is the tested platform.

### X11

X11 generally provides the strongest behavior for transparency, always-on-top placement, cross-monitor dragging, and all-workspace visibility. On qtile the Pet uses an unmanaged overlay window instead of becoming a normal group-assigned application.

### Wayland

Wayland overlay behavior depends on the compositor. When Zara detects a Wayland session with XWayland available (`DISPLAY` is set), the Pet process can use Qt's `xcb` backend so the main application can stay on Wayland while the overlay gets the window behavior it needs.

An explicitly configured `QT_QPA_PLATFORM` is respected. `ZARA_PETS_QPA_PLATFORM` can override the backend for the Pet process only. Without XWayland, Qt's native platform selection remains in effect.

### Transparent-region click-through

Reliable pixel-perfect click-through is not portable across Qt/compositors. The overlay's transparent rectangle may still intercept clicks. Zara distinguishes normal clicks from drags and exposes a right-click menu, but this remains a platform limitation rather than something documentation should euphemize into “mostly transparent UX.”

### Pet tray icon

The standalone Pet overlay does not implement its own system-tray icon. It uses its window/context menu. This is separate from Zara Desktop's tray support.

## macOS and Windows

The Qt code may run, and discovery knows the external Pet storage conventions above, but Zara only tests/supports Linux for this feature. Do not convert “portable framework” into “supported platform” in release notes.

## Troubleshooting

- **Nothing appears:** run `zara --pets-settings`, select a package, enable Pets, save, then run `zara --pets`.
- **Imported package does not animate:** verify V1/V2 dimensions. The importer rejects mismatched sheets.
- **Position resets:** saved coordinates are validated against current displays; if the old monitor disappeared, the Pet falls back to a valid screen.
- **PySide6 missing in a development checkout:** use `nix develop`. The supported dependency path is the flake, not an improvised global pip environment.
- **Wayland overlay behaves badly:** check whether XWayland is available and review [`wiki/pets.org`](../wiki/pets.org) for the Pet-only Qt backend behavior.

## Compatibility boundaries

- V1 packages do not have V2 pointer-look rows, but keep their standard state/drag/wave/jump animation set.
- Only PNG and WebP assets are accepted.
- External ChatGPT/Codex Pet formats may change. Zara's importer is adapter-based so old supported formats can remain readable while a new external format gets a new adapter.

For current feature status, including what the rest of Zara does with runtime events, return to [`features.org`](features.org) and [`architecture.org`](architecture.org).
