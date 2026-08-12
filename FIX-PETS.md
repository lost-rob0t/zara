# Fix Zarathushtra Pet Overlay — blinking + task animation

## Repo

`/home/unseen/Documents/Projects/Zarathushtra` on branch `feature/pets`.

Enter the dev shell before anything else:

```bash
cd /home/unseen/Documents/Projects/Zarathushtra
nix develop
```

All Python runs must be `python -m zara ...` (NOT the `zara` binary, which is a pre-built Nix wrapper that ignores working-tree edits).

## The two bugs to fix

### Bug 1: Mara only plays the "running" animation while being dragged, not during actual tasks

**Expected:** When the wake listener processes a command (agent starts, LLM generates, tools run), Mara should switch to the `running` state animation (sprite row 7 — "active work / processing"). When the task finishes she should go to `ready` (row 8). When she needs input, `needs-input` (row 6). On failure, `blocked` (row 5). At rest, `idle` (row 0).

**Actual:** She only shows the running animation while the user drags the window. During real tasks she stays idle.

**Root cause is likely in the event flow. Trace it end to end and fix it:**

1. `zara/wake.py` calls `runtime_bridge.agent_started()` / `agent_completed()` on the Prolog-fallback path. Check those calls actually fire (they're wrapped in `try/except: pass` — the except may be swallowing a real error. Remove the bare `except` during debugging or log it).
2. `zara/agent/graph.py` `agent_node` calls `runtime_bridge.model_started()` / `model_completed()`. Same — verify they execute.
3. `zara/pets/runtime_bridge.py` `_tell_and_publish()` does two things: (a) tells an in-process `PetStateActor` via a weakref, and (b) publishes over ZMQ. Since `zara --wake --pets` runs the wake loop and the pet overlay as **separate processes**, only the ZMQ path can carry events. Verify the publisher actually binds and the subscriber connects (endpoint `tcp://127.0.0.1:35621`, see `zara/pets/ipc.py`).
4. `zara/pets/qt_overlay.py` runs a `PetSubscriber` polled from a Qt `QTimer` at ~30 Hz. The subscriber's `poll()` calls `on_event(payload)` which calls `_dispatch_payload(payload, actor)` which maps the payload to a `PetEvent` and tells the `PetStateActor`. Verify the subscriber actually receives frames — add a `logger.info` on receipt and check stderr (the wake parent now inherits the overlay's stderr).
5. The `PetStateActor` subscriber callback `_on_state` updates the `AnimationController` and repaints. Note the guard: `if overlay_state["dragging"]: return` — this suppresses state updates **during a drag**, which is correct, but make sure it isn't stuck True after a drag ends.

**Concrete diagnostic step:** run the overlay standalone in one terminal:

```bash
nix develop -c python -m zara --pets
```

and in another, publish a single test event over ZMQ:

```bash
nix develop -c python -c "
from zara.pets.ipc import PetPublisher
pub = PetPublisher(); pub.start()
import time; time.sleep(0.5)
pub.publish('AgentStarted', label='test')
time.sleep(0.2)
pub.publish('AgentCompleted', success=True, label='test')
time.sleep(0.2)
pub.publish('UserInputRequired', kind='approval')
time.sleep(0.2)
pub.publish('ModelFailed', reason='boom')
"
```

Mara should cycle: running → ready → needs-input → blocked. If she doesn't, the bug is in the subscriber/dispatch/actor path, not the wake hooks. Fix accordingly.

### Bug 2: The overlay window blinks/flickers

**Expected:** Mara renders as a stable, transparent, always-on-top window with smooth 8 fps animation.

**Actual:** She blinks in and out (visible, then not, then visible again) on a repeating cycle.

**Likely causes — investigate each and fix the real one:**

1. **`setQuitOnLastWindowClosed(False)` + window hide on focus loss.** The current flags are `Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint | Qt.WindowDoesNotAcceptFocus` with `WA_TranslucentBackground`, `WA_ShowWithoutActivating`, `WA_NoSystemBackground`. Some compositors (Mutter on GNOME, KWin on KDE/Wayland) withdraw a frameless translucent window when it doesn't accept focus and no other app-focused window owns it, causing a blink loop. Try removing `Qt.WindowDoesNotAcceptFocus` and instead call `setAttribute(Qt.WA_ShowWithoutActivating, True)` only — that keeps the window non-activating without the compositor withdrawing it. Test on the user's actual compositor (ask if unsure; they run GNOME on X11 most likely).

2. **Composited transparency + `CompositionMode_Source` clear.** The `paintEvent` currently does:
   ```python
   painter.setCompositionMode(QPainter.CompositionMode_Source)
   painter.fillRect(self.rect(), Qt.transparent)
   painter.setCompositionMode(QPainter.CompositionMode_SourceOver)
   painter.drawPixmap(self.rect(), self._current_pixmap)
   ```
   On some compositors, clearing the whole widget to transparent every frame causes the compositor to briefly show the desktop behind it before the pixmap is drawn, producing a flicker. Try removing the explicit `fillRect(Qt.transparent)` clear and instead set `WA_OpaquePaintEvent` to False (it already is via `WA_TranslucentBackground`) and let Qt manage the clear. Only add the explicit clear back if smearing appears.

3. **`WA_NoSystemBackground`.** This attribute can cause blinking on X11 when the compositor doesn't preserve the window content between frames. Try removing `WA_NoSystemBackground` and keeping only `WA_TranslucentBackground`.

4. **Window flags interfering with the compositor's compositing.** On GNOME/Mutter, `Qt.WindowStaysOnTopHint` maps to `NET_WM_STATE_ABOVE` which is fine, but combined with a frameless translucent unmapped-for-focus window, Mutter can enter a withdraw/map loop. The robust fix is to give the window a tiny 1x1 "focus sink" parent, OR drop `WindowDoesNotAcceptFocus` and rely solely on `WA_ShowWithoutActivating`.

5. **The animation timer repainting at 60 Hz even when the frame hasn't changed.** `frame_changed()` should gate this, but if `_update_frame` is also being called from elsewhere (e.g. the IPC timer or state callbacks) on every tick, it can cause extra repaints. Confirm `_update_frame` is only called when `frame_changed()` is true OR when state actually transitions.

**Concrete diagnostic step:** add `print("paint", time.monotonic())` to `paintEvent` and watch the cadence. A healthy stream is ~8 paints/sec (one per frame change). A blinking window will show alternating paint/gap or paint bursts. Also add `print("state", state)` in `_on_state` to see if state is toggling.

## What NOT to do

- Don't add a background to the window (it must stay transparent).
- Don't remove `WindowStaysOnTopHint`.
- Don't switch to `Qt.Tool` (that caused worse blinking in earlier testing — some compositors withdraw tool windows when the app loses focus).
- Don't change the ZMQ endpoint or topology (PUB binds, SUB connects — that's tested and working).
- Don't touch the sprite row mapping (row 7 = running/tasks, row 1 = drag — that's correct per the user's request).
- Don't add inline comments unless asked.
- Don't add new dependencies.
- Follow `AGENTS.md` (no new linters, no non-Nix deps, Nix for builds/tests).

## Verification

After fixing, verify both:

1. **No blinking:** `nix develop -c python -m zara --pets` shows a stable Mara for 30+ seconds with no flicker.
2. **Tasks animate:** in one terminal `nix develop -c python -m zara --pets`, in another run the ZMQ test snippet above — Mara cycles through running → ready → needs-input → blocked. Then run `nix develop -c python -m zara --wake --pets`, say "hey zara" and give a task — Mara shows row 7 during work, not just while dragged.

Run the pets test suite to confirm no regressions:

```bash
nix develop -c pytest -q t/test_pets_state.py t/test_pets_actor.py t/test_pets_manifest.py t/test_pets_import.py t/test_pets_animation.py t/test_pets_geometry.py t/test_pets_discovery.py t/test_pets_ipc.py
```

All 116+ tests must still pass.

## Commit

Commit with `fix(pets): ...` messages on the `feature/pets` branch. Push. Do not open a new PR — the existing PR #76 tracks this branch.