% Android <-> desktop pairing — verified live-session knowledge (2026-11-27).
% Provenance: session on worktree zarathushtra-android-refresh, master
% 0fad5bf3/b80a3394, Galaxy S21 (R3CR50FV4XB, Android 15), daemon + phone on
% 10.50.50.0/24. Issues: #1539 (socket path), #1540 (pairing surface),
% epic #1358 (acceptance, session comment 7232550809).

% The daemon exposes its security-admin control socket in exactly one of two
% places depending on endpoint mode (server.py _control_socket_path):
pairing_admin_socket(secure_tcp_primary, 'security-dir/security-admin.sock').
pairing_admin_socket(remote_endpoint_gateway, 'runtime-lease-parent/zara-control.sock').

% zara pair (zara/pairing.py main) ALWAYS probes the security-dir socket, so
% it cannot pair against --remote-endpoint daemons (the recommended systemd
% deployment). Workaround used successfully: primary secure-TCP mode.
issue(1539, zara_pair_admin_socket_mismatch, open).
zara_pair_workaround('zara-server --endpoint tcp://0.0.0.0:6060 --security-dir <security-dir>').
pairing_failure_signature('Pairing unavailable: security admin endpoint is not running').

% Working pairing recipe (verified to the approval step; codes matched).
pairing_recipe([
    step(1, 'start daemon primary secure-TCP + owner security dir'),
    step(2, 'zara pair --endpoint tcp://<lan-ip>:6060 --advertise-host <lan-ip>'),
    step(3, 'capture plain URI (hide qrencode from PATH, else only QR prints)'),
    step(4, 'deliver URI to phone via VIEW intent (zara://pair/v1 deep link)'),
    step(5, 'operator approves y/N at the broker prompt'),
    step(6, 'phone stores endpoint + pinned server key; enrollment ready')
]).
pairing_uri_hidden_by_qr(qrencode_present_prints_qr_only).
pairing_enroll_transient_failure_observed(retry_identical_call_succeeded).

% adb deep-link injection: `adb shell am start -d <uri>` mangles the URI at
% the first '&' (device-side shell splitting). Verified robust path: pipe
% the URI to a device file via stdin, then run an am-start script via stdin.
deeplink_injection([
    step(1, 'cat uri.txt | adb shell "cat > /data/local/tmp/pairuri.txt"'),
    step(2, "printf 'uri=$(cat /data/local/tmp/pairuri.txt)\\nam start -a android.intent.action.VIEW -d \"$uri\" ai.zara.app\\n' | adb shell sh")
]).
adb_screencap_png_via_pull(exec_out_corrupts_sometimes).

% zara-adb-pair (home-manager tool, NOT in the zara repo) misses USB devices:
% pick_device filters rows on tab+'device' but `adb devices -l` emits
% space-aligned rows. Fix: split() whitespace, require fields[1]=='device'.
% Patched copy used during session: /tmp/opencode/zara-adb-pair-fixed.
external_tool_bug(zara_adb_pair_pick_device_usb_invisible).

% Pairing surface UX (issue #1540): the "Scan pairing QR" action renders as a
% bottom overlay on EVERY route while unenrolled (PairingZaraApp.kt
% PairingSetupAction); DESIGN.md wants it owned by Settings -> Connection.
% VERIFY PAIRING dialog stays WAITING with a stale code after broker
% failure/restart and never projects URI expiry.
issue(1540, pairing_surface_overlay_and_dialog, open).

% Desktop daemon operations (verified live).
daemon_stack_services([zara_server, zara_wake, zara_desktop, zara_pair]).
daemon_secrets_env('~/.config/zarathushtra/secrets.env', openrouter_api_key).
daemon_degraded_openai_probe_warning(benign_when_openrouter_configured).
checkout_daemon_start_script('/tmp/opencode/start-checkout-server.sh').

% Device fleet state (verified 2026-11-27).
device_fleet_installed(['ai.zara.app', 'ai.zara.llmserve', 'ai.zara.code.editor',
    'ai.zara.termux.bridge', 'ai.zara.org.app', 'ai.zara.org.todo', 'ai.zara.org.editor',
    'ai.zara.org.reminder', 'ai.zara.org.timer', 'ai.zara.org.roam', 'ai.zara.org.graph',
    'ai.zara.org.home']).
device_fleet_not_installed(['ai.zara.llmserve.adversary'(test_only), wear_apps(needs_watch)]).
device_ui_survey('chat/voice/workspace/settings/connection surfaces match DESIGN.md three-menu shell; remote model card lives on Runtime & local AI screen, not Connection').
