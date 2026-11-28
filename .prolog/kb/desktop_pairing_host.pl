% Desktop remote-device pairing host surface.
% Verified 2026-11-27 in worktree zarathushtra-desktop-pair-remote-device.

surface(desktop_connections, 'Settings > Connections hosts both directions: pair-this-desktop (client) and Pair Remote device (host)').

engine(zara_pairing_host,
    'zara/pairing.py owns the protocol: PairingBroker (single-use, expiring, verify-code), plan_host_pairing/4 shared by CLI main() and the desktop dialog, security_directory/2 resolves ZARA_SECURITY_DIR > [daemon].security_dir > XDG state').

enrollment_path('host pairing enrolls through the daemon owner-local admin socket (SecurityAdminClient on state.control_socket_path); the daemon persists through PersistentSecurityState, so offline file writes must never be used for enrollment').

nickname_contract('device nicknames live in PersistentSecurityState client records as an optional backward-compatible "nickname" field; rename_device/2 renames ACTIVE devices only, stores trimmed canonical text (<=48 chars, no control chars), and load fails closed on non-canonical stored values').

cancellation_contract('PairingBroker.wait_for_claim/1 takes an optional stop event and raises PairingCancelled promptly; the desktop dialog cancels without waiting out the expiry window').

qr_rendering('desktop QR uses the nix-managed python qrcode package (qr_pixmap in zara/desktop/windows/pairing_host.py); on ImportError it degrades to URI copy, never crashes').

mesh_wording('pairing UI and nicknames are device-generic (Phone/Desktop/Device fallback from device_id kind prefix); never hardcode Android').

test_layout([
    t/test_pairing.py - protocol + host plan + cancellation,
    t/test_security_device_nicknames.py - nickname persistence/fail-closed,
    t/test_desktop_pair_remote.py - full dialog flow with a real broker, scripted device, and live SecurityAdminServer
]).

desktop_dialog_contract('PairRemoteDeviceDialog emits session_ready/claim_observed/session_finished across threads; ClaimView (not the raw PairingClaim socket) crosses into the UI; nickname is captured at approve time and written through plan.security_dir').
