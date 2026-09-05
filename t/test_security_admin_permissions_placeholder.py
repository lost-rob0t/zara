from __future__ import annotations

import os
import stat
from pathlib import Path

from zara.security import Capability
from zara.security_admin import SecurityAdminClient, SecurityAdminServer
from zara.security_state import PersistentSecurityState


def test_security_admin_permission_contract_is_covered(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    registry = state.load_registry()
    admin = SecurityAdminServer(state, capabilities={Capability.SESSION_BASIC})
    admin.bind_registry(registry)
    admin.start()
    try:
        directory_info = os.lstat(state.directory)
        socket_info = os.lstat(state.control_socket_path)
        assert stat.S_IMODE(directory_info.st_mode) == 0o700
        assert stat.S_IMODE(socket_info.st_mode) == 0o600
        assert socket_info.st_uid == os.getuid()
        assert SecurityAdminClient(state.control_socket_path).request("list") == []
    finally:
        admin.close(timeout=1.0)
