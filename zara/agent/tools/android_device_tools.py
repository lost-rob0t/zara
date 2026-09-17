"""Agent-facing Android control tools.

Authority lives on the Android device in its local Prolog workspace. This tool
is only a transport surface: if ``android_raw`` is not advertised, the server
cannot make it exist and the call fails closed.
"""

from __future__ import annotations

import json
from typing import Dict

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field

from ... import android_device_bridge


class AndroidExecuteArgs(BaseModel):
    backend: str = Field(
        default="auto",
        description=(
            "Android execution backend. Use auto unless a specific identity is required. "
            "Known backends include assist, intent, accessibility, notification, ime, "
            "shell, shizuku, root, device_policy, hidden_api, and app_functions."
        ),
        min_length=1,
        max_length=64,
    )
    operation: str = Field(
        ...,
        description=(
            "Operation identifier exposed by the selected backend, for example "
            "assist.snapshot, accessibility.snapshot, accessibility.global_action, "
            "accessibility.node_action, accessibility.gesture, notification.list, "
            "notification.reply, ime.context, ime.commit, intent.start_activity, "
            "shell.exec, device_policy.lock_now, or device_policy.reboot."
        ),
        min_length=1,
        max_length=128,
    )
    arguments: Dict[str, str] = Field(
        default_factory=dict,
        description="String-valued operation arguments. The Android Prolog policy is final authority.",
    )
    timeout_seconds: float = Field(
        default=30.0,
        ge=1.0,
        le=300.0,
        description="Maximum time to wait for the phone operation.",
    )


def _android_execute(
    backend: str = "auto",
    operation: str = "",
    arguments: Dict[str, str] | None = None,
    timeout_seconds: float = 30.0,
) -> str:
    try:
        result = android_device_bridge.execute(
            backend=backend,
            operation=operation,
            arguments=arguments or {},
            timeout_seconds=timeout_seconds,
        )
    except Exception as error:
        return json.dumps(
            {
                "success": False,
                "error": type(error).__name__,
                "message": str(error),
            },
            separators=(",", ":"),
        )

    return json.dumps(
        {
            "success": True,
            "action_id": result.action_id,
            "capability": result.capability,
            "outcome": result.outcome,
            "backend": getattr(result, "backend", None),
            "identity": getattr(result, "identity", None),
            "output": getattr(result, "output", None),
        },
        separators=(",", ":"),
    )


def build_android_execute_tool() -> StructuredTool:
    # Install gateway tracking while built-ins are loaded. AgentManager is
    # constructed before the server gateway, so every subsequently-created
    # gateway is observed without changing the transport constructor API.
    android_device_bridge.install_gateway_tracking()
    return StructuredTool.from_function(
        func=_android_execute,
        name="android_execute",
        description=(
            "Control the connected Zara Android device through its locally authorized raw Android plane. "
            "The phone's Prolog android_authority/android_backend policy determines actual power. "
            "With android_authority(unrestricted), use this for screen context, arbitrary Accessibility "
            "actions/gestures, notification actions/replies, IME editing, raw Intents, shell, root/Shizuku, "
            "and device-owner operations when those backends are provisioned. Use backend=auto normally. "
            "Never assume root/Shizuku/device-owner exists; inspect the returned backend/identity/error."
        ),
        args_schema=AndroidExecuteArgs,
    )
