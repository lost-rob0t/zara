"""ZARA/1 extension for the explicitly trusted Android raw-operation plane.

The base protocol keeps the portable device capability set intentionally tiny.
This module extends that schema in one place for Zara Android without relaxing
validation for existing capabilities or message types.
"""

from __future__ import annotations

import re
from typing import Any

from . import protocol

_BACKEND_RE = re.compile(r"^[a-z][a-z0-9_]{0,63}$")
_OPERATION_RE = re.compile(r"^[a-z][a-z0-9_.:-]{0,127}$")
_ARGUMENT_KEY_RE = re.compile(r"^[A-Za-z][A-Za-z0-9_.:-]{0,127}$")
_MAX_ARGUMENTS = 128
_MAX_ARGUMENT_BYTES = 64 * 1024
_MAX_RESULT_OUTPUT_BYTES = 48 * 1024
_BASE_CAPABILITIES = protocol.DEVICE_CAPABILITIES
_BASE_ARGS_VALIDATOR = protocol._validate_device_action_args
_BASE_DEVICE_VALIDATOR = protocol._validate_device_envelope
_INSTALLED = False


def _bounded_utf8(name: str, value: Any, *, max_bytes: int) -> str:
    if not isinstance(value, str):
        raise protocol.ProtocolValidationError(f"{name} must be a string")
    if len(value.encode("utf-8")) > max_bytes:
        raise protocol.ProtocolValidationError(f"{name} exceeds byte limit")
    return value


def _validate_android_raw_args(value: Any) -> None:
    if not isinstance(value, dict):
        raise protocol.ProtocolValidationError("android_raw args must be an object")
    if set(value) != {"backend", "operation", "arguments"}:
        raise protocol.ProtocolValidationError("android_raw args have invalid fields")

    backend = _bounded_utf8("backend", value["backend"], max_bytes=64)
    if _BACKEND_RE.fullmatch(backend) is None:
        raise protocol.ProtocolValidationError("android_raw backend is invalid")

    operation = _bounded_utf8("operation", value["operation"], max_bytes=128)
    if _OPERATION_RE.fullmatch(operation) is None:
        raise protocol.ProtocolValidationError("android_raw operation is invalid")

    arguments = value["arguments"]
    if not isinstance(arguments, dict):
        raise protocol.ProtocolValidationError("android_raw arguments must be an object")
    if len(arguments) > _MAX_ARGUMENTS:
        raise protocol.ProtocolValidationError("android_raw argument count exceeds limit")
    for key, item in arguments.items():
        if not isinstance(key, str) or _ARGUMENT_KEY_RE.fullmatch(key) is None:
            raise protocol.ProtocolValidationError("android_raw argument key is invalid")
        _bounded_utf8(f"android_raw argument {key}", item, max_bytes=_MAX_ARGUMENT_BYTES)


def _validate_rich_device_result(message: protocol.ProtocolMessage) -> None:
    body = protocol._validate_device_common(message)
    if message.reply_to is not None or message.trace_id is not None:
        raise protocol.ProtocolValidationError("device.action.result has invalid correlation")
    required = {"action_id", "outcome"}
    optional = {"backend", "identity", "output"}
    if not required.issubset(body) or set(body) - required - optional:
        raise protocol.ProtocolValidationError("device.action.result body has invalid fields")
    if body.get("outcome") != "completed":
        raise protocol.ProtocolValidationError("device.action.result outcome must be completed")
    protocol._validate_ascii_token("action_id", body["action_id"], max_bytes=128)
    if "backend" in body:
        backend = _bounded_utf8("backend", body["backend"], max_bytes=64)
        if _BACKEND_RE.fullmatch(backend) is None:
            raise protocol.ProtocolValidationError("device action backend is invalid")
    if "identity" in body:
        _bounded_utf8("identity", body["identity"], max_bytes=256)
    if "output" in body:
        _bounded_utf8("output", body["output"], max_bytes=_MAX_RESULT_OUTPUT_BYTES)


def install() -> None:
    global _INSTALLED
    if _INSTALLED:
        return

    protocol.DEVICE_CAPABILITIES = frozenset((*_BASE_CAPABILITIES, "android_raw"))

    def validate_device_action_args(capability: str, value: Any) -> None:
        if capability == "android_raw":
            _validate_android_raw_args(value)
            return
        _BASE_ARGS_VALIDATOR(capability, value)

    protocol._validate_device_action_args = validate_device_action_args

    def validate_device_envelope(message: protocol.ProtocolMessage) -> None:
        if message.type == "device.action.result":
            _validate_rich_device_result(message)
            return
        _BASE_DEVICE_VALIDATOR(message)

    protocol._validate_device_envelope = validate_device_envelope
    _INSTALLED = True


def uninstall_for_tests() -> None:
    """Restore the base protocol exactly; production code never calls this."""

    global _INSTALLED
    protocol.DEVICE_CAPABILITIES = _BASE_CAPABILITIES
    protocol._validate_device_action_args = _BASE_ARGS_VALIDATOR
    protocol._validate_device_envelope = _BASE_DEVICE_VALIDATOR
    _INSTALLED = False
