"""Tests for desktop-pet Qt platform selection."""

from __future__ import annotations

from zara.pets.cli import _configure_pet_qpa_platform


def test_wayland_with_xwayland_prefers_xcb():
    env = {
        "XDG_SESSION_TYPE": "wayland",
        "WAYLAND_DISPLAY": "wayland-0",
        "DISPLAY": ":0",
    }

    platform = _configure_pet_qpa_platform(env, "linux")

    assert platform == "xcb"
    assert env["QT_QPA_PLATFORM"] == "xcb"


def test_wayland_without_xwayland_keeps_native_backend():
    env = {
        "XDG_SESSION_TYPE": "wayland",
        "WAYLAND_DISPLAY": "wayland-0",
    }

    platform = _configure_pet_qpa_platform(env, "linux")

    assert platform is None
    assert "QT_QPA_PLATFORM" not in env


def test_existing_qt_platform_is_respected():
    env = {
        "XDG_SESSION_TYPE": "wayland",
        "WAYLAND_DISPLAY": "wayland-0",
        "DISPLAY": ":0",
        "QT_QPA_PLATFORM": "wayland",
    }

    platform = _configure_pet_qpa_platform(env, "linux")

    assert platform == "wayland"
    assert env["QT_QPA_PLATFORM"] == "wayland"


def test_zara_pet_platform_override_wins():
    env = {
        "QT_QPA_PLATFORM": "wayland",
        "ZARA_PETS_QPA_PLATFORM": "xcb",
    }

    platform = _configure_pet_qpa_platform(env, "linux")

    assert platform == "xcb"
    assert env["QT_QPA_PLATFORM"] == "xcb"


def test_non_linux_platform_is_unchanged():
    env = {"XDG_SESSION_TYPE": "wayland", "DISPLAY": ":0"}

    platform = _configure_pet_qpa_platform(env, "darwin")

    assert platform is None
    assert "QT_QPA_PLATFORM" not in env
