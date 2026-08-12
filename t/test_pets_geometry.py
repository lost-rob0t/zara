"""Tests for window geometry and off-screen position recovery."""

from __future__ import annotations

from zara.pets.geometry import ScreenRect, recover_position


def test_saved_position_on_screen_is_preserved():
    screens = [ScreenRect(0, 0, 1920, 1080)]
    assert recover_position(500, 400, screens) == (500, 400)


def test_saved_position_off_screen_falls_back():
    screens = [ScreenRect(0, 0, 1920, 1080)]
    # Saved at 5000,5000 (off every screen); fall back to default clamped.
    x, y = recover_position(5000, 5000, screens)
    assert screens[0].contains(x, y)


def test_saved_monitor_disappeared_falls_back():
    # Saved on a second monitor at (3000, 400) but only the primary exists now.
    screens = [ScreenRect(0, 0, 1920, 1080)]
    x, y = recover_position(3000, 400, screens, default=(100, 100))
    assert screens[0].contains(x, y)


def test_no_screens_returns_default():
    assert recover_position(None, None, [], default=(120, 130)) == (120, 130)


def test_none_saved_position_uses_default():
    screens = [ScreenRect(0, 0, 1920, 1080)]
    x, y = recover_position(None, None, screens, default=(50, 60))
    assert screens[0].contains(x, y)


def test_multi_monitor_position_on_second_screen_preserved():
    screens = [ScreenRect(0, 0, 1920, 1080), ScreenRect(1920, 0, 1920, 1080)]
    assert recover_position(2500, 400, screens) == (2500, 400)


def test_position_at_edge_with_margin_is_preserved():
    screens = [ScreenRect(0, 0, 1920, 1080)]
    # 8px margin: position (5, 5) is inside the margin boundary.
    assert recover_position(5, 5, screens) == (5, 5)


def test_default_clamped_when_screen_smaller():
    screens = [ScreenRect(0, 0, 300, 300)]
    x, y = recover_position(None, None, screens, default=(500, 500))
    assert x < 300 and y < 300


def test_negative_saved_position_rejected():
    screens = [ScreenRect(0, 0, 1920, 1080)]
    x, y = recover_position(-500, -500, screens)
    assert screens[0].contains(x, y)


def test_screen_rect_contains():
    rect = ScreenRect(0, 0, 100, 100)
    assert rect.contains(50, 50)
    assert not rect.contains(150, 50)
    assert rect.contains(0, 0, margin=0)
    assert not rect.contains(-1, 0, margin=0)