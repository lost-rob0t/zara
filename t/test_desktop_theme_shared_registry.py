from __future__ import annotations

from zara import themes as semantic_themes
from zara.desktop import theme as desktop_theme


def test_desktop_uses_the_single_shared_semantic_theme_registry():
    key = "test-desktop-shared"
    assert desktop_theme.THEME_REGISTRY is semantic_themes.THEME_REGISTRY
    assert desktop_theme.ThemeDefinition is semantic_themes.ThemeDefinition

    try:
        semantic_themes.configure_theme_registry(
            {
                key: {
                    "base": "signal-cabin",
                    "label": "Desktop Shared",
                    "primary": "#CC2244",
                }
            },
            load_packages=False,
            active_theme=key,
        )

        assert key in desktop_theme.THEME_REGISTRY
        assert desktop_theme.resolve_theme(key) == semantic_themes.resolve_theme(key)
        assert desktop_theme.resolve_theme(key).colors["primary"] == "#CC2244"
        assert desktop_theme.build_theme_palette(key).color(
            desktop_theme.QPalette.ColorRole.Highlight
        ).name().upper() == "#CC2244"
    finally:
        semantic_themes.configure_theme_registry(
            {},
            load_packages=False,
            active_theme="signal-cabin",
        )
