from __future__ import annotations

import pytest

from zara.themes import (
    THEME_REGISTRY,
    active_theme_key,
    configure_theme_registry,
    load_theme_packages,
    register_theme,
    resolve_theme,
    theme_from_mapping,
    unregister_theme,
)


def test_config_reload_removes_only_prior_config_owned_themes():
    config_key = "test-operator-red"
    package_key = "test-package-blue"

    class FakeEntryPoint:
        name = package_key

        @staticmethod
        def load():
            return lambda: {
                "base": "nord",
                "label": "Package Blue",
                "primary": "#1C71D8",
            }

    try:
        load_theme_packages(lambda: [FakeEntryPoint()])
        configure_theme_registry(
            {
                config_key: {
                    "base": "signal-cabin",
                    "label": "Operator Red",
                    "primary": "#FF304F",
                }
            },
            load_packages=False,
            active_theme=config_key,
        )

        assert resolve_theme(config_key).key == config_key
        assert active_theme_key() == config_key

        configure_theme_registry({}, load_packages=False)

        assert config_key not in THEME_REGISTRY
        assert package_key in THEME_REGISTRY
        assert "signal-cabin" in THEME_REGISTRY
        assert active_theme_key() == "signal-cabin"
    finally:
        if config_key in THEME_REGISTRY:
            unregister_theme(config_key)
        if package_key in THEME_REGISTRY:
            unregister_theme(package_key)


def test_rejected_config_reload_is_atomic():
    config_key = "test-operator-red"
    external_key = "test-external-blue"

    try:
        register_theme(
            theme_from_mapping(
                external_key,
                {
                    "base": "nord",
                    "label": "External Blue",
                    "primary": "#1C71D8",
                },
            )
        )
        configure_theme_registry(
            {
                config_key: {
                    "base": "signal-cabin",
                    "label": "Operator Red",
                    "primary": "#FF304F",
                }
            },
            load_packages=False,
            active_theme=config_key,
        )
        before_registry = dict(THEME_REGISTRY)
        before_active = active_theme_key()

        with pytest.raises(ValueError, match="already registered"):
            configure_theme_registry(
                {
                    external_key: {
                        "base": "signal-cabin",
                        "label": "Config Blue",
                        "primary": "#3355FF",
                    }
                },
                load_packages=False,
            )

        assert dict(THEME_REGISTRY) == before_registry
        assert active_theme_key() == before_active
    finally:
        if config_key in THEME_REGISTRY:
            unregister_theme(config_key)
        if external_key in THEME_REGISTRY:
            unregister_theme(external_key)
