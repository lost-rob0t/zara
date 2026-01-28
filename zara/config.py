"""
Configuration system for Zarathushtra.

Provides TOML-based configuration for the entire system with automatic
initialization and module loading.
"""

import os
import sys
from pathlib import Path
from typing import Dict, Any, List, Optional

# Use tomllib (Python 3.11+) or fallback to tomli
if sys.version_info >= (3, 11):
    import tomllib
else:
    try:
        import tomli as tomllib
    except ImportError:
        tomllib = None


DEFAULT_CONFIG_TOML = """# Zarathushtra Configuration

[wake]
# Wake word detection settings
model_path = "~/.zarathushtra/models/wake.onnx"
threshold = 0.5
sample_rate = 16000

[stt]
# Speech-to-Text settings
provider = "faster-whisper"  # or "whisper"
model = "small"
device = "cpu"  # or "cuda"
threads = 4

[tts]
# Text-to-Speech settings
provider = "qwen"  # or "custom"
model_path = ""
sample_rate = 16000

[llm]
# LLM provider for agent mode
provider = "ollama"  # "anthropic", "openai", or "ollama"
model = ""  # Leave empty for provider defaults
endpoint = "http://localhost:11434/api/chat"

# API keys (can also be set via environment variables)
# anthropic_api_key = ""
# openai_api_key = ""

[agent]
# Conversational agent settings
conversation_timeout = 60  # seconds
max_steps = 10  # max agentic steps per turn

[prolog]
# Prolog engine settings
main_file = "main.pl"
load_on_startup = true

[tools]
# Enable/disable built-in tools
calculator = true
current_time = true
prolog_query = true

[modules]
# Module/plugin search paths
# Paths can be absolute or relative to home directory (~)
search_paths = [
    "~/.zarathushtra/plugins",
    "~/.zarathushtra/modules",
]

# Modules to auto-load at startup
# Format: ["module_name.py"]
autoload = []

[dictate]
# Dictation mode settings
model = "small"
device = "cpu"
workers = 2
stop_phrases = ["end voice", "stop voice"]
"""


class ZaraConfig:
    """
    Configuration manager for Zarathushtra.

    Handles loading, validation, and access to all system configuration.
    Automatically initializes default config if none exists.
    """

    def __init__(self, config_path: Optional[str] = None):
        """
        Initialize configuration.

        Args:
            config_path: Path to config.toml file. If None, uses default location.
        """
        if config_path:
            self.config_dir = Path(config_path).parent
            self.config_file = Path(config_path)
        else:
            # Use XDG_CONFIG_HOME or default to ~/.config
            xdg_config = os.getenv("XDG_CONFIG_HOME")
            if xdg_config:
                self.config_dir = Path(xdg_config) / "zarathushtra"
            else:
                self.config_dir = Path.home() / ".config" / "zarathushtra"

            self.config_file = self.config_dir / "config.toml"

        # Initialize config if needed
        self._ensure_config_exists()

        # Load configuration
        self._config = self._load_config()

    def _ensure_config_exists(self):
        """Create default config file if it doesn't exist."""
        if not self.config_file.exists():
            # Create config directory
            self.config_dir.mkdir(parents=True, exist_ok=True)

            # Write default config
            self.config_file.write_text(DEFAULT_CONFIG_TOML)
            print(f"Initialized default config at: {self.config_file}")

    def _load_config(self) -> Dict[str, Any]:
        """
        Load configuration from TOML file.

        Returns:
            Parsed configuration dict
        """
        if tomllib is None:
            print("Warning: tomli/tomllib not available, using defaults")
            return self._get_default_config()

        try:
            with open(self.config_file, "rb") as f:
                return tomllib.load(f)
        except Exception as e:
            print(f"Warning: Failed to load config: {e}")
            return self._get_default_config()

    def _get_default_config(self) -> Dict[str, Any]:
        """Get default configuration as dict."""
        return {
            "wake": {
                "model_path": "~/.zarathushtra/models/wake.onnx",
                "threshold": 0.5,
                "sample_rate": 16000
            },
            "stt": {
                "provider": "faster-whisper",
                "model": "small",
                "device": "cpu",
                "threads": 4
            },
            "tts": {
                "provider": "qwen",
                "model_path": "",
                "sample_rate": 16000
            },
            "llm": {
                "provider": "ollama",
                "model": "",
                "endpoint": "http://localhost:11434/api/chat"
            },
            "agent": {
                "conversation_timeout": 60,
                "max_steps": 10
            },
            "prolog": {
                "main_file": "main.pl",
                "load_on_startup": True
            },
            "tools": {
                "calculator": True,
                "current_time": True,
                "prolog_query": True
            },
            "modules": {
                "search_paths": ["~/.zarathushtra/plugins", "~/.zarathushtra/modules"],
                "autoload": []
            },
            "dictate": {
                "model": "small",
                "device": "cpu",
                "workers": 2,
                "stop_phrases": ["end voice", "stop voice"]
            }
        }

    def get(self, section: str, key: str, default: Any = None) -> Any:
        """
        Get configuration value.

        Args:
            section: Config section (e.g., "llm", "agent")
            key: Key within section
            default: Default value if not found

        Returns:
            Configuration value or default
        """
        return self._config.get(section, {}).get(key, default)

    def get_section(self, section: str) -> Dict[str, Any]:
        """
        Get entire configuration section.

        Args:
            section: Section name

        Returns:
            Section dict or empty dict if not found
        """
        return self._config.get(section, {})

    def get_llm_config(self) -> Dict[str, Any]:
        """
        Get LLM configuration with environment variable override.

        Environment variables take precedence over config file.

        Returns:
            LLM configuration dict
        """
        llm_config = self.get_section("llm")

        # Override with environment variables if set
        provider = os.getenv("ZARA_LLM_PROVIDER", llm_config.get("provider", "ollama"))
        model = os.getenv("ZARA_LLM_MODEL", llm_config.get("model", ""))
        endpoint = os.getenv("ZARA_LLM_ENDPOINT", llm_config.get("endpoint", ""))

        # Get API keys from config or environment
        anthropic_key = os.getenv("ANTHROPIC_API_KEY", llm_config.get("anthropic_api_key", ""))
        openai_key = os.getenv("OPENAI_API_KEY", llm_config.get("openai_api_key", ""))

        return {
            "provider": provider,
            "model": model if model else None,
            "endpoint": endpoint if endpoint else None,
            "anthropic_api_key": anthropic_key if anthropic_key else None,
            "openai_api_key": openai_key if openai_key else None
        }

    def get_module_search_paths(self) -> List[Path]:
        """
        Get module search paths with expansion.

        Expands ~ and environment variables in paths.
        Only returns existing directories.

        Returns:
            List of expanded Path objects
        """
        modules_config = self.get_section("modules")
        search_paths = modules_config.get("search_paths", ["~/.zarathushtra/plugins"])

        expanded_paths = []
        for path_str in search_paths:
            # Expand ~ and environment variables
            expanded = os.path.expanduser(os.path.expandvars(path_str))
            path = Path(expanded)

            # Only include existing directories
            if path.exists() and path.is_dir():
                expanded_paths.append(path)
            elif not path.exists():
                # Create directory if it doesn't exist
                try:
                    path.mkdir(parents=True, exist_ok=True)
                    expanded_paths.append(path)
                except Exception:
                    # Skip if we can't create it
                    pass

        return expanded_paths

    def get_autoload_modules(self) -> List[str]:
        """
        Get list of modules to auto-load.

        Returns:
            List of module file names
        """
        modules_config = self.get_section("modules")
        return modules_config.get("autoload", [])

    def get_tool_config(self) -> Dict[str, bool]:
        """
        Get tool enable/disable configuration.

        Returns:
            Dict mapping tool names to enabled status
        """
        return self.get_section("tools")

    def reload(self):
        """Reload configuration from file."""
        self._config = self._load_config()


# Global config instance
_global_config: Optional[ZaraConfig] = None


def get_config(config_path: Optional[str] = None) -> ZaraConfig:
    """
    Get global configuration instance.

    Args:
        config_path: Optional custom config path (only used on first call)

    Returns:
        ZaraConfig instance
    """
    global _global_config

    if _global_config is None:
        _global_config = ZaraConfig(config_path)

    return _global_config


def init_config(config_path: Optional[str] = None) -> ZaraConfig:
    """
    Initialize configuration system.

    This should be called once at application startup.

    Args:
        config_path: Optional custom config path

    Returns:
        ZaraConfig instance
    """
    global _global_config
    _global_config = ZaraConfig(config_path)
    return _global_config


def load_user_modules(config: Optional[ZaraConfig] = None):
    """
    Load user modules from configured paths.

    Args:
        config: Optional config instance (uses global if None)
    """
    if config is None:
        config = get_config()

    # Get module search paths
    search_paths = config.get_module_search_paths()

    if not search_paths:
        return

    # Import the agent tool loader
    try:
        from .agent.tools.loader import load_plugins
        from .agent.tools.registry import ToolRegistry
    except ImportError:
        # Agent system not available
        return

    # Load plugins from each search path
    for path in search_paths:
        try:
            load_plugins(str(path))
        except Exception as e:
            print(f"Warning: Failed to load modules from {path}: {e}")
