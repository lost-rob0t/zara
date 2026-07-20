"""
Configuration system for Zarathushtra.

Provides TOML-based configuration for the entire system with automatic
initialization and module loading.
"""

import os
import sys
import math
from pathlib import Path
from typing import Any, Dict, List, Optional


DEFAULT_FILE_TOOL_MAX_BYTES = 20000
DEFAULT_OLLAMA_ENDPOINT = "http://localhost:11434/api/chat"

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
# Optional phrases to end conversation mode
stop_phrases = ["goodbye", "bye", "end conversation", "stop conversation", "end session", "stop session"]
# Stop TTS playback when user starts speaking
stop_tts_on_input = true
# Silence detection tuning (seconds)
silence_duration = 1.5
silence_threshold = 0.02
silence_log_interval = 0.5
# Return to passive mode if nobody speaks after a wake word
first_speech_timeout = 5.0
# Hard cap for a single utterance and buffered callback chunks
max_utterance_duration = 30.0
audio_queue_chunks = 32

# Immediate acknowledgement (ZARA-025)
[wake.acknowledgement]
enabled = true
# Default single phrase (kept for backward compatibility)
phrase = "Okay"
# Multiple phrase variants rotated round-robin across turns.
# Clips are pre-generated at startup and cached in
# $XDG_CACHE_HOME/zarathushtra/acknowledgement-<provider>-<voice>-<phrase>-<hash>.wav
phrases = ["Okay", "Let me think about that", "One sec", "Got it", "Sure", "Hmm, let me see"]
provider = "edge"
voice = "en-US-AriaNeural"
volume = 1.0

[stt]
# Speech-to-Text settings
provider = "faster-whisper"  # or "whisper"
model = "small"
device = "cpu"  # or "cuda"
threads = 4
# Streaming VAD (Silero) parameters — 512 samples = 32 ms at 16 kHz
vad_threshold = 0.5
min_speech_ms = 128        # ~4 chunks before speech_started
trailing_silence_ms = 512  # ~16 chunks after speech to trigger speech_ended
max_utterance_ms = 30000   # hard cap on utterance length
no_speech_timeout_ms = 5000
partial_transcript_ms = 1000  # partial transcription interval

[tts]
# Text-to-Speech settings
provider = "qwen3"  # "local", "11labs", "edge", or "qwen3"
model_path = ""
sample_rate = 16000
connect_timeout = 5.0
read_timeout = 20.0
total_timeout = 30.0

[llm]
# LLM provider for agent mode
provider = "ollama"  # "anthropic", "openai", or "ollama"
model = ""  # Leave empty for provider defaults
endpoint = ""
connect_timeout = 5.0
read_timeout = 20.0
total_timeout = 30.0
max_retries = 2
history_limit = 20

# API keys (can also be set via environment variables)
# anthropic_api_key = ""
# openai_api_key = ""

[agent]
# Conversational agent settings
conversation_timeout = 3600  # seconds (60 minutes grace window)
# Short grace period after TTS playback completes before the listener
# returns to passive mode. Keep small so the user can respond quickly.
post_tts_silence_seconds = 2.0
max_steps = 10  # max agentic steps per turn
# System prompt (inline string or filepath)
system_prompt = ""

[latency]
# Structured voice-turn metrics. XDG_STATE_HOME is used when metrics_path is empty.
enabled = true
metrics_path = ""

[latency.budgets]
# Deterministic local fixture gates. Provider LLM latency is report-only.
wake_to_ack_first_audio_p95_ms = 350
speech_end_to_final_transcript_p95_ms = 700
first_text_chunk_to_first_tts_audio_p95_ms = 500
barge_in_to_playback_stop_p95_ms = 200

[prolog]
# Prolog engine settings
main_file = "main.pl"
load_on_startup = true

[tools]
# Enable/disable LangChain tools
calculator = true
get_current_time = true
query_prolog = true
remember = true
recall = true
file_tools = false

[file_tools]
# File tools are disabled above by default. Relative roots use the repository root.
readable_roots = ["."]
writable_roots = ["."]
max_bytes = 20000

[database]
# Shared SQLite database
path = "~/.local/share/zarathushtra/zara.db"

[todo]
# Todo defaults
default_status = "TODO"
default_duration_minutes = 30

[noaa]
# NOAA weather defaults used by the NOAA plugin
# default_latitude = 39.7456
# default_longitude = -97.0892
# user_agent = "ZarathushtraWeather/1.0 (contact: you@example.com)"

[memory]
# Long-term memory settings
enabled = true
persist_directory = "~/.local/share/zarathushtra/chroma"
collection_name = "zara_memory"
embedding_backend = "onnx"
embedding_model = "all-MiniLM-L6-v2"
top_k = 5
max_chars = 1200
summary_max_chars = 4000

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
# Leave empty to use the canonical defaults in zara.dictate.
stop_phrases = []

"""


class ConfigError(RuntimeError):
    """Raised when a configuration file cannot be loaded or validated."""


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
            raise ConfigError("TOML support is unavailable; install tomli or use Python 3.11+")

        try:
            with open(self.config_file, "rb") as f:
                config = tomllib.load(f)
        except (OSError, tomllib.TOMLDecodeError) as error:
            raise ConfigError(f"Failed to load config {self.config_file}: {error}") from error

        self._validate_config(config)
        return config

    def _validate_config(self, config: Dict[str, Any]) -> None:
        tts_config = config.get("tts", {})
        if not isinstance(tts_config, dict):
            raise ConfigError("Invalid [tts] configuration: expected a TOML table")

        provider = tts_config.get("provider", "qwen3")
        if provider == "qwen":
            provider = "qwen3"
            tts_config["provider"] = provider

        supported_providers = {"local", "11labs", "edge", "qwen3"}
        if provider not in supported_providers:
            choices = ", ".join(sorted(supported_providers))
            raise ConfigError(f"Unsupported TTS provider {provider!r}; choose one of: {choices}")

        if provider == "11labs":
            required = ("elevenlabs_api_key", "elevenlabs_voice_id")
            missing = [key for key in required if not tts_config.get(key)]
            if missing:
                fields = ", ".join(f"tts.{key}" for key in missing)
                raise ConfigError(f"11labs TTS requires {fields}")

        llm_config = config.get("llm", {})
        if not isinstance(llm_config, dict):
            raise ConfigError("Invalid [llm] configuration: expected a TOML table")
        llm_provider = llm_config.get("provider", "ollama")
        if llm_provider not in {"anthropic", "openai", "ollama"}:
            raise ConfigError(f"Unsupported LLM provider {llm_provider!r}")
        for key in ("connect_timeout", "read_timeout", "total_timeout"):
            value = llm_config.get(key, 1.0)
            if isinstance(value, bool) or not isinstance(value, (int, float)) or value <= 0:
                raise ConfigError(f"llm.{key} must be a positive number")
        for key, minimum in (("max_retries", 0), ("history_limit", 1)):
            value = llm_config.get(key, minimum)
            if isinstance(value, bool) or not isinstance(value, int) or value < minimum:
                raise ConfigError(f"llm.{key} must be an integer of at least {minimum}")

        tools_config = config.get("tools", {})
        if not isinstance(tools_config, dict):
            raise ConfigError("Invalid [tools] configuration: expected a TOML table")
        if not isinstance(tools_config.get("file_tools", False), bool):
            raise ConfigError("tools.file_tools must be true or false")

        file_config = config.get("file_tools", {})
        if not isinstance(file_config, dict):
            raise ConfigError("Invalid [file_tools] configuration: expected a TOML table")
        for key in ("readable_roots", "writable_roots"):
            roots = file_config.get(key, ["."])
            if (
                not isinstance(roots, list)
                or not roots
                or any(not isinstance(root, str) or not root for root in roots)
            ):
                raise ConfigError(f"file_tools.{key} must be a non-empty string list")
        max_bytes = file_config.get("max_bytes", DEFAULT_FILE_TOOL_MAX_BYTES)
        if not isinstance(max_bytes, int) or isinstance(max_bytes, bool) or max_bytes < 1:
            raise ConfigError("file_tools.max_bytes must be a positive integer")

        latency_config = config.get("latency", {})
        if not isinstance(latency_config, dict):
            raise ConfigError("Invalid [latency] configuration: expected a TOML table")
        if not isinstance(latency_config.get("enabled", True), bool):
            raise ConfigError("latency.enabled must be true or false")
        metrics_file = latency_config.get("metrics_path", "")
        if not isinstance(metrics_file, str):
            raise ConfigError("latency.metrics_path must be a string")
        budgets = latency_config.get("budgets", {})
        if not isinstance(budgets, dict):
            raise ConfigError("Invalid [latency.budgets] configuration: expected a TOML table")
        allowed_budgets = {
            "wake_to_ack_first_audio_p95_ms",
            "speech_end_to_final_transcript_p95_ms",
            "first_text_chunk_to_first_tts_audio_p95_ms",
            "barge_in_to_playback_stop_p95_ms",
        }
        unknown_budgets = sorted(set(budgets) - allowed_budgets)
        if unknown_budgets:
            raise ConfigError(
                "Unknown latency budget(s): " + ", ".join(unknown_budgets)
            )
        for key, value in budgets.items():
            if (
                isinstance(value, bool)
                or not isinstance(value, (int, float))
                or not math.isfinite(value)
                or value <= 0
            ):
                raise ConfigError(f"latency.budgets.{key} must be a positive number")

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
        endpoint_override = os.getenv("ZARA_LLM_ENDPOINT")
        endpoint = endpoint_override or llm_config.get("endpoint", "")
        if (
            not endpoint_override
            and provider != "ollama"
            and endpoint == DEFAULT_OLLAMA_ENDPOINT
        ):
            endpoint = ""

        # Get API keys from config or environment
        anthropic_key = os.getenv("ANTHROPIC_API_KEY", llm_config.get("anthropic_api_key", ""))
        openai_key = os.getenv("OPENAI_API_KEY", llm_config.get("openai_api_key", ""))

        return {
            "provider": provider,
            "model": model if model else None,
            "endpoint": endpoint if endpoint else None,
            "anthropic_api_key": anthropic_key if anthropic_key else None,
            "openai_api_key": openai_key if openai_key else None,
            "connect_timeout": float(llm_config.get("connect_timeout", 5.0)),
            "read_timeout": float(llm_config.get("read_timeout", 20.0)),
            "total_timeout": float(llm_config.get("total_timeout", 30.0)),
            "max_retries": int(llm_config.get("max_retries", 2)),
            "history_limit": int(llm_config.get("history_limit", 20)),
        }

    def get_latency_config(self) -> Dict[str, Any]:
        """Return structured latency metric settings and deterministic budgets."""
        latency_config = self.get_section("latency")
        return {
            "enabled": bool(latency_config.get("enabled", True)),
            "metrics_path": latency_config.get("metrics_path", ""),
            "budgets": dict(latency_config.get("budgets", {})),
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

    def get_file_tool_config(self, repo_root: Path) -> Dict[str, Any]:
        file_config = self.get_section("file_tools")

        def expand_roots(key: str) -> List[Path]:
            roots = []
            for value in file_config.get(key, ["."]):
                expanded = Path(os.path.expanduser(os.path.expandvars(value)))
                roots.append(expanded if expanded.is_absolute() else repo_root / expanded)
            return roots

        return {
            "base_dir": repo_root,
            "readable_roots": expand_roots("readable_roots"),
            "writable_roots": expand_roots("writable_roots"),
            "max_bytes": file_config.get("max_bytes", DEFAULT_FILE_TOOL_MAX_BYTES),
        }

    def get_agent_system_prompt(self) -> Optional[str]:
        """
        Get the agent system prompt.

        If the value points to a file, read the prompt from disk.
        """
        agent_config = self.get_section("agent")
        prompt_value = agent_config.get("system_prompt", "")
        if not prompt_value:
            return None

        expanded = os.path.expanduser(os.path.expandvars(str(prompt_value)))
        prompt_path = Path(expanded)
        if not prompt_path.is_absolute():
            candidate = self.config_dir / expanded
            if candidate.exists():
                prompt_path = candidate

        if prompt_path.exists() and prompt_path.is_file():
            try:
                return prompt_path.read_text(encoding="utf-8")
            except Exception:
                return str(prompt_value)

        return str(prompt_value)

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
