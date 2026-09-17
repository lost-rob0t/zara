"""Top-level Zara desktop windows."""

from .chat import FullChatWindow
from .copilot import CopilotPresentation
from .extensible import CopilotWindow
from .extensible_settings import SettingsWindow
from .quick import QuickCopilotWindow
from .status import DesktopStatusWindow

__all__ = [
    "CopilotPresentation",
    "CopilotWindow",
    "DesktopStatusWindow",
    "FullChatWindow",
    "QuickCopilotWindow",
    "SettingsWindow",
]
