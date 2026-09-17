"""Top-level Zara desktop windows."""

from .chat import FullChatWindow
from .copilot import CopilotPresentation
from .extensible import CopilotWindow
from .quick import QuickCopilotWindow
from .settings import SettingsWindow
from .status import DesktopStatusWindow

__all__ = [
    "CopilotPresentation",
    "CopilotWindow",
    "DesktopStatusWindow",
    "FullChatWindow",
    "QuickCopilotWindow",
    "SettingsWindow",
]
