"""Top-level Zara desktop windows."""

from .chat import FullChatWindow
from .quick import QuickCopilotWindow
from .status import DesktopStatusWindow

__all__ = ["DesktopStatusWindow", "FullChatWindow", "QuickCopilotWindow"]
