"""
Keyboard typing utility for dictation mode
"""

import subprocess
import shutil


def type_text(text: str):
    """
    Type text into the currently focused window
    Uses xdotool on Linux, fallback to pynput
    """
    # Try xdotool first (fastest, most reliable on Linux)
    if shutil.which("xdotool"):
        subprocess.run([
            "xdotool", "type", 
            "--delay", "0",
            "--clearmodifiers",
            text
        ])
        return
    
    # Fallback to pynput
    try:
        from pynput.keyboard import Controller
        kb = Controller()
        kb.type(text)
    except ImportError:
        print("Error: Neither xdotool nor pynput available for typing")
    except Exception as e:
        print(f"Typing error: {e}")


if __name__ == "__main__":
    import time
    
    print("Testing typing in 3 seconds...")
    print("Focus a text editor!")
    time.sleep(3)
    
    type_text("Thus spoke Zarathustra: ")
    time.sleep(0.5)
    type_text("I teach you the overman. ")
    
    print("Done")
