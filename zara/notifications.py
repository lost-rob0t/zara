"""
Async notification utilities using notify-send.

Provides fast, non-blocking desktop notifications.
"""

import asyncio
import logging
from typing import Literal

logger = logging.getLogger(__name__)


UrgencyLevel = Literal["low", "normal", "critical"]


async def send_notification_async(
    title: str,
    message: str,
    urgency: UrgencyLevel = "normal",
    timeout: int = 5000
) -> bool:
    """
    Send desktop notification using notify-send (async, non-blocking).

    Args:
        title: Notification title
        message: Notification body text
        urgency: Urgency level (low, normal, critical)
        timeout: Timeout in milliseconds (-1 for default)

    Returns:
        True if notification sent successfully
    """
    try:
        cmd = [
            "notify-send",
            "-u", urgency,
            "-t", str(timeout),
            title,
            message
        ]

        proc = await asyncio.create_subprocess_exec(
            *cmd,
            stdout=asyncio.subprocess.DEVNULL,
            stderr=asyncio.subprocess.DEVNULL
        )

        await proc.wait()
        return proc.returncode == 0

    except Exception as e:
        logger.error(f"Failed to send notification: {e}")
        return False


def send_notification(
    title: str,
    message: str,
    urgency: UrgencyLevel = "normal",
    timeout: int = 5000
) -> bool:
    """
    Synchronous wrapper for send_notification_async.

    Args:
        title: Notification title
        message: Notification body text
        urgency: Urgency level (low, normal, critical)
        timeout: Timeout in milliseconds

    Returns:
        True if notification sent successfully
    """
    try:
        loop = asyncio.get_event_loop()
        if loop.is_running():
            # If we're already in an async context, create a task
            asyncio.create_task(
                send_notification_async(title, message, urgency, timeout)
            )
            return True
        else:
            # Otherwise run synchronously
            return asyncio.run(
                send_notification_async(title, message, urgency, timeout)
            )
    except Exception as e:
        logger.error(f"Notification error: {e}")
        return False


async def test_notifications():
    """Test notification system."""
    print("Sending test notifications...")

    # Test 1: Normal notification
    await send_notification_async(
        "Zara Test",
        "This is a normal priority notification",
        urgency="normal"
    )
    await asyncio.sleep(1)

    # Test 2: Low urgency
    await send_notification_async(
        "Zara Test",
        "This is a low priority notification",
        urgency="low"
    )
    await asyncio.sleep(1)

    # Test 3: Critical
    await send_notification_async(
        "Zara Test",
        "This is a critical notification",
        urgency="critical"
    )

    print("Test notifications sent!")


if __name__ == "__main__":
    asyncio.run(test_notifications())
