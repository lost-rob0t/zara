"""
Direct conversation mode CLI for agent.

Allows direct interaction with the conversational agent without
wake word detection. Useful for testing and debugging.
"""

import sys
import asyncio
from .agent import AgentManager
from .config import get_config


async def chat_loop():
    """Main conversation loop"""
    print("Zarathushtra Agent Mode")
    print("Type 'exit' or 'quit' to end conversation\n")

    # Get configuration
    config = get_config()

    # Initialize agent with config
    agent = AgentManager(config=config)
    agent.conversation_manager.enter_conversation()  # Start in conversation mode

    while True:
        try:
            # Get user input
            user_input = input("You: ")

            if not user_input.strip():
                continue

            if user_input.lower() in ["exit", "quit", "bye", "goodbye"]:
                print("Goodbye!")
                break

            # Process with agent
            result = await agent.process_async(user_input)
            response = result["response"]

            print(f"Zara: {response}")

            # Show tool usage if any
            if result.get("tool_results"):
                tools_used = [r["tool"] for r in result["tool_results"]]
                print(f"[Tools used: {', '.join(tools_used)}]")

            print()  # Blank line for readability

        except KeyboardInterrupt:
            print("\nGoodbye!")
            break
        except Exception as e:
            print(f"Error: {e}", file=sys.stderr)
            continue


def main():
    """Entry point for agent CLI"""
    try:
        asyncio.run(chat_loop())
        return 0
    except KeyboardInterrupt:
        return 0
    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
