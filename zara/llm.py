"""
LLM Client - handles chat history and API calls
Supports: Anthropic, OpenAI, Ollama
"""

import os
import asyncio
import json
import aiohttp
from typing import List, Dict, Optional


class ChatHistory:
    """Manages conversation history with sliding window"""
    
    def __init__(self, max_length: int = 20):
        self.messages: List[Dict] = []
        self.max_length = max_length
    
    def add_user_message(self, content: str):
        """Add user message to history"""
        self.messages.append({
            "role": "user",
            "content": content
        })
        self._trim()
    
    def add_assistant_message(self, content: str):
        """Add assistant message to history"""
        self.messages.append({
            "role": "assistant", 
            "content": content
        })
        self._trim()
    
    def get_messages(self) -> List[Dict]:
        """Get message history for API"""
        return self.messages.copy()
    
    def _trim(self):
        """Keep only recent messages"""
        if len(self.messages) > self.max_length:
            self.messages = self.messages[-self.max_length:]
    
    def clear(self):
        """Clear history"""
        self.messages.clear()


class LLMClient:
    """
    LLM interface with provider abstraction
    Supports: anthropic, openai, ollama
    """

    def __init__(
        self,
        provider: str = "ollama",
        model: Optional[str] = None,
        endpoint: Optional[str] = None
    ):
        self.provider = provider

        if provider == "anthropic":
            try:
                from anthropic import AsyncAnthropic
            except ImportError:
                raise ValueError("anthropic package not installed. Install via: pip install anthropic")

            api_key = os.getenv("ANTHROPIC_API_KEY")
            if not api_key:
                raise ValueError("ANTHROPIC_API_KEY not set. Set via: export ANTHROPIC_API_KEY=sk-...")
            self.client = AsyncAnthropic(api_key=api_key)
            self.model = model or "claude-sonnet-4-20250514"

        elif provider == "openai":
            api_key = os.getenv("OPENAI_API_KEY")
            if not api_key:
                raise ValueError("OPENAI_API_KEY not set. Set via: export OPENAI_API_KEY=sk-...")
            self.api_key = api_key
            self.model = model or "gpt-4o-mini"
            self.endpoint = endpoint or "https://api.openai.com/v1/chat/completions"

        elif provider == "ollama":
            # Ollama runs locally, no API key needed
            self.model = model or "llama3.2"
            self.endpoint = endpoint or "http://localhost:11434/api/chat"

        else:
            raise ValueError(f"Unsupported provider: {provider}. Use: anthropic, openai, or ollama")
    
    async def query_async(
        self,
        prompt: str,
        system_prompt: Optional[str] = None,
        chat_history: Optional[List[Dict]] = None,
        max_tokens: int = 1024
    ) -> str:
        """
        Send query to LLM and get response
        """
        if self.provider == "anthropic":
            return await self._query_anthropic(
                prompt, system_prompt, chat_history, max_tokens
            )
        elif self.provider == "openai":
            return await self._query_openai(
                prompt, system_prompt, chat_history, max_tokens
            )
        elif self.provider == "ollama":
            return await self._query_ollama(
                prompt, system_prompt, chat_history, max_tokens
            )
        else:
            raise ValueError(f"Unsupported provider: {self.provider}")
    
    async def _query_anthropic(
        self,
        prompt: str,
        system_prompt: Optional[str],
        chat_history: Optional[List[Dict]],
        max_tokens: int
    ) -> str:
        """Query Anthropic API"""
        import time

        # Build messages
        messages = []
        if chat_history:
            messages.extend(chat_history)

        messages.append({
            "role": "user",
            "content": prompt
        })

        # Default system prompt
        if system_prompt is None:
            system_prompt = self._default_system_prompt()

        # Make API call
        try:
            start_time = time.time()
            response = await self.client.messages.create(
                model=self.model,
                max_tokens=max_tokens,
                system=system_prompt,
                messages=messages
            )
            elapsed = time.time() - start_time

            print(f"[Anthropic] Response time: {elapsed:.2f}s (model: {self.model})")

            return response.content[0].text

        except Exception as e:
            print(f"Anthropic API error ({self.model}): {e}")
            return f"Error ({self.model}): {str(e)}"

    async def _query_openai(
        self,
        prompt: str,
        system_prompt: Optional[str],
        chat_history: Optional[List[Dict]],
        max_tokens: int
    ) -> str:
        """Query OpenAI API"""
        import time

        # Build messages
        messages = []

        # Add system prompt
        if system_prompt is None:
            system_prompt = self._default_system_prompt()
        messages.append({
            "role": "system",
            "content": system_prompt
        })

        # Add chat history
        if chat_history:
            messages.extend(chat_history)

        # Add current prompt
        messages.append({
            "role": "user",
            "content": prompt
        })

        # Make API call
        try:
            start_time = time.time()
            async with aiohttp.ClientSession() as session:
                headers = {
                    "Authorization": f"Bearer {self.api_key}",
                    "Content-Type": "application/json"
                }
                payload = {
                    "model": self.model,
                    "messages": messages,
                    "max_tokens": max_tokens
                }

                async with session.post(
                    self.endpoint,
                    headers=headers,
                    json=payload,
                    timeout=aiohttp.ClientTimeout(total=30)
                ) as response:
                    elapsed = time.time() - start_time

                    if response.status != 200:
                        error_text = await response.text()
                        return f"OpenAI API error {response.status}: {error_text}"

                    data = await response.json()
                    print(f"[OpenAI] Response time: {elapsed:.2f}s (model: {self.model})")
                    return data["choices"][0]["message"]["content"]

        except asyncio.TimeoutError:
            return f"Error: {self.model} timeout (30s)"
        except Exception as e:
            print(f"OpenAI API error ({self.model}): {e}")
            return f"Error ({self.model}): {str(e)}"

    async def _query_ollama(
        self,
        prompt: str,
        system_prompt: Optional[str],
        chat_history: Optional[List[Dict]],
        max_tokens: int
    ) -> str:
        """Query Ollama local API"""
        import time

        # Build messages
        messages = []

        # Add system prompt
        if system_prompt is None:
            system_prompt = self._default_system_prompt()
        messages.append({
            "role": "system",
            "content": system_prompt
        })

        # Add chat history
        if chat_history:
            messages.extend(chat_history)

        # Add current prompt
        messages.append({
            "role": "user",
            "content": prompt
        })

        # Make API call
        try:
            start_time = time.time()
            async with aiohttp.ClientSession() as session:
                payload = {
                    "model": self.model,
                    "messages": messages,
                    "stream": False
                }

                async with session.post(
                    self.endpoint,
                    json=payload,
                    timeout=aiohttp.ClientTimeout(total=30)
                ) as response:
                    elapsed = time.time() - start_time

                    if response.status != 200:
                        error_text = await response.text()
                        return f"Ollama API error {response.status}: {error_text}. Is Ollama running? (ollama serve)"

                    data = await response.json()
                    print(f"[Ollama] Response time: {elapsed:.2f}s (model: {self.model})")
                    return data["message"]["content"]

        except aiohttp.ClientConnectorError:
            return f"Error: Cannot connect to Ollama ({self.model}). Is it running? Start with: ollama serve"
        except asyncio.TimeoutError:
            return f"Error: {self.model} timeout (30s)"
        except Exception as e:
            print(f"Ollama API error ({self.model}): {e}")
            return f"Error ({self.model}): {str(e)}"

    def _default_system_prompt(self) -> str:
        """Default system prompt for Zarathustra"""
        return (
            "You are Zarathustra, an agentic large language model living inside a voice assistant. "
            "Your goal is to be helpful, precise, and safe for the user. You should use available "
            "tools when they help accomplish the user's request, including reading or writing files "
            "when explicitly asked. You speak with wisdom and directness, valuing strength, creativity, "
            "and the will to overcome. Be helpful and philosophical."
        )
    
    def query(self, prompt: str, **kwargs) -> str:
        """Synchronous wrapper"""
        return asyncio.run(self.query_async(prompt, **kwargs))


if __name__ == "__main__":
    # Test all providers
    async def test():
        import sys

        provider = sys.argv[1] if len(sys.argv) > 1 else "ollama"
        print(f"Testing {provider} provider...\n")

        client = LLMClient(provider=provider)
        history = ChatHistory()

        # First message
        print("Query: Hello, who are you?")
        response = await client.query_async(
            "Hello, who are you?",
            chat_history=history.get_messages()
        )
        print(f"Response: {response}\n")

        history.add_user_message("Hello, who are you?")
        history.add_assistant_message(response)

        # Follow-up with context
        print("Query: What is your philosophy?")
        response = await client.query_async(
            "What is your philosophy?",
            chat_history=history.get_messages()
        )
        print(f"Response: {response}\n")

        history.add_user_message("What is your philosophy?")
        history.add_assistant_message(response)

        # Test context retention
        print("Query: What was my first question?")
        response = await client.query_async(
            "What was my first question?",
            chat_history=history.get_messages()
        )
        print(f"Response: {response}")

    asyncio.run(test())
