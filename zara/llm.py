"""Bounded HTTP clients for supported LLM providers."""

import asyncio
import logging
import os
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

import aiohttp


logger = logging.getLogger(__name__)

DEFAULT_HISTORY_LENGTH = 20
RETRYABLE_STATUS_CODES = {429, 500, 502, 503, 504}


@dataclass(frozen=True)
class LLMResult:
    provider: str
    model: str
    success: bool
    text: str = ""
    error: str = ""
    error_type: str = ""
    status: Optional[int] = None
    cancelled: bool = False
    attempts: int = 1


class ChatHistory:
    def __init__(self, max_length: int = DEFAULT_HISTORY_LENGTH):
        if max_length < 1:
            raise ValueError("max_length must be positive")
        self.messages: List[Dict[str, str]] = []
        self.max_length = max_length

    def add_user_message(self, content: str) -> None:
        self.messages.append({"role": "user", "content": content})
        self._trim()

    def add_assistant_message(self, content: str) -> None:
        self.messages.append({"role": "assistant", "content": content})
        self._trim()

    def get_messages(self) -> List[Dict[str, str]]:
        return [message.copy() for message in self.messages]

    def _trim(self) -> None:
        self.messages = self.messages[-self.max_length :]

    def clear(self) -> None:
        self.messages.clear()


class LLMClient:
    def __init__(
        self,
        provider: str = "ollama",
        model: Optional[str] = None,
        endpoint: Optional[str] = None,
        api_key: Optional[str] = None,
        connect_timeout: float = 5.0,
        read_timeout: float = 20.0,
        total_timeout: float = 30.0,
        max_retries: int = 2,
        retry_delay: float = 0.1,
        history_limit: int = DEFAULT_HISTORY_LENGTH,
    ):
        if connect_timeout <= 0 or read_timeout <= 0 or total_timeout <= 0:
            raise ValueError("LLM timeouts must be positive")
        if max_retries < 0:
            raise ValueError("max_retries cannot be negative")
        if history_limit < 1:
            raise ValueError("history_limit must be positive")

        self.provider = provider
        self.model, self.endpoint, self.api_key = self._provider_config(
            provider, model, endpoint, api_key
        )
        self.max_retries = max_retries
        self.retry_delay = max(0.0, retry_delay)
        self.history_limit = history_limit
        self.timeout = aiohttp.ClientTimeout(
            total=total_timeout,
            connect=connect_timeout,
            sock_read=read_timeout,
        )
        self._total_timeout = total_timeout
        self._session: Optional[aiohttp.ClientSession] = None

    @staticmethod
    def _provider_config(
        provider: str,
        model: Optional[str],
        endpoint: Optional[str],
        api_key: Optional[str],
    ) -> Tuple[str, str, Optional[str]]:
        if provider == "anthropic":
            resolved_key = api_key or os.getenv("ANTHROPIC_API_KEY")
            if not resolved_key:
                raise ValueError("ANTHROPIC_API_KEY is not set")
            return (
                model or "claude-sonnet-4-20250514",
                endpoint or "https://api.anthropic.com/v1/messages",
                resolved_key,
            )
        if provider == "openai":
            resolved_key = api_key or os.getenv("OPENAI_API_KEY")
            if not resolved_key:
                raise ValueError("OPENAI_API_KEY is not set")
            return (
                model or "gpt-4o-mini",
                endpoint or "https://api.openai.com/v1/chat/completions",
                resolved_key,
            )
        if provider == "ollama":
            return (
                model or "llama3.2",
                endpoint or "http://localhost:11434/api/chat",
                None,
            )
        raise ValueError(
            f"Unsupported provider: {provider}. Use: anthropic, openai, or ollama"
        )

    async def __aenter__(self) -> "LLMClient":
        await self._ensure_session()
        return self

    async def __aexit__(self, exc_type, exc_value, traceback) -> None:
        await self.close()

    async def _ensure_session(self) -> aiohttp.ClientSession:
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession(timeout=self.timeout)
        return self._session

    async def close(self) -> None:
        if self._session is not None:
            await self._session.close()
            self._session = None

    def _messages(
        self,
        prompt: str,
        chat_history: Optional[List[Dict[str, str]]],
    ) -> List[Dict[str, str]]:
        history = list(chat_history or [])[-self.history_limit :]
        messages: List[Dict[str, str]] = []
        for message in history:
            role = message.get("role")
            content = message.get("content")
            if role not in {"user", "assistant"} or not isinstance(content, str):
                continue
            messages.append({"role": role, "content": content})
        messages.append({"role": "user", "content": prompt})
        return messages[-self.history_limit :]

    def serialize_request(
        self,
        prompt: str,
        system_prompt: Optional[str] = None,
        chat_history: Optional[List[Dict[str, str]]] = None,
        max_tokens: int = 1024,
    ) -> Tuple[Dict[str, str], Dict[str, Any]]:
        system = system_prompt or self._default_system_prompt()
        messages = self._messages(prompt, chat_history)
        headers = {"Content-Type": "application/json"}

        if self.provider == "anthropic":
            headers.update(
                {
                    "x-api-key": str(self.api_key),
                    "anthropic-version": "2023-06-01",
                }
            )
            payload = {
                "model": self.model,
                "max_tokens": max_tokens,
                "system": system,
                "messages": messages,
            }
        elif self.provider == "openai":
            headers["Authorization"] = f"Bearer {self.api_key}"
            payload = {
                "model": self.model,
                "messages": [{"role": "system", "content": system}, *messages],
                "max_tokens": max_tokens,
            }
        else:
            payload = {
                "model": self.model,
                "messages": [{"role": "system", "content": system}, *messages],
                "stream": False,
                "options": {"num_predict": max_tokens},
            }
        return headers, payload

    async def query_async(
        self,
        prompt: str,
        system_prompt: Optional[str] = None,
        chat_history: Optional[List[Dict[str, str]]] = None,
        max_tokens: int = 1024,
    ) -> LLMResult:
        headers, payload = self.serialize_request(
            prompt, system_prompt, chat_history, max_tokens
        )
        try:
            async with asyncio.timeout(self._total_timeout):
                return await self._request_with_retries(headers, payload)
        except asyncio.CancelledError:
            return self._error("cancelled", "LLM request was cancelled", cancelled=True)
        except TimeoutError:
            return self._error("timeout", "LLM request timed out")

    async def _request_with_retries(
        self,
        headers: Dict[str, str],
        payload: Dict[str, Any],
    ) -> LLMResult:
        attempts = self.max_retries + 1
        for attempt in range(1, attempts + 1):
            result = await self._request_once(headers, payload, attempt)
            if result.success or not self._retryable(result) or attempt == attempts:
                return result
            if self.retry_delay:
                await asyncio.sleep(self.retry_delay)
        return self._error("internal", "LLM retry loop ended unexpectedly")

    async def _request_once(
        self,
        headers: Dict[str, str],
        payload: Dict[str, Any],
        attempt: int,
    ) -> LLMResult:
        try:
            session = await self._ensure_session()
            async with session.post(
                self.endpoint, headers=headers, json=payload
            ) as response:
                if response.status < 200 or response.status >= 300:
                    detail = (await response.text()).strip()
                    return self._error(
                        "rate_limit" if response.status == 429 else "http",
                        detail or f"HTTP {response.status}",
                        status=response.status,
                        attempts=attempt,
                    )
                try:
                    data = await response.json()
                except (aiohttp.ContentTypeError, ValueError) as error:
                    return self._error("malformed_response", str(error), attempts=attempt)
                return self._parse_response(data, attempt)
        except asyncio.CancelledError:
            raise
        except asyncio.TimeoutError:
            return self._error("timeout", "LLM request timed out", attempts=attempt)
        except aiohttp.ClientError as error:
            return self._error("connection", str(error), attempts=attempt)

    def _parse_response(self, data: Any, attempt: int) -> LLMResult:
        try:
            if self.provider == "anthropic":
                content = data["content"]
                text = next(
                    block["text"]
                    for block in content
                    if block.get("type") == "text" and block.get("text")
                )
            elif self.provider == "openai":
                text = data["choices"][0]["message"]["content"]
            else:
                text = data["message"]["content"]
        except (KeyError, IndexError, StopIteration, TypeError):
            return self._error(
                "malformed_response",
                "LLM response did not match provider schema",
                attempts=attempt,
            )
        if not isinstance(text, str) or not text.strip():
            return self._error(
                "empty_response", "LLM returned an empty response", attempts=attempt
            )
        logger.info("[LLM] %s response received (%s)", self.provider, self.model)
        return LLMResult(
            provider=self.provider,
            model=self.model,
            success=True,
            text=text,
            attempts=attempt,
        )

    def _error(
        self,
        error_type: str,
        error: str,
        status: Optional[int] = None,
        cancelled: bool = False,
        attempts: int = 1,
    ) -> LLMResult:
        return LLMResult(
            provider=self.provider,
            model=self.model,
            success=False,
            error=error,
            error_type=error_type,
            status=status,
            cancelled=cancelled,
            attempts=attempts,
        )

    @staticmethod
    def _retryable(result: LLMResult) -> bool:
        return result.error_type in {"connection", "timeout", "rate_limit"} or (
            result.status in RETRYABLE_STATUS_CODES
        )

    @staticmethod
    def _default_system_prompt() -> str:
        return (
            "You are Zarathustra, an agentic large language model living inside a voice assistant. "
            "Your goal is to be helpful, precise, and safe for the user. You should use available "
            "tools when they help accomplish the user's request, including reading or writing files "
            "when explicitly asked. You speak with wisdom and directness, valuing strength, creativity, "
            "and the will to overcome. Be helpful and philosophical."
        )

    def query(self, prompt: str, **kwargs: Any) -> LLMResult:
        return asyncio.run(self.query_async(prompt, **kwargs))
