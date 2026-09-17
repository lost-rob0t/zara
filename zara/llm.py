"""Bounded HTTP clients for supported LLM providers."""

import asyncio
import json
import logging
from math import isfinite
import os
import re
from dataclasses import dataclass
from typing import Any, AsyncIterator, Dict, List, Mapping, Optional, Sequence, Tuple, Union

import aiohttp


logger = logging.getLogger(__name__)

DEFAULT_HISTORY_LENGTH = 20
RETRYABLE_STATUS_CODES = {429, 500, 502, 503, 504}
OPENROUTER_CHAT_ENDPOINT = "https://openrouter.ai/api/v1/chat/completions"
STARINTEL_CHAT_ENDPOINT = "https://llm.starintel.actor/v1/chat/completions"
DEFAULT_OPENROUTER_QUANTIZATIONS = ("fp16", "bf16", "fp8")
_ALLOWED_OPENROUTER_SORTS = frozenset({"price", "throughput", "latency"})
_ALLOWED_OPENROUTER_DATA_COLLECTION = frozenset({"allow", "deny"})
_ALLOWED_OPENROUTER_POLICY_KEYS = frozenset(
    {
        "sort",
        "allow_fallbacks",
        "quantizations",
        "data_collection",
        "zdr",
        "require_parameters",
        "order",
        "only",
        "ignore",
        "max_price",
    }
)
_ALLOWED_OPENROUTER_PRICE_KEYS = frozenset({"prompt", "completion"})
_OPENROUTER_TOKEN_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._/-]*$")
_OPENROUTER_PROVIDER_RE = re.compile(r"^[a-z0-9][a-z0-9._/-]*$")
_MAX_OPENROUTER_LIST_ITEMS = 32
_MAX_OPENROUTER_TOKEN_CHARS = 128
_MAX_OPENROUTER_USD_PER_MILLION = 1_000_000.0


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


def _openrouter_bool(value: Any, field: str) -> bool:
    if type(value) is not bool:
        raise ValueError(f"OpenRouter {field} must be boolean")
    return value


def _openrouter_text(value: Any, field: str) -> str:
    if not isinstance(value, str):
        raise ValueError(f"OpenRouter {field} must be text")
    normalized = value.strip().lower()
    if not normalized:
        raise ValueError(f"OpenRouter {field} must not be empty")
    return normalized


def _openrouter_tokens(
    values: Any,
    *,
    field: str,
    provider: bool = False,
) -> Tuple[str, ...]:
    if isinstance(values, (str, bytes)) or not isinstance(values, Sequence):
        raise ValueError(f"OpenRouter {field} must be a list")
    if len(values) > _MAX_OPENROUTER_LIST_ITEMS:
        raise ValueError(f"OpenRouter {field} list is too large")

    normalized: List[str] = []
    for raw in values:
        if not isinstance(raw, str):
            raise ValueError(f"OpenRouter {field} entries must be text")
        value = raw.strip().lower()
        if not value:
            raise ValueError(f"OpenRouter {field} entries must not be empty")
        if len(value) > _MAX_OPENROUTER_TOKEN_CHARS:
            raise ValueError(f"OpenRouter {field} entry is too long")
        matcher = _OPENROUTER_PROVIDER_RE if provider else _OPENROUTER_TOKEN_RE
        if matcher.fullmatch(value) is None:
            raise ValueError(f"OpenRouter {field} entry is invalid: {raw}")
        normalized.append(value)

    if len(normalized) != len(set(normalized)):
        raise ValueError(f"OpenRouter {field} contains duplicates")
    return tuple(normalized)


def _openrouter_price(value: Any, field: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"OpenRouter {field} price ceiling must be numeric")
    normalized = float(value)
    if (
        not isfinite(normalized)
        or normalized < 0.0
        or normalized > _MAX_OPENROUTER_USD_PER_MILLION
    ):
        raise ValueError(f"OpenRouter {field} price ceiling is invalid")
    return normalized


@dataclass(frozen=True)
class OpenRouterPolicy:
    """Validated same-model provider-routing policy for OpenRouter."""

    sort: str = "price"
    allow_fallbacks: bool = True
    quantizations: Tuple[str, ...] = DEFAULT_OPENROUTER_QUANTIZATIONS
    data_collection: str = "deny"
    zdr: bool = False
    require_parameters: bool = True
    order: Tuple[str, ...] = ()
    only: Tuple[str, ...] = ()
    ignore: Tuple[str, ...] = ()
    max_prompt_usd_per_million: Optional[float] = None
    max_completion_usd_per_million: Optional[float] = None

    @classmethod
    def from_mapping(
        cls,
        value: Optional[Union["OpenRouterPolicy", Mapping[str, Any]]] = None,
    ) -> "OpenRouterPolicy":
        if isinstance(value, cls):
            raw: Mapping[str, Any] = value.to_input_mapping()
        elif value is None:
            raw = {}
        elif isinstance(value, Mapping):
            raw = value
        else:
            raise ValueError("OpenRouter policy must be a mapping")

        unknown = set(raw) - _ALLOWED_OPENROUTER_POLICY_KEYS
        if unknown:
            names = ", ".join(sorted(str(name) for name in unknown))
            raise ValueError(f"Unsupported OpenRouter policy field(s): {names}")

        sort = _openrouter_text(raw.get("sort", "price"), "sort")
        if sort not in _ALLOWED_OPENROUTER_SORTS:
            raise ValueError(f"Unsupported OpenRouter sort: {sort}")

        data_collection = _openrouter_text(
            raw.get("data_collection", "deny"), "data_collection"
        )
        if data_collection not in _ALLOWED_OPENROUTER_DATA_COLLECTION:
            raise ValueError(
                f"Unsupported OpenRouter data_collection: {data_collection}"
            )

        quantizations = _openrouter_tokens(
            raw.get("quantizations", DEFAULT_OPENROUTER_QUANTIZATIONS),
            field="quantizations",
        )
        if not quantizations:
            raise ValueError("OpenRouter quantizations must not be empty")
        if "unknown" in quantizations:
            raise ValueError(
                "OpenRouter quantization must be explicit; unknown is not routable"
            )

        order = _openrouter_tokens(raw.get("order", ()), field="order", provider=True)
        only = _openrouter_tokens(raw.get("only", ()), field="only", provider=True)
        ignore = _openrouter_tokens(
            raw.get("ignore", ()), field="ignore", provider=True
        )
        overlap = set(only).intersection(ignore)
        if overlap:
            raise ValueError(
                "OpenRouter provider allowlist and blocklist overlap: "
                + ", ".join(sorted(overlap))
            )

        prompt_price: Optional[float] = None
        completion_price: Optional[float] = None
        if "max_price" in raw:
            max_price = raw["max_price"]
            if not isinstance(max_price, Mapping) or not max_price:
                raise ValueError("OpenRouter max_price must be a non-empty mapping")
            unknown_price = set(max_price) - _ALLOWED_OPENROUTER_PRICE_KEYS
            if unknown_price:
                names = ", ".join(sorted(str(name) for name in unknown_price))
                raise ValueError(f"Unsupported OpenRouter max_price field(s): {names}")
            if "prompt" in max_price:
                prompt_price = _openrouter_price(max_price["prompt"], "prompt")
            if "completion" in max_price:
                completion_price = _openrouter_price(
                    max_price["completion"], "completion"
                )

        return cls(
            sort=sort,
            allow_fallbacks=_openrouter_bool(
                raw.get("allow_fallbacks", True), "allow_fallbacks"
            ),
            quantizations=quantizations,
            data_collection=data_collection,
            zdr=_openrouter_bool(raw.get("zdr", False), "zdr"),
            require_parameters=_openrouter_bool(
                raw.get("require_parameters", True), "require_parameters"
            ),
            order=order,
            only=only,
            ignore=ignore,
            max_prompt_usd_per_million=prompt_price,
            max_completion_usd_per_million=completion_price,
        )

    def to_input_mapping(self) -> Dict[str, Any]:
        raw: Dict[str, Any] = {
            "sort": self.sort,
            "allow_fallbacks": self.allow_fallbacks,
            "quantizations": list(self.quantizations),
            "data_collection": self.data_collection,
            "zdr": self.zdr,
            "require_parameters": self.require_parameters,
            "order": list(self.order),
            "only": list(self.only),
            "ignore": list(self.ignore),
        }
        max_price: Dict[str, float] = {}
        if self.max_prompt_usd_per_million is not None:
            max_price["prompt"] = self.max_prompt_usd_per_million
        if self.max_completion_usd_per_million is not None:
            max_price["completion"] = self.max_completion_usd_per_million
        if max_price:
            raw["max_price"] = max_price
        return raw

    def to_wire_dict(self) -> Dict[str, Any]:
        safe = OpenRouterPolicy.from_mapping(self)
        value: Dict[str, Any] = {
            "sort": safe.sort,
            "allow_fallbacks": safe.allow_fallbacks,
            "quantizations": list(safe.quantizations),
            "data_collection": safe.data_collection,
            "zdr": safe.zdr,
            "require_parameters": safe.require_parameters,
        }
        if safe.order:
            value["order"] = list(safe.order)
        if safe.only:
            value["only"] = list(safe.only)
        if safe.ignore:
            value["ignore"] = list(safe.ignore)
        max_price: Dict[str, float] = {}
        if safe.max_prompt_usd_per_million is not None:
            max_price["prompt"] = safe.max_prompt_usd_per_million
        if safe.max_completion_usd_per_million is not None:
            max_price["completion"] = safe.max_completion_usd_per_million
        if max_price:
            value["max_price"] = max_price
        return value


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
        openrouter_policy: Optional[
            Union[OpenRouterPolicy, Mapping[str, Any]]
        ] = None,
    ):
        if connect_timeout <= 0 or read_timeout <= 0 or total_timeout <= 0:
            raise ValueError("LLM timeouts must be positive")
        if max_retries < 0:
            raise ValueError("max_retries cannot be negative")
        if history_limit < 1:
            raise ValueError("history_limit must be positive")
        if not isinstance(provider, str) or not provider.strip():
            raise ValueError("provider must be non-empty text")

        self.provider = provider.strip().lower()
        if self.provider == "openrouter":
            self.openrouter_policy: Optional[OpenRouterPolicy] = (
                OpenRouterPolicy.from_mapping(openrouter_policy)
            )
        elif openrouter_policy is not None:
            raise ValueError("OpenRouter policy may only be set for the openrouter provider")
        else:
            self.openrouter_policy = None

        self.model, self.endpoint, self.api_key = self._provider_config(
            self.provider, model, endpoint, api_key
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
        if provider == "openrouter":
            resolved_key = api_key or os.getenv("OPENROUTER_API_KEY")
            if not resolved_key:
                raise ValueError("OPENROUTER_API_KEY is not set")
            if not isinstance(model, str) or not model.strip():
                raise ValueError("OpenRouter model must be explicit")
            return (model.strip(), endpoint or OPENROUTER_CHAT_ENDPOINT, resolved_key)
        if provider == "starintel":
            resolved_key = api_key or os.getenv("STAR_LLM_ACTOR_TOKEN")
            if not resolved_key:
                raise ValueError("STAR_LLM_ACTOR_TOKEN is not set")
            if not isinstance(model, str) or not model.strip():
                raise ValueError("StarIntel model must be explicit")
            return (model.strip(), endpoint or STARINTEL_CHAT_ENDPOINT, resolved_key)
        if provider == "ollama":
            return (
                model or "llama3.2",
                endpoint or "http://localhost:11434/api/chat",
                None,
            )
        if provider == "llama_cpp":
            return (
                model or "local",
                endpoint or "http://127.0.0.1:11435/v1/chat/completions",
                None,
            )
        raise ValueError(
            f"Unsupported provider: {provider}. "
            "Use: anthropic, openai, openrouter, starintel, ollama, or llama_cpp"
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
        elif self.provider in {"openai", "openrouter", "starintel", "llama_cpp"}:
            if self.api_key:
                headers["Authorization"] = f"Bearer {self.api_key}"
            payload = {
                "model": self.model,
                "messages": [{"role": "system", "content": system}, *messages],
                "max_tokens": max_tokens,
            }
            if self.provider == "openrouter":
                assert self.openrouter_policy is not None
                payload["provider"] = self.openrouter_policy.to_wire_dict()
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

    def serialize_stream_request(
        self,
        prompt: str,
        system_prompt: Optional[str] = None,
        chat_history: Optional[List[Dict[str, str]]] = None,
        max_tokens: int = 1024,
    ) -> Tuple[Dict[str, str], Dict[str, Any]]:
        headers, payload = self.serialize_request(
            prompt, system_prompt, chat_history, max_tokens
        )
        payload["stream"] = True
        return headers, payload

    async def stream_events_async(
        self,
        prompt: str,
        system_prompt: Optional[str] = None,
        chat_history: Optional[List[Dict[str, str]]] = None,
        max_tokens: int = 1024,
    ) -> AsyncIterator["stream_events.LLMStreamEvent"]:
        """Yield provider-neutral typed events for a streamed completion.

        One terminal event (Completed, Cancelled, or Failed) is always the
        last event; failures never fabricate a completion and streaming
        never retries (retries would duplicate already-delivered deltas).
        """
        from zara.agent import stream_events
        from zara.agent.sentence_chunker import SentenceChunker

        headers, payload = self.serialize_stream_request(
            prompt, system_prompt, chat_history, max_tokens
        )
        chunker = SentenceChunker()
        emitted = ""

        async def fail(error_type: str):
            yield stream_events.Failed(error_type=error_type)

        session = await self._ensure_session()
        try:
            async with session.post(
                self.endpoint, headers=headers, json=payload
            ) as response:
                if response.status < 200 or response.status >= 300:
                    detail = (await response.text()).strip()
                    error_type = "rate_limit" if response.status == 429 else "http"
                    logger.warning(
                        "[LLM] stream failed (%s): %s", error_type, detail[:200]
                    )
                    async for event in fail(error_type):
                        yield event
                    return

                async for raw_line in response.content:
                    line = raw_line.decode("utf-8", errors="replace").strip()
                    if not line:
                        continue
                    text = None
                    done = False
                    provider_error = False
                    try:
                        if self.provider in {"openai", "openrouter", "starintel", "llama_cpp"}:
                            if not line.startswith("data:"):
                                continue
                            data = line[len("data:"):].strip()
                            if data == "[DONE]":
                                done = True
                            else:
                                text = json.loads(data)["choices"][0]["delta"].get("content")
                        elif self.provider == "anthropic":
                            if not line.startswith("data:"):
                                continue
                            data = json.loads(line[len("data:"):].strip())
                            if data.get("type") == "message_stop":
                                done = True
                            elif data.get("type") == "error":
                                provider_error = True
                            elif data.get("type") == "content_block_delta":
                                delta = data.get("delta", {})
                                if delta.get("type") == "text_delta":
                                    text = delta.get("text")
                        else:
                            data = json.loads(line)
                            if data.get("done"):
                                done = True
                            else:
                                text = data.get("message", {}).get("content")
                    except (json.JSONDecodeError, KeyError, IndexError, TypeError) as error:
                        logger.warning("[LLM] malformed stream chunk: %s", error)
                        async for event in fail("malformed_response"):
                            yield event
                        return

                    if provider_error:
                        logger.warning("[LLM] stream reported a provider error")
                        async for event in fail("provider_error"):
                            yield event
                        return
                    if done:
                        break
                    if not text:
                        continue
                    emitted += text
                    yield stream_events.TextDelta(text=text)
                    for sentence in chunker.feed(text):
                        yield stream_events.SentenceReady(text=sentence, is_final=False)

            if not emitted.strip():
                async for event in fail("empty_response"):
                    yield event
                return
            final_sentences = chunker.flush()
            for index, sentence in enumerate(final_sentences):
                yield stream_events.SentenceReady(
                    text=sentence,
                    is_final=index == len(final_sentences) - 1,
                )
            yield stream_events.Completed(full_text=emitted)
        except asyncio.CancelledError:
            yield stream_events.Cancelled()
            raise
        except TimeoutError:
            async for event in fail("timeout"):
                yield event
        except aiohttp.ClientError as error:
            logger.warning("[LLM] stream connection failed: %s", error)
            async for event in fail("connection"):
                yield event

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
                    if isinstance(block, dict)
                    and block.get("type") == "text"
                    and block.get("text")
                )
            elif self.provider in {"openai", "openrouter", "starintel", "llama_cpp"}:
                text = data["choices"][0]["message"]["content"]
            else:
                text = data["message"]["content"]
        except (AttributeError, KeyError, IndexError, StopIteration, TypeError):
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
        async def query_and_close() -> LLMResult:
            try:
                return await self.query_async(prompt, **kwargs)
            finally:
                await self.close()

        return asyncio.run(query_and_close())
