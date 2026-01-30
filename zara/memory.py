"""
Conversation memory manager with optional ChromaDB persistence.
"""

import os
import uuid
from datetime import datetime
from typing import Any, Callable, Dict, Iterable, List, Optional, Tuple
import json
import urllib.request
import urllib.error

try:
    import chromadb
    _CHROMADB_AVAILABLE = True
except ImportError:
    chromadb = None
    _CHROMADB_AVAILABLE = False

try:
    from chromadb.utils import embedding_functions
    _EMBEDDING_FUNCTIONS_AVAILABLE = True
except Exception:
    embedding_functions = None
    _EMBEDDING_FUNCTIONS_AVAILABLE = False

EmbeddingFunction = Callable[[List[str]], List[List[float]]]


def _normalize_ollama_base_url(url: str) -> str:
    cleaned = (url or "").strip().rstrip("/")
    if not cleaned:
        return "http://localhost:11434"
    if cleaned.endswith("/api/chat"):
        return cleaned[: -len("/api/chat")]
    if cleaned.endswith("/api/generate"):
        return cleaned[: -len("/api/generate")]
    if cleaned.endswith("/api/embeddings"):
        return cleaned[: -len("/api/embeddings")]
    return cleaned


def _ollama_embed(texts: List[str], base_url: str, model: str) -> List[List[float]]:
    url = _normalize_ollama_base_url(base_url) + "/api/embeddings"
    vectors: List[List[float]] = []

    for text in texts:
        payload = json.dumps({"model": model, "prompt": text}).encode("utf-8")
        request = urllib.request.Request(
            url,
            data=payload,
            headers={"Content-Type": "application/json"},
            method="POST",
        )
        with urllib.request.urlopen(request, timeout=10) as response:
            body = json.loads(response.read().decode("utf-8"))
        embedding = body.get("embedding")
        if not isinstance(embedding, list) or not embedding:
            raise ValueError("Invalid embedding response")
        vectors.append([float(v) for v in embedding])

    return vectors


class MemoryManager:
    """Manage conversational memory sessions."""

    def __init__(
        self,
        collection_name: str = "zara_memory",
        persist_directory: Optional[str] = None,
        embedding_model: str = "all-MiniLM-L6-v2",
        enabled: bool = True,
        top_k: int = 5,
        embedding_function: Optional[EmbeddingFunction] = None,
        settings: Optional[Dict[str, Any]] = None,
    ) -> None:
        self.sessions: Dict[str, List[Tuple[str, str]]] = {}
        self.collection_name = collection_name
        self.persist_directory = persist_directory
        self.embedding_model = embedding_model
        self.enabled = enabled
        self.top_k = top_k
        self.settings = settings or {}
        self._memories: List[Dict[str, Any]] = []
        self._collection = None
        self._embedding_function = None
        self.current_session_id: Optional[str] = None

        if self.enabled and _CHROMADB_AVAILABLE:
            self._setup_chroma(embedding_function)

    def _setup_chroma(self, embedding_function: Optional[EmbeddingFunction]) -> None:
        self._embedding_function = embedding_function or self._build_embedding_function()
        try:
            client = self._build_client()
            if self._embedding_function is not None:
                self._collection = client.get_or_create_collection(
                    name=self.collection_name,
                    embedding_function=self._embedding_function,
                )
            else:
                self._collection = client.get_or_create_collection(name=self.collection_name)
        except Exception:
            self._collection = None

    def _build_client(self):
        if not self.persist_directory:
            return chromadb.Client()
        path = os.path.expanduser(os.path.expandvars(self.persist_directory))
        os.makedirs(path, exist_ok=True)
        return chromadb.PersistentClient(path=path)

    def _build_embedding_function(self) -> Optional[EmbeddingFunction]:
        memory_cfg = self.settings
        backend = str(memory_cfg.get("embedding_backend", "ollama")).strip().lower()

        if backend == "ollama":
            model = str(memory_cfg.get("embedding_model", "nomic-embed-text")).strip() or "nomic-embed-text"
            base_url = str(memory_cfg.get("ollama_url") or memory_cfg.get("ollama_base_url") or "").strip()
            if not base_url:
                base_url = str(os.getenv("ZARA_MEMORY_OLLAMA_URL") or os.getenv("OLLAMA_HOST") or "").strip()

            def _embed(texts: List[str]) -> List[List[float]]:
                return _ollama_embed(texts, base_url=base_url, model=model)

            return _embed

        if backend == "sentence_transformers" and _EMBEDDING_FUNCTIONS_AVAILABLE:
            try:
                return embedding_functions.SentenceTransformerEmbeddingFunction(
                    model_name=self.embedding_model
                )
            except Exception:
                return None

        return None

    def start_session(self) -> str:
        session_id = str(uuid.uuid4())
        self.sessions[session_id] = []
        self.current_session_id = session_id
        return session_id

    def add_message(self, session_id: str, role: str, content: str) -> None:
        self.sessions.setdefault(session_id, []).append((role, content))

    def summarise_session(
        self,
        session_id: str,
        summary_text: Optional[str] = None,
        source: str = "wake",
    ) -> Optional[str]:
        messages = self.sessions.get(session_id)
        if not messages:
            return None

        summary = summary_text.strip() if summary_text else "\n".join(
            f"{role}: {content}" for role, content in messages
        )

        if summary:
            self.store_session_summary(session_id, summary, source=source)

        return summary

    def remember_fact(
        self,
        text: str,
        tags: Optional[List[str]] = None,
        session_id: Optional[str] = None,
        source: str = "agent",
    ) -> Optional[str]:
        return self._store_memory(
            text=text,
            kind="fact",
            tags=tags,
            session_id=session_id,
            source=source,
        )

    def store_session_summary(
        self,
        session_id: str,
        summary_text: str,
        source: str = "wake",
    ) -> Optional[str]:
        if not summary_text:
            return None
        return self._store_memory(
            text=summary_text,
            kind="summary",
            tags=["summary"],
            session_id=session_id,
            source=source,
        )

    def retrieve(
        self,
        query: str,
        k: Optional[int] = None,
        include_kinds: Optional[Iterable[str]] = ("fact", "summary"),
        tags: Optional[List[str]] = None,
    ) -> List[Dict[str, Any]]:
        if not self.enabled:
            return []
        query = query.strip()
        if not query:
            return []

        limit = int(k or self.top_k)
        kinds = set(include_kinds) if include_kinds else None
        tag_list = self._normalize_tags(tags)

        if self._collection is None:
            return self._retrieve_local(query, limit, kinds, tag_list)
        return self._retrieve_chroma(query, limit, kinds, tag_list)

    def _retrieve_chroma(
        self,
        query: str,
        limit: int,
        kinds: Optional[set[str]],
        tags: List[str],
    ) -> List[Dict[str, Any]]:
        candidate_count = max(limit * 3, limit)
        result = self._collection.query(
            query_texts=[query],
            n_results=candidate_count,
        )
        documents = (result.get("documents") or [[]])[0]
        ids = (result.get("ids") or [[]])[0]
        metadatas = (result.get("metadatas") or [[]])[0]
        distances = (result.get("distances") or [[]])[0]

        entries = []
        for idx, text in enumerate(documents):
            metadata = metadatas[idx] if idx < len(metadatas) else {}
            if not self._matches_filters(metadata, kinds, tags):
                continue
            entry = {
                "id": ids[idx] if idx < len(ids) else None,
                "text": text,
                "metadata": metadata,
                "score": distances[idx] if idx < len(distances) else None,
            }
            entries.append(entry)

        entries.sort(key=lambda item: item["score"] if item["score"] is not None else 0.0)
        return entries[:limit]

    def _retrieve_local(
        self,
        query: str,
        limit: int,
        kinds: Optional[set[str]],
        tags: List[str],
    ) -> List[Dict[str, Any]]:
        query_lower = query.lower()
        entries = []
        for record in self._memories:
            metadata = record.get("metadata", {})
            if not self._matches_filters(metadata, kinds, tags):
                continue
            text = record.get("text", "")
            if not text:
                continue
            score = text.lower().count(query_lower)
            if score == 0:
                continue
            entries.append(
                {
                    "id": record.get("id"),
                    "text": text,
                    "metadata": metadata,
                    "score": -float(score),
                }
            )

        entries.sort(key=lambda item: item["score"] if item["score"] is not None else 0.0)
        return entries[:limit]

    def _store_memory(
        self,
        text: str,
        kind: str,
        tags: Optional[List[str]],
        session_id: Optional[str],
        source: str,
    ) -> Optional[str]:
        if not self.enabled:
            return None
        text = (text or "").strip()
        if not text:
            return None

        memory_id = str(uuid.uuid4())
        tag_list = self._normalize_tags(tags)
        tag_string = ",".join(tag_list)
        metadata = {
            "kind": kind,
            "tags": tag_string,
            "session_id": session_id or "",
            "source": source,
            "created_at": datetime.utcnow().isoformat(),
        }

        if self._collection is not None:
            try:
                self._collection.upsert(
                    ids=[memory_id],
                    documents=[text],
                    metadatas=[metadata],
                )
                return memory_id
            except Exception:
                pass

        self._memories.append(
            {
                "id": memory_id,
                "text": text,
                "metadata": {
                    **metadata,
                    "tags": tag_list,
                },
            }
        )
        return memory_id

    def _normalize_tags(self, tags: Optional[List[str]]) -> List[str]:
        if not tags:
            return []
        cleaned = [str(tag).strip() for tag in tags if str(tag).strip()]
        return list(dict.fromkeys(cleaned))

    def _matches_filters(
        self,
        metadata: Dict[str, Any],
        kinds: Optional[set[str]],
        tags: List[str],
    ) -> bool:
        if kinds is not None:
            kind_value = metadata.get("kind")
            if kind_value not in kinds:
                return False
        if not tags:
            return True
        stored_tags = metadata.get("tags")
        if isinstance(stored_tags, str):
            stored = {tag.strip() for tag in stored_tags.split(",") if tag.strip()}
        elif isinstance(stored_tags, list):
            stored = {str(tag).strip() for tag in stored_tags if str(tag).strip()}
        else:
            stored = set()
        return all(tag in stored for tag in tags)


def build_memory_manager(
    config: Optional[Dict[str, Any]] = None,
    embedding_function: Optional[EmbeddingFunction] = None,
) -> MemoryManager:
    settings = config or {}
    return MemoryManager(
        collection_name=settings.get("collection_name", "zara_memory"),
        persist_directory=settings.get("persist_directory"),
        embedding_model=settings.get("embedding_model") or "nomic-embed-text",
        enabled=bool(settings.get("enabled", True)),
        top_k=int(settings.get("top_k", 5)),
        embedding_function=embedding_function,
        settings=settings,
    )
