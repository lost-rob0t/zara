"""
Conversation memory manager with optional ChromaDB persistence.
"""

from typing import Dict, List, Tuple, Optional
import uuid

try:
    import chromadb
    _CHROMADB_AVAILABLE = True
except ImportError:
    chromadb = None
    _CHROMADB_AVAILABLE = False


class MemoryManager:
    """Manage conversational memory sessions."""

    def __init__(self, collection_name: str = "zara_memory") -> None:
        self.sessions: Dict[str, List[Tuple[str, str]]] = {}
        self.collection_name = collection_name
        if _CHROMADB_AVAILABLE:
            self._client = chromadb.Client()
            self._collection = self._client.get_or_create_collection(name=collection_name)
        else:
            self._memories: List[Tuple[str, str]] = []

    def start_session(self) -> str:
        session_id = str(uuid.uuid4())
        self.sessions[session_id] = []
        return session_id

    def add_message(self, session_id: str, role: str, content: str) -> None:
        self.sessions.setdefault(session_id, []).append((role, content))

    def summarise_session(self, session_id: str, summary_text: Optional[str] = None) -> Optional[str]:
        messages = self.sessions.get(session_id)
        if not messages:
            return None

        summary = summary_text.strip() if summary_text else "\n".join(
            f"{role}: {content}" for role, content in messages
        )

        if _CHROMADB_AVAILABLE:
            self._collection.upsert(documents=[summary], ids=[session_id])
        else:
            self._memories = [(sid, s) for sid, s in self._memories if sid != session_id]
            self._memories.append((session_id, summary))

        return summary
