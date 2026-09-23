from __future__ import annotations

import ipaddress
from urllib.parse import urlsplit

from .qwen import Qwen3TTSClient as BaseQwen3TTSClient


class LocalVoiceRegistryQwen3TTSClient(BaseQwen3TTSClient):
    def _require_local_voice_mutation_endpoint(self) -> None:
        parsed = urlsplit(self.base_url)
        host = (parsed.hostname or "").lower().rstrip(".")
        if parsed.scheme not in {"http", "https"} or not host:
            raise RuntimeError(
                "Qwen voice registry mutations require a loopback http(s) endpoint"
            )
        if host == "localhost":
            return
        try:
            address = ipaddress.ip_address(host)
        except ValueError:
            address = None
        if address is not None and address.is_loopback:
            return
        raise RuntimeError(
            "Qwen voice registry mutations require a loopback endpoint; "
            "remote/shared endpoints need provider-side atomic mutation semantics"
        )

    async def register_voice(
        self,
        name: str,
        wav_file_path: str,
        reference_text: str = "",
    ) -> dict:
        self._require_local_voice_mutation_endpoint()
        return await super().register_voice(
            name,
            wav_file_path,
            reference_text=reference_text,
        )

    async def delete_voice(self, name: str) -> dict:
        self._require_local_voice_mutation_endpoint()
        return await super().delete_voice(name)


__all__ = ["LocalVoiceRegistryQwen3TTSClient"]
