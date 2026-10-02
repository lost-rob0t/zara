from __future__ import annotations

import asyncio
from html import escape
from importlib import metadata as importlib_metadata
import threading
from typing import Any

from aiohttp import web

from zara.plugins import PluginMetadata, ServicePlugin
from zara.themes import (
    THEME_REGISTRY,
    active_theme_key,
    resolve_theme,
    theme_css_variables,
)


DEFAULT_HOST = "127.0.0.1"
DEFAULT_PORT = 8787
MAX_RESPONSE_BYTES = 512 * 1024


def require_safe_bind(host: str, *, allow_remote: bool) -> str:
    normalized = str(host).strip()
    if not normalized:
        raise ValueError("beta-console host must not be empty")
    if normalized in {"127.0.0.1", "::1", "localhost"}:
        return normalized
    if not allow_remote:
        raise ValueError(
            "beta-console remote bind requires [plugins.beta-console].allow_remote=true"
        )
    return normalized


def _bounded_port(value: object) -> int:
    if isinstance(value, bool):
        raise ValueError("beta-console port must be an integer")
    try:
        port = int(value)
    except (TypeError, ValueError) as error:
        raise ValueError("beta-console port must be an integer") from error
    if not 1 <= port <= 65535:
        raise ValueError("beta-console port must be between 1 and 65535")
    return port


def _zara_version() -> str:
    try:
        return importlib_metadata.version("zara")
    except importlib_metadata.PackageNotFoundError:
        return "dev"


def build_theme_payload(theme_key: str | None) -> dict[str, Any]:
    theme = resolve_theme(theme_key)
    return {
        "selected": theme.key,
        "theme": {
            "key": theme.key,
            "label": theme.label,
            "description": theme.description,
            "colors": dict(theme.colors),
            "css_variables": theme_css_variables(theme.key),
        },
        "available": list(THEME_REGISTRY),
    }


def render_beta_console(
    *,
    theme_key: str | None,
    runtime_state: str,
    runtime_alive: bool,
    version: str,
) -> str:
    theme = resolve_theme(theme_key)
    variables = ";".join(
        f"{name}:{value}" for name, value in theme_css_variables(theme.key).items()
    )
    live_label = "telemetry live" if runtime_alive else "runtime offline"
    live_class = "live" if runtime_alive else "offline"
    values = {
        "__THEME_VARIABLES__": variables,
        "__THEME_LABEL__": escape(theme.label),
        "__RUNTIME_STATE__": escape(str(runtime_state)),
        "__LIVE_LABEL__": live_label,
        "__LIVE_CLASS__": live_class,
        "__VERSION__": escape(str(version)),
    }
    rendered = _BETA_CONSOLE_HTML
    for marker, value in values.items():
        rendered = rendered.replace(marker, value)
    return rendered


class BetaConsolePlugin(ServicePlugin):
    """Read-only first-party beta tester console on the canonical plugin runtime."""

    enabled_by_default = False
    metadata = PluginMetadata(
        name="beta-console",
        version="0.1.0",
        description="Local beta tester dashboard and diagnostics console.",
    )

    def __init__(self) -> None:
        self._runtime = None
        self._host = DEFAULT_HOST
        self._port = DEFAULT_PORT
        self._theme_key = "signal-cabin"
        self._ready = threading.Event()
        self._startup_error: BaseException | None = None

    def start(self, runtime) -> None:
        configuration = dict(runtime.configuration)
        allow_remote = configuration.get("allow_remote", False)
        if not isinstance(allow_remote, bool):
            raise ValueError("beta-console allow_remote must be true or false")

        self._host = require_safe_bind(
            configuration.get("host", DEFAULT_HOST),
            allow_remote=allow_remote,
        )
        self._port = _bounded_port(configuration.get("port", DEFAULT_PORT))

        selected = configuration.get("theme", "inherit")
        if not isinstance(selected, str):
            raise ValueError("beta-console theme must be a string")
        selected_key = active_theme_key() if selected == "inherit" else selected
        self._theme_key = resolve_theme(selected_key).key
        self._runtime = runtime
        self._ready.clear()
        self._startup_error = None

        runtime.start_worker("beta-console", self._worker)
        if not self._ready.wait(timeout=3.0):
            raise RuntimeError("beta console did not become ready")
        if self._startup_error is not None:
            raise RuntimeError(f"beta console failed to start: {self._startup_error}")

    def stop(self) -> None:
        # PluginRuntime owns the managed worker stop event and join deadline.
        # Keeping stop() side-effect free avoids a second lifecycle authority.
        return None

    def _worker(self, stop_event: threading.Event) -> None:
        try:
            asyncio.run(self._serve(stop_event))
        except BaseException as error:
            self._startup_error = error
            self._ready.set()
            raise

    async def _serve(self, stop_event: threading.Event) -> None:
        app = web.Application(client_max_size=64 * 1024)
        app.router.add_get("/", self._http_index)
        app.router.add_get("/beta", self._http_index)
        app.router.add_get("/api/v1/status", self._http_status)
        app.router.add_get("/api/v1/theme", self._http_theme)

        runner = web.AppRunner(app, access_log=None)
        try:
            await runner.setup()
            site = web.TCPSite(runner, self._host, self._port)
            await site.start()
            self._ready.set()
            while not stop_event.is_set():
                await asyncio.sleep(0.05)
        finally:
            await runner.cleanup()

    @staticmethod
    def _response_headers() -> dict[str, str]:
        return {
            "Cache-Control": "no-store",
            "Content-Security-Policy": (
                "default-src 'self'; style-src 'unsafe-inline'; "
                "script-src 'none'; connect-src 'self'; img-src 'self' data:; "
                "base-uri 'none'; frame-ancestors 'none'"
            ),
            "X-Content-Type-Options": "nosniff",
            "Referrer-Policy": "no-referrer",
        }

    async def _http_index(self, _request: web.Request | None) -> web.Response:
        runtime = self._runtime
        status = runtime.status if runtime is not None else None
        rendered = render_beta_console(
            theme_key=self._theme_key,
            runtime_state=getattr(status, "state", "unknown"),
            runtime_alive=bool(getattr(status, "alive", False)),
            version=_zara_version(),
        )
        if len(rendered.encode("utf-8")) > MAX_RESPONSE_BYTES:
            raise web.HTTPInternalServerError(text="beta console response exceeded limit")
        return web.Response(
            text=rendered,
            content_type="text/html",
            charset="utf-8",
            headers=self._response_headers(),
        )

    async def _http_status(self, _request: web.Request | None) -> web.Response:
        runtime = self._runtime
        status = runtime.status if runtime is not None else None
        return web.json_response(
            {
                "plugin": self.metadata.name,
                "plugin_version": self.metadata.version,
                "zara_version": _zara_version(),
                "runtime": {
                    "state": getattr(status, "state", "unknown"),
                    "alive": bool(getattr(status, "alive", False)),
                },
                "theme": resolve_theme(self._theme_key).key,
            },
            headers=self._response_headers(),
        )

    async def _http_theme(self, _request: web.Request | None) -> web.Response:
        return web.json_response(
            build_theme_payload(self._theme_key),
            headers=self._response_headers(),
        )


def create_plugin() -> BetaConsolePlugin:
    return BetaConsolePlugin()


_BETA_CONSOLE_HTML = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Zara Beta Console</title>
<style>
:root{__THEME_VARIABLES__;font-family:Inter,system-ui,sans-serif}
*{box-sizing:border-box} body{margin:0;background:var(--zara-ground);color:var(--zara-text)}
main{max-width:1180px;margin:auto;padding:28px}.top{display:flex;justify-content:space-between;gap:20px;align-items:center}
.eyebrow,.meta{font:12px ui-monospace,monospace;color:var(--zara-text-muted)}
h1{margin:5px 0 0}.badge{padding:7px 10px;border:1px solid var(--zara-line);border-radius:999px}.badge.live{color:var(--zara-primary)}.badge.offline{color:var(--zara-danger)}
.grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px;margin-top:24px}.card{background:var(--zara-panel-deep);border:1px solid var(--zara-line);border-radius:12px;padding:18px}.wide{grid-column:1/-1}
h2{font-size:15px;margin:0 0 10px}.metric{font-size:24px;font-weight:700}.muted{color:var(--zara-text-muted)}
@media(max-width:700px){main{padding:18px}.top{align-items:flex-start;flex-direction:column}.grid{grid-template-columns:1fr}.wide{grid-column:auto}}
</style>
</head>
<body><main>
<div class="top"><div><div class="eyebrow">Beta tester console · __VERSION__</div><h1>System overview</h1></div><div class="badge __LIVE_CLASS__">__LIVE_LABEL__</div></div>
<div class="grid">
<section class="card"><h2>Runtime</h2><div class="metric">__RUNTIME_STATE__</div><div class="meta">Theme: __THEME_LABEL__</div></section>
<section class="card"><h2>Test missions</h2><p class="muted">Run bounded acceptance missions from the canonical Zara test stack.</p></section>
<section class="card"><h2>Diagnostics</h2><p class="muted">Read-only runtime health. Private prompts, credentials and transcripts are never exposed.</p></section>
<section class="card"><h2>Beta builds</h2><p class="muted">Inspect the installed Zara version and current runtime state.</p></section>
<section class="card wide"><h2>Issues</h2><p class="muted">Use exact build and diagnostic evidence when filing regressions.</p></section>
</div>
</main></body></html>"""
