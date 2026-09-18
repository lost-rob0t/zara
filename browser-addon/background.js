const ext = globalThis.browser ?? globalThis.chrome;

const DEFAULT_BRIDGE_URL = "ws://127.0.0.1:8765/v1/browser";
const RECONNECT_DELAY_MS = 2000;
const KEEPALIVE_MS = 20000;

let socket = null;
let reconnectTimer = null;
let keepaliveTimer = null;

function errorText(error) {
  if (error instanceof Error) {
    return error.message.slice(0, 500);
  }
  return String(error).slice(0, 500);
}

async function settings() {
  const stored = await ext.storage.local.get(["bridgeUrl", "token"]);
  return {
    bridgeUrl: String(stored.bridgeUrl || DEFAULT_BRIDGE_URL),
    token: String(stored.token || ""),
  };
}

function setBadge(connected) {
  if (!ext.action?.setBadgeText) {
    return;
  }
  ext.action.setBadgeText({ text: connected ? "ON" : "" }).catch(() => {});
}

function clearTimers() {
  if (reconnectTimer !== null) {
    clearTimeout(reconnectTimer);
    reconnectTimer = null;
  }
  if (keepaliveTimer !== null) {
    clearInterval(keepaliveTimer);
    keepaliveTimer = null;
  }
}

function scheduleReconnect() {
  if (reconnectTimer !== null) {
    return;
  }
  reconnectTimer = setTimeout(() => {
    reconnectTimer = null;
    connect().catch(() => scheduleReconnect());
  }, RECONNECT_DELAY_MS);
}

async function connect() {
  const config = await settings();
  if (config.token.length < 24) {
    setBadge(false);
    scheduleReconnect();
    return;
  }

  if (socket && (socket.readyState === WebSocket.OPEN || socket.readyState === WebSocket.CONNECTING)) {
    return;
  }

  const ws = new WebSocket(config.bridgeUrl);
  socket = ws;

  ws.onopen = () => {
    ws.send(JSON.stringify({ type: "hello", token: config.token, protocol: 1 }));
  };

  ws.onmessage = async (event) => {
    let message;
    try {
      message = JSON.parse(event.data);
    } catch {
      return;
    }

    if (message?.type === "ready") {
      setBadge(true);
      if (keepaliveTimer !== null) {
        clearInterval(keepaliveTimer);
      }
      keepaliveTimer = setInterval(() => {
        if (ws.readyState === WebSocket.OPEN) {
          ws.send(JSON.stringify({ type: "ping" }));
        }
      }, KEEPALIVE_MS);
      return;
    }

    if (message?.type !== "call" || typeof message.id !== "string") {
      return;
    }

    try {
      const result = await dispatch(message.action, message.args || {});
      if (ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({ type: "result", id: message.id, ok: true, result }));
      }
    } catch (error) {
      if (ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({
          type: "result",
          id: message.id,
          ok: false,
          error: errorText(error),
        }));
      }
    }
  };

  ws.onclose = () => {
    if (socket === ws) {
      socket = null;
    }
    clearTimers();
    setBadge(false);
    scheduleReconnect();
  };

  ws.onerror = () => {
    setBadge(false);
  };
}

function requireHttpUrl(raw) {
  const value = String(raw || "").trim();
  const url = new URL(value);
  if (url.protocol !== "http:" && url.protocol !== "https:") {
    throw new Error("browser URLs must use http or https");
  }
  if (value.length > 8192) {
    throw new Error("URL exceeds size limit");
  }
  return value;
}

async function resolveTab(args = {}) {
  if (args.tab_id !== undefined && args.tab_id !== null) {
    const id = Number(args.tab_id);
    if (!Number.isInteger(id) || id <= 0) {
      throw new Error("tab_id must be a positive integer");
    }
    return await ext.tabs.get(id);
  }

  const tabs = await ext.tabs.query({ active: true, currentWindow: true });
  if (!tabs.length) {
    throw new Error("no active browser tab");
  }
  return tabs[0];
}

function tabView(tab) {
  return {
    id: tab.id,
    window_id: tab.windowId,
    active: Boolean(tab.active),
    pinned: Boolean(tab.pinned),
    status: tab.status || null,
    title: tab.title || "",
    url: tab.url || "",
  };
}

function unwrapPageResult(result) {
  if (result && typeof result.__zara_error === "string") {
    throw new Error(result.__zara_error);
  }
  if (!result || typeof result !== "object") {
    throw new Error("browser page returned an invalid result");
  }
  return result;
}

async function sendPageCommand(tab, action, args) {
  if (!tab?.id) {
    throw new Error("tab has no id");
  }

  try {
    const result = await ext.tabs.sendMessage(tab.id, { source: "zara", action, args });
    return unwrapPageResult(result);
  } catch (_firstError) {
    await ext.scripting.executeScript({
      target: { tabId: tab.id },
      files: ["content.js"],
    });
    const result = await ext.tabs.sendMessage(tab.id, { source: "zara", action, args });
    return unwrapPageResult(result);
  }
}

async function dispatch(action, args) {
  switch (action) {
    case "tabs.list": {
      const tabs = await ext.tabs.query({});
      return { tabs: tabs.map(tabView) };
    }

    case "tabs.open": {
      const tab = await ext.tabs.create({
        url: requireHttpUrl(args.url),
        active: args.active !== false,
      });
      return { tab: tabView(tab) };
    }

    case "tabs.navigate": {
      const tab = await resolveTab(args);
      const updated = await ext.tabs.update(tab.id, {
        url: requireHttpUrl(args.url),
      });
      return { tab: tabView(updated) };
    }

    case "page.read": {
      const tab = await resolveTab(args);
      return await sendPageCommand(tab, "read", {
        max_chars: args.max_chars,
      });
    }

    case "page.elements": {
      const tab = await resolveTab(args);
      return await sendPageCommand(tab, "elements", {
        max_items: args.max_items,
      });
    }

    case "page.extract": {
      const tab = await resolveTab(args);
      return await sendPageCommand(tab, "extract", {
        selector: args.selector,
        max_chars: args.max_chars,
      });
    }

    case "page.click": {
      const tab = await resolveTab(args);
      return await sendPageCommand(tab, "click", {
        selector: args.selector,
      });
    }

    case "page.type": {
      const tab = await resolveTab(args);
      return await sendPageCommand(tab, "type", {
        selector: args.selector,
        text: args.text,
        clear: args.clear !== false,
      });
    }

    case "page.submit": {
      const tab = await resolveTab(args);
      return await sendPageCommand(tab, "submit", {
        selector: args.selector,
      });
    }

    case "page.screenshot": {
      const tab = await resolveTab(args);
      if (!tab.active) {
        throw new Error("screenshot requires the requested tab to be active");
      }
      const dataUrl = await ext.tabs.captureVisibleTab(tab.windowId, {
        format: "png",
      });
      return {
        tab: tabView(tab),
        mime_type: "image/png",
        data_url: dataUrl,
      };
    }

    default:
      throw new Error("unsupported Zara browser action");
  }
}

ext.runtime.onInstalled.addListener(() => {
  connect().catch(() => scheduleReconnect());
});

if (ext.runtime.onStartup) {
  ext.runtime.onStartup.addListener(() => {
    connect().catch(() => scheduleReconnect());
  });
}

ext.storage.onChanged.addListener((changes, areaName) => {
  if (areaName !== "local" || (!changes.bridgeUrl && !changes.token)) {
    return;
  }
  if (socket) {
    try {
      socket.close();
    } catch {
      socket = null;
    }
  }
  clearTimers();
  connect().catch(() => scheduleReconnect());
});

connect().catch(() => scheduleReconnect());
