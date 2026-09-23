const ext = globalThis.browser ?? globalThis.chrome;

const bridgeUrl = document.getElementById("bridge-url");
const token = document.getElementById("token");
const status = document.getElementById("status");

async function load() {
  const stored = await ext.storage.local.get(["bridgeUrl", "token"]);
  bridgeUrl.value = stored.bridgeUrl || "ws://127.0.0.1:8765/v1/browser";
  token.value = stored.token || "";
}

async function save() {
  const url = bridgeUrl.value.trim();
  const secret = token.value;
  if (!url.startsWith("ws://") && !url.startsWith("wss://")) {
    status.textContent = "Bridge URL must use ws:// or wss://";
    return;
  }
  if (secret.length < 24) {
    status.textContent = "Token must be at least 24 characters";
    return;
  }
  await ext.storage.local.set({ bridgeUrl: url, token: secret });
  status.textContent = "Saved";
  setTimeout(() => {
    status.textContent = "";
  }, 1500);
}

document.getElementById("save").addEventListener("click", () => {
  save().catch((error) => {
    status.textContent = error instanceof Error ? error.message : String(error);
  });
});

load().catch((error) => {
  status.textContent = error instanceof Error ? error.message : String(error);
});
