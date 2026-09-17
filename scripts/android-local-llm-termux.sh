#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

usage() {
  cat <<'EOF'
usage: android-local-llm-termux.sh MODEL.gguf [PORT]

Starts a phone-local llama.cpp OpenAI-compatible server bound only to 127.0.0.1.
The model file is never downloaded by this script; choose a GGUF whose license and
size you have reviewed for your device.

Environment:
  ZARA_LLM_CONTEXT       context tokens, default 4096
  ZARA_LLM_THREADS       inference threads, default nproc
  ZARA_LLM_GPU_LAYERS    llama.cpp -ngl value, default 0 (CPU-safe baseline)
  ZARA_LLM_BUILD_JOBS    source build jobs, default nproc
  ZARA_LLM_MODEL_NAME    Zara model label, default local-model
  ZARA_LLM_QUANTIZATION  Zara quantization label, default Q4_K_M
EOF
}

MODEL_PATH="${1:-}"
PORT="${2:-8080}"
[[ -n "$MODEL_PATH" ]] || { usage >&2; exit 64; }
[[ -f "$MODEL_PATH" ]] || { echo "model not found: $MODEL_PATH" >&2; exit 66; }
[[ "$PORT" =~ ^[0-9]+$ ]] && (( PORT >= 1 && PORT <= 65535 )) || {
  echo "invalid port: $PORT" >&2
  exit 64
}

NPROC="$(nproc 2>/dev/null || printf '4')"
CONTEXT="${ZARA_LLM_CONTEXT:-4096}"
THREADS="${ZARA_LLM_THREADS:-$NPROC}"
GPU_LAYERS="${ZARA_LLM_GPU_LAYERS:-0}"
BUILD_JOBS="${ZARA_LLM_BUILD_JOBS:-$NPROC}"
MODEL_NAME="${ZARA_LLM_MODEL_NAME:-local-model}"
QUANTIZATION="${ZARA_LLM_QUANTIZATION:-Q4_K_M}"

LLAMA_SERVER="$(command -v llama-server || true)"
if [[ -z "$LLAMA_SERVER" ]]; then
  echo "llama-server not found; trying the Termux llama-cpp package..." >&2
  pkg update -y
  if pkg install -y llama-cpp; then
    LLAMA_SERVER="$(command -v llama-server || true)"
  fi
fi

if [[ -z "$LLAMA_SERVER" ]]; then
  echo "Termux package unavailable; building upstream llama.cpp locally..." >&2
  pkg install -y git cmake clang make libandroid-spawn
  SOURCE_ROOT="${HOME}/.cache/zara/llama.cpp"
  if [[ -d "${SOURCE_ROOT}/.git" ]]; then
    git -C "$SOURCE_ROOT" fetch --depth=1 origin master
    git -C "$SOURCE_ROOT" reset --hard origin/master
  else
    mkdir -p "$(dirname "$SOURCE_ROOT")"
    git clone --depth=1 https://github.com/ggml-org/llama.cpp.git "$SOURCE_ROOT"
  fi
  cmake -S "$SOURCE_ROOT" -B "${SOURCE_ROOT}/build-zara" \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_SHARED_LIBS=OFF \
    -DLLAMA_CURL=OFF
  cmake --build "${SOURCE_ROOT}/build-zara" --target llama-server -j"$BUILD_JOBS"
  LLAMA_SERVER="${SOURCE_ROOT}/build-zara/bin/llama-server"
fi

[[ -x "$LLAMA_SERVER" ]] || {
  echo "llama-server build/install did not produce an executable" >&2
  exit 70
}

cat <<EOF
Zara local LLM endpoint: http://127.0.0.1:${PORT}
In Zara chat, enable it with:
/model use http://127.0.0.1:${PORT} ${MODEL_NAME} ${QUANTIZATION}

Starting llama-server with context=${CONTEXT}, threads=${THREADS}, ngl=${GPU_LAYERS}
EOF

exec "$LLAMA_SERVER" \
  -m "$MODEL_PATH" \
  --host 127.0.0.1 \
  --port "$PORT" \
  -c "$CONTEXT" \
  -t "$THREADS" \
  -ngl "$GPU_LAYERS"
