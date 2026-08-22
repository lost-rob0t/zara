#!/usr/bin/env bash
set -euo pipefail

pytest -q t/test_principal_isolation.py t/test_conversation_store.py t/test_memory.py
