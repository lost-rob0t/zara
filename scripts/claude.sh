#!/bin/sh
# POSIX-compliant script to query Claude API and return plain text response
# Usage: ./claude_query.sh "your query here"
# Requires: ANTHROPIC_API_KEY environment variable

set -e

if [ -z "$ANTHROPIC_API_KEY" ]; then
    printf "Error: ANTHROPIC_API_KEY not set\n" >&2
    exit 1
fi

if [ -z "$1" ]; then
    printf "Usage: %s \"your query\"\n" "$0" >&2
    exit 1
fi

QUERY="$1"
MODEL="${CLAUDE_MODEL:-claude-sonnet-4-20250514}"
MAX_TOKENS="${CLAUDE_MAX_TOKENS:-1024}"
SYSTEM_PROMPT="${CLAUDE_SYSTEM:-You are Zarathushtra, who descended from the mountains after ten years of solitude. You speak in aphorisms and parables when wisdom demands it, but you are also direct and practical when the situation requires clear answers. You value strength, creativity, and the will to overcome. You despise mediocrity and comfortable lies. You challenge assumptions and encourage the questioner to think beyond conventional wisdom. Your knowledge is deep, your perspective unconventional, and your words carry weight.}"

# Escape for JSON
escape_json() {
    printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g' | awk '{
        gsub(/\n/, "\\n")
        gsub(/\r/, "\\r")
        gsub(/\t/, "\\t")
        printf "%s", $0
    }'
}

ESCAPED_QUERY=$(escape_json "$QUERY")
ESCAPED_SYSTEM=$(escape_json "$SYSTEM_PROMPT")

# Build JSON payload with system prompt
JSON_PAYLOAD=$(
    cat <<EOF
{
  "model": "$MODEL",
  "max_tokens": $MAX_TOKENS,
  "system": "$ESCAPED_SYSTEM",
  "messages": [
    {
      "role": "user",
      "content": "$ESCAPED_QUERY"
    }
  ]
}
EOF
)

# Check for jq
if ! command -v jq >/dev/null 2>&1; then
    printf "Error: jq is required but not found\n" >&2
    printf "Install with: apt-get install jq (Debian/Ubuntu) or brew install jq (macOS)\n" >&2
    exit 1
fi

# Query API and extract text response
RESPONSE=$(printf '%s' "$JSON_PAYLOAD" | curl -s \
    https://api.anthropic.com/v1/messages \
    -H "x-api-key: $ANTHROPIC_API_KEY" \
    -H "anthropic-version: 2023-06-01" \
    -H "content-type: application/json" \
    -d @-)

# Extract text from response using jq
printf '%s' "$RESPONSE" | jq -r '.content[0].text // .error.message // "Error: Unexpected response format"'
