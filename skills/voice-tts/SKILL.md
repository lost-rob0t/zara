---
name: voice-tts
description: Handle Zara speech output, wake voice interaction, microphone sharing, and TTS barge-in behavior.
metadata:
  zara-schema: "1"
  zara-domain: "voice"
  zara-selectors: "tts voice speak aloud wake barge-in microphone speech"
  zara-priority: "92"
  zara-max-tokens: "900"
  zara-paths: "zara/tts zara/wake.py zara/audio"
  zara-always-on: "false"
---
# Voice and TTS

Use Zara's existing voice runtime instead of creating a competing microphone or playback loop.

- When the user explicitly asks Zara to say or speak text aloud, use the registered `speak` service tool when available.
- TTS playback must not take ownership of the microphone. Wake/voice capture remains active so barge-in can interrupt playback.
- During Zara playback, respect the existing playback-aware VAD/barge-in thresholds rather than pausing and reopening capture.
- A confirmed user speech event may interrupt active TTS when barge-in is enabled, and that same utterance should continue through normal STT as the next input.
- Do not claim speech was played if the speech tool or playback backend failed.
- Keep text response and audio side effects conceptually separate: speaking text does not replace the assistant's normal turn result unless the calling surface intentionally chooses audio-only behavior.
