% Durable voice-output knowledge (verified 2026-11-16, RX 5500 XT / RADV).
voice_tts_provider(llamacpp, "shells llama-tts binary; default provider since 2026-11-16").
voice_tts_model(pocket_tts, "159MB GGUF + 60MB mmproj, ~/.cache/zara/tts/pocket-tts/").
voice_tts_measured(pocket_tts, "-ngl 12: 0.97 GiB VRAM peak, 5.14x realtime, cloning via --tts-speaker-file").
voice_tts_measured(qwen3_tts_17b, "llama-tts Vulkan: ~1.1-1.2x realtime, 7.56 GiB VRAM peak - quality tier only").
voice_tts_limit(arch_llama_server, "Arch llama-cpp 0.6.0 llama-server has NO /tts or /v1/audio/speech routes; wrap llama-tts instead").
voice_tts_workaround(ffprobe_speaker, "llama-tts decodes speaker refs via ffprobe pipe; nix-profile ffprobe fails on stdin - engine prepends /usr/bin to PATH").
voice_stt_measured(whisper_vulkan, "large-v3-turbo q5_0: RTF 0.15 on 10.4s clip, ~0.5 GiB VRAM; stock arch package auto-loads /usr/lib/ggml/libggml-vulkan.so").
voice_status_announcer(design, "TaskStatusAnnouncer consumes runtime bus task events; runner publishes truncated goal in event.label; speaks via AudioOutput* pipeline; gated by [tasks].status_announcements").
voice_rocm_warning(rdna1, "gfx1010 never ROCm-supported; HSA_OVERRIDE segfaults torch>=2.0; entire voice stack rides Mesa RADV Vulkan").
voice_profile_modes(contract, "normal -> llamacpp (local GPU), game -> edge (low latency, frees VRAM); persisted as [tts] provider").
voice_profile_live_switch(path, "voice intent -> python_skills set_game_mode/set_normal_mode -> voice_profile.set_voice_mode -> ZaraConfig.set_value transactional TOML persist -> registered reset callbacks drop TtsOutputBridge/TaskStatusAnnouncer cached engines; next phrase rebuilds from fresh config").
voice_profile_cli(caveat, "zara voice-mode game|normal|status flips persisted config in its own process; a running daemon applies it at next restart or next spoken voice-mode command").
voice_profile_intent_surface("game mode"/"normal mode" resolve via normalizer slang_map joins -> python_skill_intent(gamemode/normalmode,0) materialized as verb_intent; command_gate COMMAND_TRIGGER_WORDS carries game/normal so bare phrases pass the gate and reach Prolog).
config_set_value(contract, "ZaraConfig.set_value(section,key,value) validates the merged config in memory first, then section-scoped TOML text edit; invalid values change neither file nor memory; supported scalar types str/bool/int/float").
