voice_ingress_backend(vulkan, whisper_cpp).
voice_ingress_backend(amd, whisper_cpp).
voice_ingress_backend(rocm, whisper_cpp).
voice_ingress_backend(hip, whisper_cpp).

voice_ingress_model_resolution(whisper_cpp, resolve_whisper_cpp_model).
voice_ingress_backend_failure_policy(whisper_cpp, propagate_without_fallback).

verified_by(voice_ingress_backend(_, whisper_cpp),
            't/test_voice_runtime_ingress.py').
