voice_ingress_backend(vulkan, whisper_cpp).
voice_ingress_backend(amd, whisper_cpp).
voice_ingress_backend(rocm, whisper_cpp).
voice_ingress_backend(hip, whisper_cpp).

voice_ingress_model_resolution(whisper_cpp, resolve_whisper_cpp_model).
voice_ingress_backend_failure_policy(whisper_cpp, propagate_without_fallback).
voice_ingress_worker_state(whisper_cpp, 'zara/whisper_cpp.py', user_runtime_state).
voice_ingress_worker_failure_policy(whisper_cpp, refuse_live_worker_and_circuit_vulkan_device_loss).

verified_by(voice_ingress_backend(_, whisper_cpp),
            't/test_voice_runtime_ingress.py').
verified_by(voice_ingress_worker_failure_policy(whisper_cpp, _),
            't/test_whisper_cpp.py').
