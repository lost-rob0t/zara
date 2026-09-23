from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def test_llm_serve_module_is_part_of_android_build():
    settings = (ROOT / "android/settings.gradle.kts").read_text()
    build = (ROOT / "android/llm-serve/build.gradle.kts").read_text()

    assert 'include(":llm-serve")' in settings
    assert 'applicationId = "ai.zara.llmserve"' in build
    assert '../app/src/main/java/ai/zara/app/localai' in build
    assert "kotlin.directories" in build
    assert ".java.srcDir" not in build
    assert "libs.litert.lm.android" in build


def test_llm_serve_is_loopback_only_and_bounded():
    source = (
        ROOT
        / "android/llm-serve/src/main/java/ai/zara/llmserve/OllamaLoopbackServer.kt"
    ).read_text()

    assert 'const val LOOPBACK_HOST = "127.0.0.1"' in source
    assert "const val DEFAULT_PORT = 11434" in source
    assert "InetAddress.getByName(LOOPBACK_HOST)" in source
    assert "0.0.0.0" not in source
    for endpoint in ("/api/version", "/api/tags", "/api/ps", "/api/chat", "/api/generate"):
        assert endpoint in source
    assert "MAX_BODY_BYTES" in source
    assert "MAX_MESSAGES" in source
    assert "MAX_PROMPT_CHARS" in source


def test_llm_serve_service_is_private_foreground_service():
    manifest = (
        ROOT / "android/llm-serve/src/main/AndroidManifest.xml"
    ).read_text()

    assert 'android.permission.INTERNET' in manifest
    assert 'android.permission.FOREGROUND_SERVICE' in manifest
    assert 'android:name=".LlmServeService"' in manifest
    assert 'android:exported="false"' in manifest
    assert 'android:foregroundServiceType="specialUse"' in manifest


def test_llm_serve_model_import_requires_explicit_quantization():
    activity = (
        ROOT / "android/llm-serve/src/main/java/ai/zara/llmserve/MainActivity.kt"
    ).read_text()
    service = (
        ROOT / "android/llm-serve/src/main/java/ai/zara/llmserve/LlmServeService.kt"
    ).read_text()

    assert 'SELECT_QUANTIZATION = "Select quantization"' in activity
    assert "LocalModelQuantization.requireKnown" in service
    assert "sha256(uri)" in service
    assert "MAX_MODEL_BYTES" in service


def test_llm_serve_reuses_actor_runtime_instead_of_creating_second_inference_core():
    engine = (
        ROOT / "android/llm-serve/src/main/java/ai/zara/llmserve/LlmServeEngine.kt"
    ).read_text()

    assert "LocalAiRuntime(" not in engine
    assert "LocalModelStore(" not in engine
    assert "LiteRtLocalLlmBackend(" not in engine
    assert "LocalAiRemoteClient" in engine
    assert "Cannot replace the active model during generation" not in engine
