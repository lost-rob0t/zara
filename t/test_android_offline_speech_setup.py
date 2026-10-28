"""Android package visibility and canonical speech owner integration contracts."""
from pathlib import Path
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
APP = ROOT / "android/app/src/main"
ANDROID = "{http://schemas.android.com/apk/res/android}"


def test_tts_service_is_visible_to_android_package_manager():
    manifest = ET.parse(APP / "AndroidManifest.xml").getroot()
    actions = [a.get(ANDROID + "name") for a in manifest.findall("./queries/intent/action")]
    assert "android.intent.action.TTS_SERVICE" in actions


def test_assistant_reuses_the_canonical_speech_service():
    source = (APP / "java/ai/zara/app/assistant/LocalAssistantVoiceController.kt").read_text()
    assert "appSession.speakLocal(result.text)" in source
    assert "appSession.stopLocalSpeech()" in source
    assert "TextToSpeech(" not in source
    assert "Speech unavailable:" in source


def test_main_app_client_uses_cross_process_ipc():
    source = (APP / "java/ai/zara/app/localai/LocalAiServiceClient.kt").read_text()
    assert "LocalAiRemoteClient(context)" in source
    assert "as? LocalAiService.LocalBinder" not in source


def test_runtime_settings_have_speech_setup_test_stop_and_refresh_actions():
    card = (APP / "java/ai/zara/app/ui/LocalSpeechSettings.kt").read_text()
    for action in ("Test offline speech", "Stop speech", "Install voice data", "Android speech settings", "Refresh voices"):
        assert action in card
    activity = (APP / "java/ai/zara/app/MainActivity.kt").read_text()
    assert "TextToSpeech.Engine.ACTION_INSTALL_TTS_DATA" in activity
    assert "appSession.refreshLocalSpeech()" in activity


def test_companion_launch_requires_matching_signature_and_visible_package():
    manifest = ET.parse(APP / "AndroidManifest.xml").getroot()
    assert "ai.zara.llmserve" in [p.get(ANDROID + "name") for p in manifest.findall("./queries/package")]
    activity = (APP / "java/ai/zara/app/MainActivity.kt").read_text()
    assert "packageManager.checkSignatures(packageName, LOCAL_COMPANION_PACKAGE)" in activity
    assert "PackageManager.SIGNATURE_MATCH" in activity
