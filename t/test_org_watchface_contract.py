from pathlib import Path
from xml.etree import ElementTree


ROOT = Path(__file__).resolve().parents[1]
WATCHFACE = ROOT / "android/org-watchface"
WATCHFACE_XML = WATCHFACE / "src/main/res/raw/watchface.xml"
MANIFEST = WATCHFACE / "src/main/AndroidManifest.xml"


def test_org_watchface_is_resource_only_and_uses_canonical_version_context():
    gradle = (WATCHFACE / "build.gradle.kts").read_text()
    manifest = MANIFEST.read_text()

    assert "version.properties" in gradle
    assert 'getProperty("zara.version")' in gradle
    assert 'getProperty("android.versionCode")' in gradle
    assert "versionName = zaraVersionName" in gradle
    assert "versionCode = zaraAndroidVersionCode" in gradle
    assert 'versionName = "0.1.0-alpha"' not in gradle
    assert "versionCode = 1" not in gradle

    assert 'android:hasCode="false"' in manifest
    assert 'android:name="android.hardware.type.watch"' in manifest
    assert 'android:name="com.google.wear.watchface.format.version"' in manifest

    code_files = [
        path
        for path in (WATCHFACE / "src/main").rglob("*")
        if path.suffix in {".kt", ".java", ".class"}
    ]
    assert code_files == []


def test_org_watchface_uses_canonical_org_complication_providers():
    root = ElementTree.parse(WATCHFACE_XML).getroot()
    assert root.tag == "WatchFace"
    assert root.attrib == {"width": "450", "height": "450"}

    slots = root.findall(".//ComplicationSlot")
    assert len(slots) == 7

    by_id = {slot.attrib["slotId"]: slot for slot in slots}
    assert set(by_id) == {str(index) for index in range(1, 8)}

    for lane in range(1, 7):
        slot = by_id[str(lane)]
        assert set(slot.attrib["supportedTypes"].split()) == {"RANGED_VALUE", "EMPTY"}
        policy = slot.find("DefaultProviderPolicy")
        assert policy is not None
        assert policy.attrib["primaryProvider"] == (
            f"ai.zara.app/ai.zara.wear.complications.OrgSchedule{lane}ComplicationService"
        )
        assert policy.attrib["primaryProviderType"] == "RANGED_VALUE"

        transforms = {
            transform.attrib["target"]: transform.attrib["value"]
            for transform in slot.findall(".//Transform")
        }
        assert transforms["startAngle"] == "[COMPLICATION.RANGED_VALUE_MIN] * 0.5"
        assert transforms["endAngle"] == "[COMPLICATION.RANGED_VALUE_MAX] * 0.5"

    current_next = by_id["7"]
    assert set(current_next.attrib["supportedTypes"].split()) == {"SHORT_TEXT", "EMPTY"}
    policy = current_next.find("DefaultProviderPolicy")
    assert policy is not None
    assert policy.attrib["primaryProvider"] == (
        "ai.zara.app/ai.zara.wear.complications.OrgNextTodoComplicationService"
    )
    assert policy.attrib["primaryProviderType"] == "SHORT_TEXT"


def test_org_watchface_preserves_ambient_and_analog_contract():
    watchface = WATCHFACE_XML.read_text()

    assert '<Variant mode="AMBIENT" target="alpha" value="0" />' in watchface
    assert '<Transform target="angle" value="[HOUR_0_11_MINUTE] * 30" />' in watchface
    assert '<Transform target="angle" value="[MINUTE_SECOND] * 6" />' in watchface
    assert '<Transform target="angle" value="[SECOND] * 6" />' in watchface
    assert "[COMPLICATION.TEXT]" in watchface

    info = (WATCHFACE / "src/main/res/xml/watch_face_info.xml").read_text()
    assert '<Preview value="@drawable/preview" />' in info
    assert '<MultipleInstancesAllowed value="true" />' in info
    assert '<Editable value="true" />' in info


def test_org_watchface_is_built_scanned_and_uploaded_by_android_gate():
    settings = (ROOT / "android/settings.gradle.kts").read_text()
    test_android = (ROOT / "scripts/test-android.sh").read_text()
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()

    assert 'include(":org-watchface")' in settings
    assert ":org-watchface:assembleDebug" in test_android
    assert 'watchface_apk="org-watchface/build/outputs/apk/debug/org-watchface-debug.apk"' in test_android
    assert 'for apk in "$phone_apk" "$code_apk" "$termux_bridge_apk" "$wear_apk" "$voice_apk" "$watchface_apk"' in test_android
    assert "zara-org-time-watchface-${{ github.event.pull_request.head.sha || github.sha }}" in workflow
    assert "android/org-watchface/build/outputs/apk/debug/org-watchface-debug.apk" in workflow
