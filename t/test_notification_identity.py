from zara.notification_identity import derive_notification_id


def test_notification_identity_is_stable_and_generation_scoped() -> None:
    first = derive_notification_id(source_peer="phone", platform_identity="android:chat:42", generation=7)
    same = derive_notification_id(source_peer="phone", platform_identity="android:chat:42", generation=7)
    replaced = derive_notification_id(source_peer="phone", platform_identity="android:chat:42", generation=8)
    assert first == same
    assert first != replaced
    assert first.startswith("notification:")


def test_notification_identity_component_boundaries_do_not_collide() -> None:
    left = derive_notification_id(source_peer="a", platform_identity="bc", generation=1)
    right = derive_notification_id(source_peer="ab", platform_identity="c", generation=1)
    assert left != right
