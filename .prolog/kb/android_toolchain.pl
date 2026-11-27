% Android toolchain — verified build, test, and device recipes.
% Provenance: session run e554906d916a4c7d80ecc3c1ec1fbfb7, base 58a4f13d,
% worktree zarathushtra-android-refresh. Every fact below was executed
% successfully in that session unless marked otherwise.

% The Android toolchain lives ONLY in the nested android flake dev shell,
% not the root dev shell. From the repo root the invocation is
% `nix develop ./android`; from inside android/ it is `nix develop .`
% (using ./android from inside android/ fails: android/android is missing).
android_devshell("nix develop ./android").
android_devshell_from_android_dir("nix develop .").

% The android/ tree ships NO gradle wrapper. The dev shell provides
% `gradle` (9.5.1) on PATH; `./gradlew` always fails with "No such file
% or directory". Root devShell `.#android` is a known broken alias
% (issue #1267: missing NDK + Trealla env).
gradle_invocation(devshell_gradle_binary).
gradle_invocation(no_wrapper_file).

% Phone/Wear APK quick build (verified 2026-11-26, app 110MB, wear 35MB,
% wear-voice 28MB). build-trealla.sh must run from inside android/ with
% the dev shell env; the Gradle native build then needs the built
% library root exported as ZARA_TREALLA_LIBRARY_ROOT.
quick_build_recipe([
    step(1, 'cd android'),
    step(2, 'nix develop . -c bash ./build-trealla.sh'),
    step(3, 'export ZARA_TREALLA_LIBRARY_ROOT="$PWD/app/build/trealla"'),
    step(4, 'gradle :app:assembleDebug -x lint')
]).
quick_build_outputs([
    apk('app/build/outputs/apk/debug/app-debug.apk', 110),
    apk('wear-app/build/outputs/apk/debug/wear-app-debug.apk', 35),
    apk('wear-voice/build/outputs/apk/debug/wear-voice-debug.apk', 28)
]).

% Focused JVM test loop for :app (fast, seconds):
%   nix develop . -c gradle :app:testDebugUnitTest --tests "ai.zara.app.model.*"
focused_test_command('gradle :app:testDebugUnitTest --tests <filter>').

% Full gates (per android/AGENTS.md and skills/zara-android-release):
full_android_gate('scripts/test-android.sh').
release_gate('scripts/test-android-release.sh').

% adb comes from the dev shell's platform-tools; the host has no system
% adb. Device visibility still requires the phone to enumerate on USB
% (lsusb) and pass the debugging authorization prompt.
adb_source(devshell_platform_tools).

% Compose/UI work authority: android/DESIGN.md + design/reference SVG +
% GitHub epic #648; three-menu navigation already implemented in
% AppNavigation.kt (Chat|Voice, Logic|Health|Projects|Scheduled,
% Settings overview -> child screens).
ui_authority(android_design_md).
ui_authority(epic_648).
