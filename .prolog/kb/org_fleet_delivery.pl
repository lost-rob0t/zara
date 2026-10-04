% Org Android app fleet — durable engineering knowledge (delivered 2026-09-25).
% Consumed issues: #1000 (epic), #1056 (separate APKs, independence), #1177 (ORG-10 APK lane), PR #972.
% Releases: v0.3.0-alpha @ 6033bf1a and v0.3.1-alpha @ 370834cc (/releases/latest, 30 assets each).

fleet_module("org-app", "ai.zara.org.app", full_surface_flagship).
fleet_module("org-editor", "ai.zara.org.editor", focused_surface).
fleet_module("org-todo", "ai.zara.org.todo", focused_surface).
fleet_module("org-reminder", "ai.zara.org.reminder", focused_surface).
fleet_module("org-timer", "ai.zara.org.timer", focused_surface).
fleet_module("org-roam", "ai.zara.org.roam", focused_surface).
fleet_module("org-graph", "ai.zara.org.graph", focused_surface).
fleet_module("org-home", "ai.zara.org.home", focused_surface).

% Architecture: every fleet app is a thin launcher over the shared
% org-surfaces library (OrgWorkspaceScreen + surfaces), which sits on
% org-core (parser/projections) and org-storage (OrgHome authority).
% Flagship = same screen with all 8 surface tabs; focused app = 1 tab.
fleet_uses_shared_libraries(Module) :-
    fleet_module(Module, _, _).

% Independence contract: no fleet app may depend on :app (Zara phone),
% another fleet module, or the optional ai.zara.org.sync.home provider.
% SHARED workspace mode degrades to "Choose Org directory" SAF picker
% when the provider is absent; CUSTOM_SAF always works standalone.
fleet_standallone_mechanism(saf_picked_directory).
fleet_shared_workspace_authority("ai.zara.org.sync.home", optional).

% Release surface contract (release.yml / android-latest.yml):
% - per-app assets zara-<module>-<version>.apk + .sha256 + .manifest.txt
%   with application_id, source_sha, version_name, version_code;
% - versioned release is created WITHOUT --prerelease so it serves
%   GitHub /releases/latest (prereleases are excluded from /latest);
% - android-latest carries zara-<module>-latest.apk + org_<module>_*
%   manifest entries with publish-job SHA cross-checks.
release_asset_pattern("zara-<module>-<version>.apk").
latest_release_page_requires_full_release(true).

% Tooling facts discovered during delivery:
% - aapt2 lives in the android flake shell (ANDROID_HOME from nix develop ./android),
%   NOT the root dev shell; installable/signer scripts require that shell.
% - Gradle daemon heap must stay >= 4g for the 13-APK gate; 2g caused
%   D8 OutOfMemoryError in :org-todo:mergeExtDexDebug on CI (fixed in
%   android/gradle.properties).
% - Master is protected by ruleset 19153430 (requires `test` check on
%   master pushes): even docs chores must go through a PR.
% - The release workflow's master-push promotion publishes the versioned
%   release before a later v* tag push; the tag push then fails BY DESIGN
%   with "refusing to overwrite" — verify the promoted release instead.
% - What's-new overlay (android/app/.../update/Changelog.kt) parses the
%   embedded assets/CHANGELOG.md for `## <zara.version>`; a release built
%   before its section lands shows no overlay. Org fleet APKs embed no
%   changelog asset at all (overlay is a phone-app feature).
% - CI emulator evidence flakes with "Could not load devices ... devices.xml"
%   + "Screen did not retain text containing stock server response";
%   signature is infra-level, rerun-fails-jobs passes (seen 3x).
verification_notes([
    gradle_heap_min_4g,
    android_shell_for_aapt2_scripts,
    master_requires_test_check_ruleset_19153430,
    tag_push_after_master_promotion_refuses_by_design,
    whatsnew_needs_matching_changelog_section_at_build_time,
    emulator_evidence_flake_is_rerunnable
]).

% Fleet packaging contract tests live in t/test_org_app_fleet.py;
% they assert consistency (SemVer + zara.version == release.target +
% versionCode == target code), NOT a specific version literal.
