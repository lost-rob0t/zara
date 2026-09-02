{
  description = "Zara Android reproducible build toolchain";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      eachSystem = nixpkgs.lib.genAttrs supportedSystems;
      treallaRevision = "b25ccfb8e485a697bb47f1947d6fb8e0ad4e6aaf";

      mkSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              android_sdk.accept_license = true;
            };
          };

          androidEnvRepo = builtins.fromJSON
            (builtins.readFile "${pkgs.path}/pkgs/development/mobile/androidenv/repo.json");
          ndkVersion = androidEnvRepo.latest.ndk;

          androidEnv = pkgs.androidenv.composeAndroidPackages {
            cmdLineToolsVersion = "11";
            platformVersions = [ "37" ];
            buildToolsVersions = [ "36.0.0" ];
            includeCmake = true;
            cmakeVersions = [ "3.22.1" ];
            includeEmulator = false;
            includeNDK = true;
            ndkVersions = [ ndkVersion ];
            includeSources = false;
            includeSystemImages = false;
          };

          androidSdk = pkgs.runCommand "zara-android-sdk" { } ''
            sdk=$out/libexec/android-sdk
            mkdir -p $sdk/licenses $sdk/build-tools $sdk/platforms $sdk/cmake $sdk/ndk

            ln -s ${androidEnv.platform-tools}/libexec/android-sdk/platform-tools $sdk/platform-tools

            ${pkgs.lib.concatMapStrings (buildTools: ''
              for directory in ${buildTools}/libexec/android-sdk/build-tools/*; do
                ln -s "$directory" $sdk/build-tools/$(basename "$directory")
              done
            '') androidEnv."build-tools"}

            ${pkgs.lib.concatMapStrings (platform: ''
              for directory in ${platform}/libexec/android-sdk/platforms/*; do
                ln -s "$directory" $sdk/platforms/$(basename "$directory")
              done
            '') androidEnv.platforms}

            ${pkgs.lib.concatMapStrings (cmake: ''
              for directory in ${cmake}/libexec/android-sdk/cmake/*; do
                ln -s "$directory" $sdk/cmake/$(basename "$directory")
              done
            '') androidEnv.cmake}

            ${pkgs.lib.concatMapStrings (ndk: ''
              ln -s ${ndk}/libexec/android-sdk/ndk $sdk/ndk/${ndk.version}
            '') androidEnv.ndk-bundles}

            ${pkgs.lib.concatMapStrings (name: ''
              ln -s ${
                pkgs.writeText "zara-android-sdk-license-${name}"
                  (pkgs.lib.concatStringsSep "\n"
                    (map (text: builtins.hashString "sha1" text)
                      androidEnvRepo.licenses.${name}))
              } $sdk/licenses/${name}
            '') [ "android-sdk-license" ]}

            mkdir -p $out/bin
            for executable in ${androidEnv.platform-tools}/bin/*; do
              ln -s "$executable" $out/bin
            done
          '';

          treallaSource = pkgs.fetchFromGitHub {
            owner = "trealla-prolog";
            repo = "trealla";
            rev = treallaRevision;
            hash = pkgs.lib.fakeHash;
          };
        in
        {
          devShells.default = pkgs.mkShell {
            name = "zara-android-dev-shell";

            packages = [
              pkgs.jdk21
              pkgs.gradle_9
              pkgs.gnumake
              pkgs.gcc
              androidSdk
            ];

            shellHook = ''
              export ANDROID_HOME=${androidSdk}/libexec/android-sdk
              export ANDROID_SDK_ROOT=$ANDROID_HOME
              export ANDROID_NDK_ROOT=${androidEnv.ndk-bundle}/libexec/android-sdk/ndk
              export JAVA_HOME=${pkgs.jdk21.home}
              export ZARA_ANDROID_NDK_VERSION=${androidEnv.ndk-bundle.version}
              export ZARA_TREALLA_SOURCE_DIR=${treallaSource}
              echo "Zara Android toolchain ready: Gradle 9 + JDK 21 + SDK 37 + NDK ${androidEnv.ndk-bundle.version}"
            '';
          };
        };
    in
    {
      devShells = nixpkgs.lib.mapAttrs (_: value: value.devShells) (eachSystem mkSystem);
    };
}
