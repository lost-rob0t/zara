{
  description = "Zarathushtra – Simple voice assistant";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      eachSystem = nixpkgs.lib.genAttrs supportedSystems;

      mkSystem = system:
        let
          pkgs = import nixpkgs { inherit system; };

          # nixpkgs default Python moved to 3.14 in the 2026-08 revision. Zara's
          # tested runtime is 3.13; stay pinned until the full dependency set
          # (faster-whisper, pyswip, LangChain stack) is validated on 3.14.
          python = pkgs.python313;

          # whisper.cpp is the AMD/Radeon STT path. Vulkan avoids tying Zara's
          # local voice acceleration to ROCm GPU support matrices.
          whisperCppVulkan = pkgs.whisper-cpp.override {
            vulkanSupport = true;
          };

          # nixpkgs now packages sherpa-onnx (1.13.3) matched against its own
          # onnxruntime; the hand-pinned 1.12.37 wheel (ORTools 1.23.2 ABI) is
          # retired with the old nixpkgs revision.

          # Android SDK for the Zara Android client (#171). Nix supplies the
          # toolchain; Gradle/AGP keep Android build semantics. No NDK yet:
          # the Trealla/NDK spike lands with #172 per the #170 bakeoff record.
          #
          # Scoped pkgs instance: the SDK is unfree and its license must be
          # accepted; the main Zara package set stays untouched.
          androidPkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              android_sdk.accept_license = true;
            };
          };

          androidEnv = androidPkgs.androidenv.composeAndroidPackages {
            cmdLineToolsVersion = "11";
            platformVersions = [ "37" ];
            buildToolsVersions = [ "36.0.0" ];
            includeEmulator = false;
            includeNDK = false;
            includeSources = false;
            includeSystemImages = false;
          };

          # The composite androidenv `androidsdk` attribute is broken in the
          # pinned nixpkgs revision (its cmdline-tools composite fails to
          # evaluate), so the SDK root is assembled from the individually
          # deployed platform-tools/build-tools/platforms packages plus the
          # accepted-license hash files derived from androidenv's repo.json.
          # AGP needs none of cmdline-tools/sdkmanager for assemble/test.
          androidSdk = androidPkgs.runCommand "zara-android-sdk" { } ''
            sdk=$out/libexec/android-sdk
            mkdir -p $sdk/licenses $sdk/build-tools $sdk/platforms

            ln -s ${androidEnv.platform-tools}/libexec/android-sdk/platform-tools $sdk/platform-tools

            ${androidPkgs.lib.concatMapStrings (bt: ''
              for d in ${bt}/libexec/android-sdk/build-tools/*; do
                ln -s "$d" $sdk/build-tools/$(basename $d)
              done
            '') androidEnv."build-tools"}

            ${androidPkgs.lib.concatMapStrings (pl: ''
              for d in ${pl}/libexec/android-sdk/platforms/*; do
                ln -s "$d" $sdk/platforms/$(basename $d)
              done
            '') androidEnv.platforms}

            ${androidPkgs.lib.concatMapStrings (name: ''
              ln -s ${
                androidPkgs.writeText "zara-android-sdk-license-${name}"
                  (androidPkgs.lib.concatStringsSep "\n"
                    (map (t: builtins.hashString "sha1" t)
                      androidEnvRepo.licenses.${name}))
              } $sdk/licenses/${name}
            '') [ "android-sdk-license" ]}

            mkdir -p $out/bin
            for i in ${androidEnv.platform-tools}/bin/*; do
              ln -s $i $out/bin
            done
          '';

          androidEnvRepo = builtins.fromJSON
            (builtins.readFile "${androidPkgs.path}/pkgs/development/mobile/androidenv/repo.json");

          # Build pyswip from GitHub (use the same python toolchain everywhere)
          pyswip = python.pkgs.buildPythonPackage rec {
            pname = "pyswip";
            version = "0.3.1";
            format = "pyproject";

            src = pkgs.fetchFromGitHub {
              owner = "yuce";
              repo = "pyswip";
              rev = "v${version}";
              sha256 = "sha256-WmePtJ7MnGIyfQ6O3TaWGADkvRSyPLFbj2C8nbOLM3k=";
            };

            nativeBuildInputs = [
              python.pkgs.setuptools
              python.pkgs.wheel
            ];

            buildInputs = [ pkgs.swi-prolog ];

            doCheck = false;

            meta = {
              description = "PySwip is a Python-SWI-Prolog bridge";
              homepage = "https://github.com/yuce/pyswip";
            };
          };

          # Official MCP Python SDK v2. Hand-packaged so the SDK's dependency
          # set stays independent of nixpkgs churn in the httpx/httpcore
          # namespace; revisit if nixpkgs ships MCP v2 natively.
          httpcore2V2 = python.pkgs.buildPythonPackage rec {
            pname = "httpcore2";
            version = "2.9.1";
            format = "wheel";
            src = pkgs.fetchPypi {
              inherit pname version format;
              dist = "py3";
              python = "py3";
              hash = "sha256-YYJHI3noVf5CISRqK7fs7eQDvGHGeYBirheH0FHM3iY=";
            };
            dependencies = [
              python.pkgs.h11
              python.pkgs.truststore
              python.pkgs.anyio
            ];
            pythonImportsCheck = [ "httpcore2" ];
            doCheck = false;
          };

          httpx2V2 = python.pkgs.buildPythonPackage rec {
            pname = "httpx2";
            version = "2.9.1";
            format = "wheel";
            src = pkgs.fetchPypi {
              inherit pname version format;
              dist = "py3";
              python = "py3";
              hash = "sha256-GCD+FKmrEQe/7/OSWZh0KUULBw7A/zjMh+sNjJf9xxo=";
            };
            dependencies = [
              python.pkgs.anyio
              python.pkgs.certifi
              httpcore2V2
              python.pkgs.idna
            ];
            pythonImportsCheck = [ "httpx2" ];
            doCheck = false;
          };

          mcpTypesV2 = python.pkgs.buildPythonPackage rec {
            pname = "mcp-types";
            version = "2.0.0";
            format = "wheel";
            src = pkgs.fetchPypi {
              pname = "mcp_types";
              inherit version format;
              dist = "py3";
              python = "py3";
              hash = "sha256-ay3nl8onl/Vot5Up4bJZSONN5RG8wL2C/vEDmm0bjrA=";
            };
            dependencies = [
              python.pkgs.pydantic
              python.pkgs.typing-extensions
            ];
            pythonImportsCheck = [ "mcp_types" ];
            doCheck = false;
          };

          mcpV2 = python.pkgs.buildPythonPackage rec {
            pname = "mcp";
            version = "2.0.0";
            format = "wheel";
            src = pkgs.fetchPypi {
              inherit pname version format;
              dist = "py3";
              python = "py3";
              hash = "sha256-HLTHXS0se4wddWNV5dgqOfKCLMfxPiKiBR18o1kjSdY=";
            };
            dependencies = [
              python.pkgs.anyio
              httpx2V2
              python.pkgs.jsonschema
              mcpTypesV2
              python.pkgs.opentelemetry-api
              python.pkgs.pydantic
              python.pkgs.pyjwt
              python.pkgs.cryptography
              python.pkgs.python-multipart
              python.pkgs.sse-starlette
              python.pkgs.starlette
              python.pkgs.typing-extensions
              python.pkgs.typing-inspection
              python.pkgs.uvicorn
            ];
            pythonImportsCheck = [ "mcp" ];
            doCheck = false;
          };

          pythonLibs = python.withPackages (p: [
            p.sounddevice
            p.numpy
            p.pynput
            p.faster-whisper
            p.openai-whisper
            p.sherpa-onnx
            p.aiohttp
            p.soundfile
            p.pyyaml
            p.pydantic
            p.httpx
            mcpV2
            p.tomli  # TOML parsing for config system
            p.orgparse
            pyswip
            # LangChain + LangGraph for agent system
            p.langchain
            p.langchain-core
            p.langchain-community
            p.langgraph
            p.langchain-anthropic
            p.anthropic
            p.langchain-openai
            p.openai
            p.langchain-ollama
            p.ollama
            # TTS providers
            p.elevenlabs
            p.edge-tts
            # Memory
            p.chromadb
            p.sentence-transformers
            # Actor framework for real-time turn coordinator
            p.pykka
            # Streaming VAD (Silero VAD via GGML C extension)
            p.pysilero-vad
            # Desktop pet overlay (PySide6/Qt6)
            p.pyside6
            # Pillow for WebP->PNG conversion at pet import time (Qt's nixpkgs
            # build lacks the WebP image plugin)
            p.pillow
            # ZeroMQ for cross-process pet event streaming (wake -> pet overlay)
            p.pyzmq
            # Testing
            p.pytest
            p.pytest-asyncio
            # Packaging metadata sanity checks
            p.setuptools
            p.wheel
          ]);

          # Shared derivation builder for the Zara runtime packages.
          mkZaraPackage = { pname, binaryName ? pname, addFlags, withProlog ? true, extraPath ? [ ], }:
            pkgs.stdenv.mkDerivation {
              inherit pname;
              version = "1.0";
              src = ./.;

              nativeBuildInputs = [ pkgs.makeWrapper ];

              installPhase = ''
                mkdir -p $out/bin
                mkdir -p $out/lib/python
                ${if withProlog then "mkdir -p $out/share/zarathushtra" else ""}

                # Copy the zara Python module
                cp -r $src/zara $out/lib/python/

                ${if withProlog then ''
                  # Copy ALL Prolog sources with structure intact
                  cp $src/*.pl $out/share/zarathushtra/ 2>/dev/null || true
                  cp -r $src/kb $out/share/zarathushtra/
                  cp -r $src/modules $out/share/zarathushtra/
                  cp -r $src/assets $out/share/zarathushtra/
                '' else ""}

                # Create wrapper with correct Python interpreter and environment
                makeWrapper ${pythonLibs}/bin/python3 $out/bin/${binaryName} \
                  --add-flags "${addFlags}" \
                  --prefix PATH : ${pkgs.lib.makeBinPath ([ pkgs.swi-prolog pkgs.mpv ] ++ extraPath)} \
                  --set PYTHONPATH $out/lib/python${if withProlog then ":$out/share/zarathushtra" else ""}:${pythonLibs}/${python.sitePackages} \
                  --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath [ pkgs.libsndfile pkgs.portaudio ]} \
                  ${if withProlog then "--set SWI_HOME_DIR ${pkgs.swi-prolog}/lib/swipl" else ""} \
                  --run "${if withProlog then "cd $out/share/zarathushtra" else ""}"
              '';
            };

          zara-cli = mkZaraPackage {
            pname = "zara-cli";
            binaryName = "zara";
            addFlags = "-m zara";
            extraPath = [ pkgs.xdotool pkgs.pulseaudio pkgs.ffmpeg-full whisperCppVulkan ];
          };

          zara-server = mkZaraPackage {
            pname = "zara-server";
            binaryName = "zara-server";
            addFlags = "-m zara.server";
            extraPath = [ pkgs.xdotool pkgs.pulseaudio pkgs.ffmpeg-full whisperCppVulkan ];
          };

          zara-desktop = mkZaraPackage {
            pname = "zara-desktop";
            addFlags = "-m zara.desktop.app";
            extraPath = [ pkgs.xdotool pkgs.pulseaudio pkgs.ffmpeg-full whisperCppVulkan ];
          };

          zara-wake = mkZaraPackage {
            pname = "zara-wake";
            addFlags = "-m zara --wake";
            extraPath = [ pkgs.xdotool pkgs.pulseaudio pkgs.ffmpeg-full whisperCppVulkan ];
          };

          zara-dictate = mkZaraPackage {
            pname = "zara-dictate";
            addFlags = "-m zara --dictate";
            withProlog = false;
            extraPath = [ pkgs.xdotool whisperCppVulkan ];
          };

          # zara-prolog keeps the historical layout: Python wrapper that points
          # at the bundled Prolog share for console mode.
          zara-prolog = pkgs.stdenv.mkDerivation {
            pname = "zara-prolog";
            version = "1.0";
            src = ./.;

            buildInputs = [ pkgs.swi-prolog pkgs.makeWrapper ];

            installPhase = ''
              mkdir -p $out/share/zarathushtra
              mkdir -p $out/bin

              # Copy ALL Prolog sources with structure intact
              cp $src/*.pl $out/share/zarathushtra/ 2>/dev/null || true
              cp -r $src/kb $out/share/zarathushtra/
              cp -r $src/modules $out/share/zarathushtra/
              cp -r $src/scripts $out/share/zarathushtra/
              cp -r $src/zara $out/share/zarathushtra/
              cp -r $src/assets $out/share/zarathushtra/

              # zara-console (Python wrapper)
              makeWrapper ${pythonLibs}/bin/python3 $out/bin/zara-console \
                --add-flags "-m zara --console" \
                --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.swi-prolog pkgs.mpv ]} \
                --set PYTHONPATH $out/share/zarathushtra:${pythonLibs}/${python.sitePackages} \
                --set SWI_HOME_DIR ${pkgs.swi-prolog}/lib/swipl \
                --run "cd $out/share/zarathushtra"
            '';
          };

          zarathushtra = pkgs.buildEnv {
            name = "zarathushtra-full";
            paths = [ zara-cli zara-server zara-desktop zara-prolog zara-wake zara-dictate ];
          };

          # Development wrappers intentionally execute the working checkout,
          # not the immutable package copy in the Nix store.
          zara-dev = pkgs.writeShellScriptBin "zara" ''
            export PYTHONPATH="$PWD''${PYTHONPATH:+:$PYTHONPATH}"
            exec ${pythonLibs}/bin/python -m zara "$@"
          '';

          zara-desktop-dev = pkgs.writeShellScriptBin "zara-desktop" ''
            export PYTHONPATH="$PWD''${PYTHONPATH:+:$PYTHONPATH}"
            exec ${pythonLibs}/bin/python -m zara.desktop.app "$@"
          '';

          checks = {
            # Run the Python test suite. The source tree is read-only in the
            # Nix store, so copy it to a writable scratch dir first so pytest
            # can write ``.pytest_cache`` and ``__pycache__``. Set HOME to a
            # writable temp dir so tests that resolve ``Path.home()`` work.
            pytest = pkgs.runCommand "zara-check-pytest"
              {
                nativeBuildInputs = [ pythonLibs pkgs.swi-prolog pkgs.makeWrapper pkgs.cacert ];
                src = ./.;
              }
              ''
                export HOME=$(mktemp -d)
                export XDG_CONFIG_HOME=$HOME/.config
                export XDG_RUNTIME_DIR=$(mktemp -d)
                export ZARA_DICTATION_PIDFILE=$XDG_RUNTIME_DIR/zara_dictation.pid
                export ZARA_DICTATION_LOGFILE=$XDG_RUNTIME_DIR/zara_dictation.log
                export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
                export NIX_SSL_CERT_FILE=$SSL_CERT_FILE
                export LANG=C.UTF-8
                export LC_ALL=C.UTF-8
                cp -r $src $out-src
                chmod -R u+w $out-src
                cd $out-src
                export PYTHONPATH="$out-src''${PYTHONPATH:+:$PYTHONPATH}"
                # MCP is an installed runtime contract, not an optional skipped
                # test dependency. Prove v2 is present before running pytest.
                ${pythonLibs}/bin/python -c 'import importlib.metadata as m; import mcp; assert int(m.version("mcp").split(".", 1)[0]) >= 2'
                ${pythonLibs}/bin/python -m pytest -q
                touch $out
              '';

            # Compile every Python module to catch syntax errors. The source
            # tree is read-only in the Nix store, so copy it to a writable
            # location first so ``compileall`` can write ``__pycache__``.
            syntax = pkgs.runCommand "zara-check-syntax"
              {
                nativeBuildInputs = [ pythonLibs ];
                src = ./.;
              }
              ''
                cp -r $src $out-src
                chmod -R u+w $out-src
                cd $out-src
                ${pythonLibs}/bin/python -m compileall -q zara scripts
                touch $out
              '';

            # Ensure main.pl and its module graph load cleanly in SWI-Prolog.
            # An isolated HOME prevents the user's local config from masking
            # load failures (or causing spurious ones) during the check.
            prolog-load = pkgs.runCommand "zara-check-prolog-load"
              {
                nativeBuildInputs = [ pkgs.swi-prolog ];
                src = ./.;
              }
              ''
                cd $src
                export HOME=$(mktemp -d)
                export XDG_CONFIG_HOME=$HOME/.config
                swipl -q -g "consult('main.pl'), halt" -t "halt(1)"
                touch $out
              '';

            # Enforce deterministic fixture latency budgets and retain the
            # JSONL/percentile report as the check output.
            latency = pkgs.runCommand "zara-check-latency"
              {
                nativeBuildInputs = [ pythonLibs pkgs.swi-prolog ];
                src = ./.;
              }
              ''
                export HOME=$(mktemp -d)
                export XDG_CONFIG_HOME=$HOME/.config
                export XDG_RUNTIME_DIR=$(mktemp -d)
                export SWI_HOME_DIR=${pkgs.swi-prolog}/lib/swipl
                cp -r $src $out-src
                chmod -R u+w $out-src
                cd $out-src
                export PYTHONPATH="$out-src''${PYTHONPATH:+:$PYTHONPATH}"
                export ARTIFACT_DIR=$out
                bash scripts/test-latency-metrics.sh
              '';

            # Exercise the installed Nix wrappers with isolated HOME and
            # mocked hardware so the package layout is verified end-to-end.
            wrappers = pkgs.runCommand "zara-check-wrappers"
              {
                nativeBuildInputs = [ zara-cli zara-server zara-desktop zara-wake zara-dictate zara-prolog pkgs.bash ];
                src = ./.;
              }
              ''
                export HOME=$(mktemp -d)
                export XDG_CONFIG_HOME=$HOME/.config
                export XDG_RUNTIME_DIR=$(mktemp -d)
                export ZARA_DICTATION_PIDFILE=$XDG_RUNTIME_DIR/zara_dictation.pid
                export ZARA_DICTATION_LOGFILE=$XDG_RUNTIME_DIR/zara_dictation.log
                # zara with no args prints help and exits 1 — that proves the
                # wrapper, Python interpreter, and zara package all resolve.
                set +e
                zara >$HOME/cli.out 2>&1
                cli_rc=$?
                zara --help >$HOME/cli-help.out 2>&1
                zara-server --help >$HOME/server.out 2>&1
                zara-wake --help >$HOME/wake.out 2>&1 || true
                zara-console --help >$HOME/console.out 2>&1 || true
                zara-dictate --help >$HOME/dictate.out 2>&1 || true
                set -e
                test "$cli_rc" -eq 1
                grep -q "Zarathustra Voice Assistant" $HOME/cli.out
                grep -q -- "--desktop" $HOME/cli-help.out
                grep -q "Long-lived Zara assistant service" $HOME/server.out
                grep -q "usage:" $HOME/wake.out
                grep -q "usage:" $HOME/console.out
                grep -q "usage:" $HOME/dictate.out
                command -v zara-desktop >/dev/null
                test -x "$(command -v zara-desktop)"
                touch $out
              '';
          };
        in
        {
          packages = {
            zara-cli = zara-cli;
            zara-server = zara-server;
            zara-desktop = zara-desktop;
            zara-prolog = zara-prolog;
            zara-wake = zara-wake;
            zara-dictate = zara-dictate;
            zarathushtra = zarathushtra;
            default = zarathushtra;
          };

          apps = {
            zara = {
              type = "app";
              program = "${zara-cli}/bin/zara";
            };
            zara-server = {
              type = "app";
              program = "${zara-server}/bin/zara-server";
            };
            zara-desktop = {
              type = "app";
              program = "${zara-desktop}/bin/zara-desktop";
            };
            zara-wake = {
              type = "app";
              program = "${zara-wake}/bin/zara-wake";
            };
            zara-console = {
              type = "app";
              program = "${zara-prolog}/bin/zara-console";
            };
            zara-dictate = {
              type = "app";
              program = "${zara-dictate}/bin/zara-dictate";
            };
            default = {
              type = "app";
              program = "${zara-cli}/bin/zara";
            };
          };

          devShells.default = pkgs.mkShell {
            name = "zarathushtra-dev-shell";

            buildInputs = [
              pythonLibs
              zara-dev
              zara-desktop-dev
              whisperCppVulkan
              pkgs.xdotool
              pkgs.ffmpeg-full  # Includes ffplay for streaming audio
              pkgs.mpv  # Alternative for streaming audio playback
              pkgs.portaudio
              pkgs.swi-prolog
              pkgs.pulseaudio
            ];

            shellHook = ''
              export PYTHONPATH="$PWD''${PYTHONPATH:+:$PYTHONPATH}"
              echo "Python + Whisper + whisper.cpp/Vulkan + SWI-Prolog + LangChain + MCP v2 ready"
              echo ""
              echo "Commands:"
              echo "  zara-desktop                   # Native desktop / Quick Copilot"
              echo "  zara --desktop                 # Same canonical desktop entry point"
              echo "  zara-server                    # Long-lived Zara service"
              echo "  zara --wake                    # Wake listener"
              echo "  zara --wake --stt-provider whisper-cpp --device amd  # AMD/Vulkan STT"
              echo "  zara --console                 # Console mode"
              echo "  zara --dictate                 # Dictation mode"
              echo "  zara --agent                   # Direct agent conversation"
              echo "  zara mcp status               # Inspect MCP connections"
              echo ""
              echo "Build system:"
              echo "  nix build .#zara-desktop      # Build native desktop package"
              echo "  nix build .#zara-server       # Build service package"
              echo "  nix run .#zara-desktop        # Run native desktop / Quick Copilot"
              echo "  nix run .#zara-server         # Run long-lived Zara service"
              echo "  nix build                     # Build all packages"
              echo "  nix run                       # Run default CLI (prints help with no args)"
              echo "  nix run .#zara-wake           # Run wake listener"
              echo "  nix flake check               # Run all checks (pytest, scripts, syntax, prolog load)"
            '';
          };

          devShells.android = pkgs.mkShell {
            name = "zara-android-dev-shell";

            packages = [
              pkgs.jdk21
              pkgs.gradle_9
              androidSdk
            ];

            shellHook = ''
              export ANDROID_HOME=${androidSdk}/libexec/android-sdk
              export ANDROID_SDK_ROOT=$ANDROID_HOME
              export JAVA_HOME=${pkgs.jdk21.home}
              echo "Zara Android toolchain ready: Gradle 9 + JDK 21 + SDK (API 37)"
              echo "  nix develop .#android -c bash scripts/test-android.sh"
            '';
          };

          checks = checks;
        };

      perSystemOutputs = eachSystem mkSystem;
    in
    {
      packages = nixpkgs.lib.mapAttrs (_: v: v.packages) perSystemOutputs;
      apps = nixpkgs.lib.mapAttrs (_: v: v.apps) perSystemOutputs;
      devShells = nixpkgs.lib.mapAttrs (_: v: v.devShells) perSystemOutputs;
      checks = nixpkgs.lib.mapAttrs (_: v: v.checks) perSystemOutputs;

      flakeouts = eachSystem (system: {
        packages = builtins.attrNames self.packages.${system};
        apps = builtins.attrNames self.apps.${system};
        devShells = builtins.attrNames self.devShells.${system};
        checks = builtins.attrNames self.checks.${system};
      });
    };
}
