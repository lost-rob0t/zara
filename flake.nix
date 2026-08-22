{
  description = "Zarathushtra – Simple voice assistant";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    prolog-rlm = {
      url = "github:lost-rob0t/prolog-rlm/4cdc9854a510a2d07b559e9ae34491d43d81301a";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, prolog-rlm, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      eachSystem = nixpkgs.lib.genAttrs supportedSystems;

      mkSystem = system:
        let
          pkgs = import nixpkgs { inherit system; };

          # Use python3 (latest stable)
          python = pkgs.python3;

          # whisper.cpp is the AMD/Radeon STT path. Vulkan avoids tying Zara's
          # local voice acceleration to ROCm GPU support matrices.
          whisperCppVulkan = pkgs.whisper-cpp.override {
            vulkanSupport = true;
          };

          # The pinned nixpkgs revision predates its sherpa-onnx Python package.
          # sherpa-onnx 1.12.38 moved to ONNX Runtime 1.24.4 while this flake
          # carries ORT 1.23.2. Pin 1.12.37: it matches that ABI and already
          # includes Moonshine v2 support introduced in 1.12.28.
          sherpaOnnxWheel =
            let
              wheel =
                if system == "x86_64-linux" then {
                  file = "sherpa_onnx-1.12.37-cp313-cp313-manylinux2014_x86_64.manylinux_2_17_x86_64.whl";
                  url = "https://files.pythonhosted.org/packages/fb/d7/3a3eef865c85cf799baacca65f89ea9c89244e7f8f87cb029b8b4e65aca0/sherpa_onnx-1.12.37-cp313-cp313-manylinux2014_x86_64.manylinux_2_17_x86_64.whl";
                  sha256 = "39f58e758fbae54aa73171603db311a69d41b804ebdc0ad3d5a332064a9bc666";
                } else if system == "aarch64-linux" then {
                  file = "sherpa_onnx-1.12.37-cp313-cp313-manylinux2014_aarch64.manylinux_2_17_aarch64.whl";
                  url = "https://files.pythonhosted.org/packages/ae/ed/fbceec1edd8590a1f279b1bc278c96da1de8b9218971976791d9fa653e79/sherpa_onnx-1.12.37-cp313-cp313-manylinux2014_aarch64.manylinux_2_17_aarch64.whl";
                  sha256 = "bac7456a22ad0ee11378e2c20d5a6e7baa6a576690e6fd60962b88af83f57874";
                } else
                  throw "Unsupported sherpa-onnx wheel system: ${system}";
            in
            python.pkgs.buildPythonPackage rec {
              pname = "sherpa-onnx";
              version = "1.12.37";
              format = "wheel";

              src = pkgs.fetchurl {
                inherit (wheel) url sha256;
                name = wheel.file;
              };

              nativeBuildInputs = [ pkgs.patchelf ];
              buildInputs = [ pkgs.onnxruntime ];
              propagatedBuildInputs = [ python.pkgs.numpy ];

              # Upstream's manylinux wheel expects loader-visible ONNX Runtime
              # and the GCC C++ runtime. Nix keeps both in separate store paths,
              # so patch the native extensions before pythonImportsCheck.
              postInstall = ''
                native_libs=$(find "$out/${python.sitePackages}/sherpa_onnx" -type f -name '*.so')
                test -n "$native_libs" || {
                  echo "sherpa-onnx wheel contains no native libraries" >&2
                  exit 1
                }
                while IFS= read -r native_lib; do
                  patchelf --add-rpath "${pkgs.onnxruntime}/lib:${pkgs.stdenv.cc.cc.lib}/lib" "$native_lib"
                done <<< "$native_libs"
              '';

              doCheck = false;
              pythonImportsCheck = [ "sherpa_onnx" ];
            };

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

          # Official MCP Python SDK v2. Keep Zara's existing nixpkgs lock: the
          # current pin already satisfies the SDK's normal dependencies, but it
          # predates the parallel httpx2/httpcore2 packages. Package only those
          # pure-Python wheels plus MCP v2 here instead of bumping nixpkgs and
          # disturbing the sherpa/ONNX Runtime compatibility pin above.
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
            sherpaOnnxWheel
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
                  ${if withProlog then "--set ZARA_PROLOG_RLM_ROOT ${prolog-rlm}" else ""} \
                  ${if withProlog then "--set ZARA_RLM_SIDECAR $out/share/zarathushtra/modules/rlm_sidecar.pl" else ""} \
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
                --set ZARA_PROLOG_RLM_ROOT ${prolog-rlm} \
                --set ZARA_RLM_SIDECAR $out/share/zarathushtra/modules/rlm_sidecar.pl \
                --set SWI_HOME_DIR ${pkgs.swi-prolog}/lib/swipl \
                --run "cd $out/share/zarathushtra"
            '';
          };

          zarathushtra = pkgs.buildEnv {
            name = "zarathushtra-full";
            paths = [ zara-cli zara-desktop zara-prolog zara-wake zara-dictate ];
          };

          # Development wrappers intentionally execute the working checkout,
          # not the immutable package copy in the Nix store.
          zara-dev = pkgs.writeShellScriptBin "zara" ''
            export PYTHONPATH="$PWD''${PYTHONPATH:+:$PYTHONPATH}"
            export ZARA_PROLOG_RLM_ROOT="${prolog-rlm}"
            export ZARA_RLM_SIDECAR="$PWD/modules/rlm_sidecar.pl"
            exec ${pythonLibs}/bin/python -m zara "$@"
          '';

          zara-desktop-dev = pkgs.writeShellScriptBin "zara-desktop" ''
            export PYTHONPATH="$PWD''${PYTHONPATH:+:$PYTHONPATH}"
            export ZARA_PROLOG_RLM_ROOT="${prolog-rlm}"
            export ZARA_RLM_SIDECAR="$PWD/modules/rlm_sidecar.pl"
            exec ${pythonLibs}/bin/python -m zara.desktop.app "$@"
          '';

          checks = {
            # Run the Python test suite. The source tree is read-only in the
            # Nix store, so copy it to a writable scratch dir first so pytest
            # can write ``.pytest_cache`` and ``__pycache__``. Set HOME to a
            # writable temp dir so tests that resolve ``Path.home()`` work.
            pytest = pkgs.runCommand "zara-check-pytest"
              {
                nativeBuildInputs = [ pythonLibs pkgs.swi-prolog pkgs.makeWrapper ];
                src = ./.;
              }
              ''
                export HOME=$(mktemp -d)
                export XDG_CONFIG_HOME=$HOME/.config
                export XDG_RUNTIME_DIR=$(mktemp -d)
                export ZARA_DICTATION_PIDFILE=$XDG_RUNTIME_DIR/zara_dictation.pid
                export ZARA_DICTATION_LOGFILE=$XDG_RUNTIME_DIR/zara_dictation.log
                export ZARA_PROLOG_RLM_ROOT=${prolog-rlm}
                export LANG=C.UTF-8
                export LC_ALL=C.UTF-8
                cp -r $src $out-src
                chmod -R u+w $out-src
                cd $out-src
                export ZARA_RLM_SIDECAR="$out-src/modules/rlm_sidecar.pl"
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
                nativeBuildInputs = [ zara-cli zara-desktop zara-wake zara-dictate zara-prolog pkgs.bash ];
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
                zara-wake --help >$HOME/wake.out 2>&1 || true
                zara-console --help >$HOME/console.out 2>&1 || true
                zara-dictate --help >$HOME/dictate.out 2>&1 || true
                set -e
                test "$cli_rc" -eq 1
                grep -q "Zarathustra Voice Assistant" $HOME/cli.out
                grep -q -- "--desktop" $HOME/cli-help.out
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
              export ZARA_PROLOG_RLM_ROOT="${prolog-rlm}"
              export ZARA_RLM_SIDECAR="$PWD/modules/rlm_sidecar.pl"
              echo "Python + Whisper + whisper.cpp/Vulkan + SWI-Prolog + LangChain + MCP v2 ready"
              echo ""
              echo "Commands:"
              echo "  zara-desktop                   # Native desktop / Quick Copilot"
              echo "  zara --desktop                 # Same canonical desktop entry point"
              echo "  zara --wake                    # Wake listener"
              echo "  zara --wake --stt-provider whisper-cpp --device amd  # AMD/Vulkan STT"
              echo "  zara --console                 # Console mode"
              echo "  zara --dictate                 # Dictation mode"
              echo "  zara --agent                   # Direct agent conversation"
              echo "  zara mcp status               # Inspect MCP connections"
              echo ""
              echo "Build system:"
              echo "  nix build .#zara-desktop      # Build native desktop package"
              echo "  nix run .#zara-desktop        # Run native desktop / Quick Copilot"
              echo "  nix build                     # Build all packages"
              echo "  nix run                       # Run default CLI (prints help with no args)"
              echo "  nix run .#zara-wake           # Run wake listener"
              echo "  nix flake check               # Run all checks (pytest, scripts, syntax, prolog load)"
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
