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

          # Use python3 (latest stable)
          python = pkgs.python3;

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

          pythonLibs = python.withPackages (p: [
            p.sounddevice
            p.numpy
            p.pynput
            p.faster-whisper
            p.aiohttp
            p.soundfile
            p.pyyaml
            p.pydantic
            p.httpx
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
            # Testing
            p.pytest
            p.pytest-asyncio
            # Packaging metadata sanity checks
            p.setuptools
            p.wheel
          ]);

          # Shared derivation builder for the four Zara runtime packages.
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
            extraPath = [ pkgs.xdotool pkgs.pulseaudio pkgs.ffmpeg-full ];
          };

          zara-wake = mkZaraPackage {
            pname = "zara-wake";
            addFlags = "-m zara --wake";
            extraPath = [ pkgs.xdotool pkgs.pulseaudio pkgs.ffmpeg-full ];
          };

          zara-dictate = mkZaraPackage {
            pname = "zara-dictate";
            addFlags = "-m zara --dictate";
            withProlog = false;
            extraPath = [ pkgs.xdotool ];
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
            paths = [ zara-cli zara-prolog zara-wake zara-dictate ];
          };

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
                export LANG=C.UTF-8
                export LC_ALL=C.UTF-8
                cp -r $src $out-src
                chmod -R u+w $out-src
                cd $out-src
                export PYTHONPATH="$out-src''${PYTHONPATH:+:$PYTHONPATH}"
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
                nativeBuildInputs = [ zara-cli zara-wake zara-dictate zara-prolog pkgs.bash ];
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
                zara-wake --help >$HOME/wake.out 2>&1 || true
                zara-console --help >$HOME/console.out 2>&1 || true
                zara-dictate --help >$HOME/dictate.out 2>&1 || true
                set -e
                test "$cli_rc" -eq 1
                grep -q "Zarathustra Voice Assistant" $HOME/cli.out
                grep -q "usage:" $HOME/wake.out
                grep -q "usage:" $HOME/console.out
                grep -q "usage:" $HOME/dictate.out
                touch $out
              '';
          };
        in
        {
          packages = {
            zara-cli = zara-cli;
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
              pkgs.xdotool
              pkgs.ffmpeg-full  # Includes ffplay for streaming audio
              pkgs.mpv  # Alternative for streaming audio playback
              pkgs.portaudio
              pkgs.swi-prolog
              pkgs.pulseaudio
            ];

            shellHook = ''
              export PYTHONPATH="$PWD''${PYTHONPATH:+:$PYTHONPATH}"
              echo "Python + Whisper + SWI-Prolog + LangChain ready"
              echo ""
              echo "Commands:"
              echo "  zara --wake                    # Wake listener"
              echo "  zara --console                 # Console mode"
              echo "  zara --dictate                 # Dictation mode"
              echo "  zara --agent                   # Direct agent conversation"
              echo ""
              echo "Build system:"
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
