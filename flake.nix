{
  description = "Zarathushtra – Voice + Prolog Engine";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };

    python = pkgs.python311;
    pythonLibs = python.withPackages (p: [
      p.sounddevice
      p.numpy
      p.pynput
      p.faster-whisper
    ]);
  in {

    packages.${system} = let

      zara-prolog = pkgs.stdenv.mkDerivation {
        pname = "zara-prolog";
        version = "1.0";
        src = ./.;

        buildInputs = [ pkgs.swiProlog pkgs.makeWrapper ];

        installPhase = ''
          mkdir -p $out/share/zarathushtra
          mkdir -p $out/bin

          # Copy ALL Prolog sources with structure intact
          cp $src/*.pl $out/share/zarathushtra/ 2>/dev/null || true
          cp -r $src/kb $out/share/zarathushtra/
          cp -r $src/modules $out/share/zarathushtra/
          cp -r $src/scripts $out/share/zarathushtra/

          # zara-console (REPL)
          cat > $out/bin/zara-console <<'EOF'
#!/usr/bin/env bash
cd ${placeholder "out"}/share/zarathushtra
exec ${pkgs.swiProlog}/bin/swipl -q -s ${placeholder "out"}/share/zarathushtra/main.pl
EOF
          chmod +x $out/bin/zara-console
        '';
      };

      ################################
      ## WAKE WORD LISTENER
      ################################
      zara-wake = pkgs.stdenv.mkDerivation {
        pname = "zara-wake";
        version = "1.0";
        src = ./.;

        nativeBuildInputs = [ pkgs.makeWrapper ];

        installPhase = ''
          mkdir -p $out/bin

          # Copy the script
          cp scripts/zara_wake.py $out/bin/.zara-wake-unwrapped
          chmod +x $out/bin/.zara-wake-unwrapped

          # Patch the PROLOG_MAIN path to point at zara-prolog package
          substituteInPlace $out/bin/.zara-wake-unwrapped \
            --replace 'PROLOG_MAIN = Path(__file__).parent / "main.pl"' \
                      'PROLOG_MAIN = Path("${zara-prolog}/share/zarathushtra/main.pl")'

          # Create wrapper with correct Python interpreter and environment
          makeWrapper ${pythonLibs}/bin/python3 $out/bin/zara-wake \
            --add-flags $out/bin/.zara-wake-unwrapped \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.xdotool pkgs.pulseaudio pkgs.swiProlog ]} \
            --set PYTHONPATH ${pythonLibs}/${python.sitePackages} \
            --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath [ pkgs.libsndfile pkgs.portaudio ]}
        '';
      };

      ################################
      ## DICTATION
      ################################
      zara-dictate = pkgs.stdenv.mkDerivation {
        pname = "zara-dictate";
        version = "1.0";
        src = ./.;

        nativeBuildInputs = [ pkgs.makeWrapper ];

        installPhase = ''
          mkdir -p $out/bin

          # Copy the script
          cp scripts/zara_dictate.py $out/bin/.zara-dictate-unwrapped
          chmod +x $out/bin/.zara-dictate-unwrapped

          # Create wrapper with correct Python interpreter
          makeWrapper ${pythonLibs}/bin/python3 $out/bin/zara-dictate \
            --add-flags $out/bin/.zara-dictate-unwrapped \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.xdotool ]} \
            --set PYTHONPATH ${pythonLibs}/${python.sitePackages} \
            --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath [ pkgs.libsndfile pkgs.portaudio ]}
        '';
      };

      zarathushtra = pkgs.buildEnv {
        name = "zarathushtra-full";
        paths = [ zara-prolog zara-wake zara-dictate ];
      };

    in {
      zara-prolog = zara-prolog;
      zara-wake = zara-wake;
      zara-dictate = zara-dictate;
      zarathushtra = zarathushtra;
      default = zarathushtra;
    };

    apps.${system} = {
      zara-wake = {
        type = "app";
        program = "${self.packages.${system}.zara-wake}/bin/zara-wake";
      };
      zara-console = {
        type = "app";
        program = "${self.packages.${system}.zara-prolog}/bin/zara-console";
      };
      zara-dictate = {
        type = "app";
        program = "${self.packages.${system}.zara-dictate}/bin/zara-dictate";
      };
      default = {
        type = "app";
        program = "${self.packages.${system}.zara-wake}/bin/zara-wake";
      };
    };

    ################################
    ## DEV SHELL
    ################################
    devShells.${system}.default = pkgs.mkShell {
      name = "zarathushtra-dev-shell";

      buildInputs = [
        pythonLibs
        pkgs.xdotool
        pkgs.ffmpeg
        pkgs.portaudio
        pkgs.swiProlog
        pkgs.pulseaudio
      ];

      shellHook = ''
        echo "🔥 Zarathushstra DevShell"
        echo "Python + Whisper + SWI-Prolog ready"
        echo ""
        echo "Commands:"
        echo "  python scripts/zara_wake.py    # Direct dev run"
        echo "  swipl -s main.pl               # Test REPL"
        echo ""
        echo "Build system:"
        echo "  nix build                      # Build all"
        echo "  nix run                        # Run wake listener"
      '';
    };
  };
}
