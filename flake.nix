{
  description = "Zarathushtra – Simple voice asistant  ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };

    # FIX: you referenced `python` but never defined it
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

      buildInputs = [ pkgs.swiProlog ];

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
      pyswip
      # Packages not in nixpkgs (install via pip if needed):
      #   pip install anthropic openai
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
          cp -r $src/zara $out/share/zarathushtra/

          # zara-console (Python wrapper)
          makeWrapper ${pythonLibs}/bin/python3 $out/bin/zara-console \
            --add-flags "-m zara --console" \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.swiProlog ]} \
            --set PYTHONPATH $out/share/zarathushtra:${pythonLibs}/${python.sitePackages} \
            --set SWI_HOME_DIR ${pkgs.swiProlog}/lib/swipl \
            --run "cd $out/share/zarathushtra"
        '';
      };

      zara-wake = pkgs.stdenv.mkDerivation {
        pname = "zara-wake";
        version = "1.0";
        src = ./.;

        nativeBuildInputs = [ pkgs.makeWrapper ];

        installPhase = ''
          mkdir -p $out/bin
          mkdir -p $out/lib/python

          # Copy the zara Python module
          cp -r $src/zara $out/lib/python/

          # Copy the script
          cp scripts/zara_wake.py $out/bin/.zara-wake-unwrapped
          chmod +x $out/bin/.zara-wake-unwrapped

          # Patch the PROLOG_MAIN path to point at zara-prolog package
          substituteInPlace $out/bin/.zara-wake-unwrapped \
            --replace 'PROLOG_MAIN = pathlib.Path(__file__).parent / "main.pl"' \
                      'PROLOG_MAIN = pathlib.Path("${zara-prolog}/share/zarathushtra/main.pl")'

          # Create wrapper with correct Python interpreter and environment
          makeWrapper ${pythonLibs}/bin/python3 $out/bin/zara-wake \
            --add-flags $out/bin/.zara-wake-unwrapped \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.xdotool pkgs.pulseaudio pkgs.swiProlog ]} \
            --set PYTHONPATH $out/lib/python:${pythonLibs}/${python.sitePackages} \
            --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath [ pkgs.libsndfile pkgs.portaudio ]} \
            --set SWI_HOME_DIR ${pkgs.swiProlog}/lib/swipl
        '';
      };

      zara-dictate = pkgs.stdenv.mkDerivation {
        pname = "zara-dictate";
        version = "1.0";
        src = ./.;

        nativeBuildInputs = [ pkgs.makeWrapper ];

        installPhase = ''
          mkdir -p $out/bin
          mkdir -p $out/lib/python

          # Copy the zara Python module
          cp -r $src/zara $out/lib/python/

          # Create wrapper using Python module interface
          makeWrapper ${pythonLibs}/bin/python3 $out/bin/zara-dictate \
            --add-flags "-m zara --dictate" \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.xdotool ]} \
            --set PYTHONPATH $out/lib/python:${pythonLibs}/${python.sitePackages} \
            --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath [ pkgs.libsndfile pkgs.portaudio ]}
        '';
      };

      zara-cli = pkgs.stdenv.mkDerivation {
        pname = "zara-cli";
        version = "1.0";
        src = ./.;

        nativeBuildInputs = [ pkgs.makeWrapper ];

        installPhase = ''
          mkdir -p $out/bin
          mkdir -p $out/lib/python
          mkdir -p $out/share/zarathushtra

          # Copy the zara Python module
          cp -r $src/zara $out/lib/python/

          # Copy ALL Prolog sources with structure intact
          cp $src/*.pl $out/share/zarathushtra/ 2>/dev/null || true
          cp -r $src/kb $out/share/zarathushtra/
          cp -r $src/modules $out/share/zarathushtra/
          cp -r $src/scripts $out/share/zarathushtra/

          # Create main zara wrapper
          makeWrapper ${pythonLibs}/bin/python3 $out/bin/zara \
            --add-flags "-m zara" \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.xdotool pkgs.pulseaudio pkgs.swiProlog ]} \
            --set PYTHONPATH $out/lib/python:$out/share/zarathushtra:${pythonLibs}/${python.sitePackages} \
            --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath [ pkgs.libsndfile pkgs.portaudio ]} \
            --set SWI_HOME_DIR ${pkgs.swiProlog}/lib/swipl \
            --run "cd $out/share/zarathushtra"
        '';
      };

      zarathushtra = pkgs.buildEnv {
        name = "zarathushtra-full";
        paths = [ zara-cli zara-prolog zara-wake zara-dictate ];
      };

    in {
      zara-cli = zara-cli;
      zara-prolog = zara-prolog;
      zara-wake = zara-wake;
      zara-dictate = zara-dictate;
      zarathushtra = zarathushtra;
      default = zarathushtra;
    };

    apps.${system} = {
      zara = {
        type = "app";
        program = "${self.packages.${system}.zara-cli}/bin/zara";
      };
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
        program = "${self.packages.${system}.zara-cli}/bin/zara";
      };
    };

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
        echo "Python + Whisper + SWI-Prolog ready"
        echo ""
        echo "Commands:"
        echo "  python scripts/zara_wake.py    # Direct dev run"
        echo "  swipl -s main.pl               # Test REPL (zara console)"
        echo ""
        echo "Build system:"
        echo "  nix build                      # Build all"
        echo "  nix run                        # Run wake listener"
      '';
    };
  };
}
