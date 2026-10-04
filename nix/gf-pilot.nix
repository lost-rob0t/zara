# An isolated, pinned experimental shell; does not replace Zara's default shell.
let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
    sha256 = lock.nodes.nixpkgs.locked.narHash;
  };
  pkgs = import nixpkgs {};
  gfPilot = pkgs.stdenv.mkDerivation {
    pname = "zara-gf-pilot-toolchain";
    version = "3.12.0";
    src = pkgs.fetchurl {
      url = "https://github.com/GrammaticalFramework/gf-core/releases/download/release-3.12/gf-3.12-ubuntu-24.04.deb";
      sha256 = "17aa5452b713f1e00a0755a1bad998a926acbffb96eefde2c3800ca72536b4d8";
    };
    nativeBuildInputs = [ pkgs.dpkg pkgs.autoPatchelfHook ];
    buildInputs = [ pkgs.gmp pkgs.libffi pkgs.ncurses ];
    unpackPhase = "dpkg-deb -x $src unpacked";
    installPhase = ''
      # The Debian distribution also contains a GHC development library and
      # a Python 3.12 extension. Neither belongs in this CLI/C-runtime shell.
      mkdir -p $out/bin $out/lib/pkgconfig $out/include $out/share
      cp unpacked/usr/bin/gf $out/bin/
      cp -a unpacked/usr/lib/libpgf.so* unpacked/usr/lib/libgu.so* $out/lib/
      cp -a unpacked/usr/include/pgf unpacked/usr/include/gu $out/include/
      cp unpacked/usr/lib/pkgconfig/libpgf.pc unpacked/usr/lib/pkgconfig/libgu.pc $out/lib/pkgconfig/
      cp -a unpacked/usr/share/. $out/share/
      for pc in $out/lib/pkgconfig/*.pc; do
        substituteInPlace "$pc" --replace-fail 'prefix=/usr' "prefix=$out"
      done
    '';
    dontBuild = true;
    meta.platforms = [ "x86_64-linux" ];
  };
  rgl = builtins.fetchGit {
    url = "https://github.com/GrammaticalFramework/gf-rgl.git";
    rev = "caaa5ab8716b74bb7c043ec5efc5ae37c025db9e";
  };
in pkgs.mkShell {
  packages = [ gfPilot pkgs.gcc pkgs.pkg-config pkgs.python3 pkgs.swi-prolog ];
  GF_RGL_ROOT = rgl;
  GF_RGL_REV = "caaa5ab8716b74bb7c043ec5efc5ae37c025db9e";
}
