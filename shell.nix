with (import <nixpkgs> {});
let old-ghc = import /home/matt/old-ghc-nix {pkgs = (import <nixpkgs> {});};
ghc = old-ghc.mkGhc { url = "https://gitlab.haskell.org/ghc/ghc/-/jobs/274208/artifacts/raw/ghc-x86_64-fedora27-linux.tar.xz"
                    ; hash = "1bpjkpki2vbsvgs2j3bj8r7fm268ias1x7674kmkijlawmmb1y7x" ;};
in
stdenv.mkDerivation {
  name = "haskell-ide-engine";
  buildInputs = [
    gmp
    zlib
    ncurses
    jq
    haskellPackages.cabal-install
    ghc
  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
