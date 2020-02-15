with (import <nixpkgs> {});
let old-ghc = import /home/matt/old-ghc-nix {pkgs = (import <nixpkgs> {});};
    ghc = old-ghc.mkGhc { url = "https://gitlab.haskell.org/ghc/ghc/-/jobs/261242/artifacts/raw/ghc-x86_64-fedora27-linux.tar.xz"; hash = "0v2ci6k358jad9p9rjwnrx89v6c8k40g8pqxs5r6yczz11lfm79s" ;};
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
