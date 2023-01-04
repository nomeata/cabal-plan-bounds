with import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/718eba0289831c676963f253b19aaf8129fa77d3.tar.gz) { };
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [
    haskell.compiler.ghc8107
    haskell.compiler.ghc902
    haskell.compiler.ghc924
    haskell.compiler.ghc925
    haskell.compiler.ghc944
    ghcid
  ];
}
