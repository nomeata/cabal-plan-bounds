with import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/b2d2b9e5f4f3659e4ef6b11a31028a90771d943b.tar.gz) { };
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [
    haskell.compiler.ghc8107
    haskell.compiler.ghc902
    haskell.compiler.ghc924
    haskell.compiler.ghc925
    haskell.compiler.ghc944
    haskell.compiler.ghc962
    haskell.compiler.ghc981
    ghcid
  ];
}
