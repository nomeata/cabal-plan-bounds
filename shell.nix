with import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/8a307766007b3d904793657ae408b43927d13fff.tar.gz) { };
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [
    haskell.compiler.ghc8107
    haskell.compiler.ghc902
    haskell.compiler.ghc924
    haskell.compiler.ghc925
    haskell.compiler.ghc944
    haskell.compiler.ghc961
    ghcid
  ];
}
