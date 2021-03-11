let
  haskellNix = import (builtins.fetchTarball{
    url = "https://github.com/input-output-hk/haskell.nix/archive/454a31355f86fad3dfcb9e8ff26b85de78c6bdb9.tar.gz";
  }) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
  import nixpkgsSrc nixpkgsArgs
