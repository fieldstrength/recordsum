{ pkgs ? import ./haskell-pkgs.nix
, haskellCompiler ? "ghc8104"
, additionalModules ? {}
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "recordsum";
    src = ./.;
  };
  compiler-nix-name = haskellCompiler;
  modules = [
    additionalModules
  ];
}
