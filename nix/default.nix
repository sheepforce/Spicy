{ pkgs ? import ./nixpkgs.nix {} }:

pkgs.haskell-nix.project {

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "spicy";
    src = ./..;
  };

  compiler-nix-name = "ghc8102";
}
