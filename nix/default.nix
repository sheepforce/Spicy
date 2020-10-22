let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.haskell-nix.project {

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "spicy";
      src = ./..;
    };

    compiler-nix-name = "ghc8102";
  }
