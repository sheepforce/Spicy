let
  # Load the current master of the Chemix overlay with Quantum chemistry packages.
  chemix = import "${builtins.fetchTarball "https://gitlab.com/theoretical-chemistry-jena/nixwithchemistry/-/archive/master/nixwithchemistry-master.tar.bz2"}/default.nix";

  # The Haskell.nix infrastructure.
  haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {};

  # A patched version of the nixos-20.09 channel with potentially more cache hits for Haskell.nix.
  # Could also be normal '<nixos>' or something.
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

  # Arguments to pass to the import of nixpkgs. This includes the fulll Haskell.nix overlay and some
  # configuration options.
  haskellNixArgs = haskellNix.nixpkgsArgs;

  # Combine the Chemix and Haskell.nix overlay.
  allOverlays = haskellNixArgs.overlays ++ [chemix];
  nixpkgsArgs = haskellNixArgs // { overlays = allOverlays; };

  # The final package set with all overlays applied.
  pkgs = import nixpkgsSrc nixpkgsArgs;
in
  pkgs.haskell-nix.project {

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "spicy";
      src = ./..;
    };

    compiler-nix-name = "ghc8102";
  }
