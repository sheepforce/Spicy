{ # The Chemix overlay repository. Defaults to the current master branch. Can be pinned from outside.
  chemix ? import ./chemix/default.nix

  # The Haskell.nix repository with the Haskell infrastructure. Defaults to the current master.
, haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}

  # A Nixpkgs channel. Defaults to a rebuild version channel of nixos-20.09 from Haskell.nix,
  # but also <nixos>, ... are possible.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
}:

let
  # Arguments to pass to the import of nixpkgs. This includes the full Haskell.nix overlay and some
  # configuration options.
  haskellNixArgs = haskellNix.nixpkgsArgs;

  # Combine the Chemix and Haskell.nix overlay.
  allOverlays = haskellNixArgs.overlays ++ [ chemix ];
  nixpkgsArgs = haskellNixArgs // { overlays = allOverlays; };

  # The final package set with all overlays applied.
  pkgs = import nixpkgsSrc nixpkgsArgs;

in
  pkgs
