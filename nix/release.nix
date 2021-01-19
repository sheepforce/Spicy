{ static ? false }:

let
  chemix = import ./chemix/default.nix;

  haskellNix = import (builtins.fetchGit {
    url = "https://github.com/input-output-hk/haskell.nix.git";
    name = "haskell.nix";
    rev = "18a5a454d0160fecb0f903263cedb43003049d95";
    ref = "master";
  }) {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

  pkgs = import ./nixpkgs.nix { inherit chemix haskellNix nixpkgsSrc; };

in
  import ./default.nix { inherit pkgs static; }
