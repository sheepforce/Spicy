let
  chemix = import ./chemix/default.nix;

  haskellNix = import (builtins.fetchGit {
    url = "https://github.com/input-output-hk/haskell.nix.git";
    name = "haskell.nix";
    rev = "e729389f3266e50602f36b7035c5e34177b705ac";
    ref = "master";
  }) {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

  pkgs = import ./nixpkgs.nix { inherit chemix haskellNix nixpkgsSrc; };

in
  import ./default.nix { inherit pkgs; }
