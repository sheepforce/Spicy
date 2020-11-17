let
  chemix = import ./chemix/default.nix;

  haskellNix = import (builtins.fetchGit {
    url = "https://github.com/input-output-hk/haskell.nix.git";
    name = "haskell.nix";
    rev = "2f48630357ea61c5a231273a9cdf9e71f9653c81";
    ref = "master";
  }) {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

  pkgs = import ./nixpkgs.nix { inherit chemix haskellNix nixpkgsSrc; };

in
  import ./default.nix { inherit pkgs; }
