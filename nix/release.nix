{ static ? false
, wrap ? !static
}:

let
  chemix = import ./chemix/default.nix;

  haskellNix = import (builtins.fetchGit {
    url = "https://github.com/input-output-hk/haskell.nix.git";
    name = "haskell.nix";
    rev = "1498e70fcbae4b008733f9b5bf212f04f6a4fa0a";
    ref = "master";
  }) {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

  pkgs = import ./nixpkgs.nix { inherit chemix haskellNix nixpkgsSrc; };

in
  import ./default.nix { inherit pkgs static wrap; }
