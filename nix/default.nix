{ pkgs ? import ./nixpkgs.nix {}
, static ? false
}:

pkgs.haskell-nix.project {

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "spicy";
    src = ./..;
  };

  compiler-nix-name = "ghc8102";

  configureArgs = if static
    then builtins.toString [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-static"
      "--constraint=\"hmatrix +no-random_r\""
    ] else "";
}
