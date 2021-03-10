let
  pkgs = import ./nixpkgs.nix {};
  hsPkgs = import ./default.nix { wrap = false; };
in
  hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      spicy
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    tools = {
      cabal = "3.4.0.0";
      hlint = "2.2.11";
      haskell-language-server = "1.0.0.0"; # Hackage versions should be available
      hpack = "0.34.3";
      ormolu = "0.1.4.1";
    };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = [
      pkgs.llvm_9
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
