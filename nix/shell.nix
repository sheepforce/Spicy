let
  # Package setup.
  pkgs = import ./nixpkgs.nix {};
  hsPkgs = import ./default.nix { wrap = false; };

  # Spicy runtime configuration setup.
  spicyrc = pkgs.writeTextFile {
    name = "spicyrc";
    text = pkgs.lib.generators.toYAML {} {
      "psi4" = "${pkgs.psi4}/bin/psi4";
      "gdma" = "${pkgs.gdma}/bin/gdma";
      "pysisyphus" = "${pkgs.pysisyphus}/bin/pysis";
    };
  };
in
  hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      spicy
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # Some common tools can be added with the `tools` argument
    # See overlays/tools.nix for more details
    tools = {
      cabal = "3.4.0.0";
      hlint = "3.3";
      haskell-language-server = "1.0.0.0"; # Hackage versions should be available
      hpack = "0.34.4";
      ormolu = "0.1.4.1";
    };

    # Some you may need to get some other way.
    buildInputs = [
      pkgs.llvm_9
      pkgs.pysisyphus
      pkgs.psi4Unstable
      pkgs.gdma
    ];

    # Setup a runtime with QC wrappers available.
    shellHook = with hsPkgs; ''
      export SPICYRC=${spicyrc}
    '';

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
