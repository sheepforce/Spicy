let
  pkgs = import ./pkgs.nix;
  haskellPkgs = pkgs.haskellPkgs;
  nixpkgs = pkgs.nixpkgs;
  spicyPkgs = import ./default.nix { wrap = false; };

  # Spicy runtime configuration setup.
  spicyrc = with nixpkgs;writeTextFile {
    name = "spicyrc";
    text = lib.generators.toYAML {} ({
      "psi4" = "${qchem.psi4Unstable}/bin/psi4";
      "gdma" = "${qchem.gdma}/bin/gdma";
      "pysisyphus" = "${qchem.pysisyphus}/bin/pysis";
      "xtb" = "${qchem.xtb}/bin/xtb";
      "ipi" = "${qchem.i-pi}/bin/i-pi";
    } // lib.attrsets.optionalAttrs (qchem.turbomole != null) {"turbomole" = "${qchem.turbomole}/bin/turbomole";}
    );
  };
in
  spicyPkgs.shellFor {
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
      haskell-language-server = "1.2.0.0"; # Hackage versions should be available
      hpack = "0.34.4";
      ormolu = "0.1.4.1";
    };

    # Some you may need to get some other way.
    buildInputs = with nixpkgs; [
      niv
      qchem.pysisyphus
      qchem.psi4Unstable
      qchem.gdma
      qchem.xtb
    ];

    # Setup a runtime with QC wrappers available.
    shellHook = ''
      export SPICYRC=${spicyrc}
    '';

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
