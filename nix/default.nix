{ static ? false, wrap ? !static }:
let
  pkgs = import ./pkgs.nix;
  nixpkgs = pkgs.nixpkgs;
  haskellPkgs = pkgs.haskellPkgs;

  # Configuration File.
  spicyrc = with nixpkgs; writeTextFile {
    name = "spicyrc";
    text = lib.generators.toYAML {} ({
      "psi4" = "${qchem.psi4Unstable}/bin/psi4";
      "gdma" = "${qchem.gdma}/bin/gdma";
      "pysisyphus" = "${qchem.pysisyphus}/bin/pysis";
      "xtb" = "${qchem.xtb}/bin/xtb";
      "ipi" = "${qchem.i-pi}/bin/i-pi";
    } // lib.attrsets.optionalAttrs (qchem.turbomole != null) {"turbomole" = "${qchem.turbomole}/bin";}
    );
  };

  buildPkgs = if static then haskellPkgs.pkgsCross.musl64 else haskellPkgs;

in with buildPkgs; haskell-nix.project {
  src = haskell-nix.haskellLib.cleanGit {
    name = "spicy";
    src = ./..;
  };

  compiler-nix-name = "ghc8105";

  configureArgs = if static
    then builtins.toString [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-static"
    ] else "";

  modules = [
    { packages.spicy.components.exes.spicy.postInstall = if wrap then ''
        # Make the wrapper functions available.
        source ${makeWrapper}/nix-support/setup-hook

        # Generate a SpicyRC file for dependencies.
        wrapProgram $out/bin/spicy \
          --set SPICYRC ${spicyrc}
      '' else "";
    }
  ];
}
