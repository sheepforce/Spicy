{ static ? false, wrap ? !static }:
let
  pkgs = import ./pkgs.nix;

  # Configuration File.
  spicyrc = pkgs.writeTextFile {
    name = "spicyrc";
    text = pkgs.lib.generators.toYAML {} ({
      "psi4" = "${pkgs.qchem.psi4Unstable}/bin/psi4";
      "gdma" = "${pkgs.qchem.gdma}/bin/gdma";
      "pysisyphus" = "${pkgs.qchem.pysisyphus}/bin/pysis";
      "xtb" = "${pkgs.qchem.xtb}/bin/xtb";
    } // pkgs.lib.attrsets.optionalAttrs (pkgs.qchem.turbomole != null) {"turbomole" = "${pkgs.qchem.turbomole}/bin/turbomole";}
    );
  };

  buildPkgs = if static then pkgs.pkgsCross.musl64 else pkgs;

in buildPkgs.haskell-nix.project {
  src = buildPkgs.haskell-nix.haskellLib.cleanGit {
    name = "spicy";
    src = ./..;
  };

  compiler-nix-name = "ghc8104";

  configureArgs = if static
    then builtins.toString [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-static"
    ] else "";

  modules = [
    { packages.spicy.components.exes.spicy.postInstall = if wrap then ''
        # Make the wrapper functions available.
        source ${buildPkgs.makeWrapper}/nix-support/setup-hook

        # Generate a SpicyRC file for dependencies.
        wrapProgram $out/bin/spicy \
          --set SPICYRC ${spicyrc}
      '' else "";
    }
  ];
}
