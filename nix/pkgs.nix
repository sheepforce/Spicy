let
  sources = import ./sources.nix;
  haskellNix = import sources."haskell.nix" { };
  qchem = import sources.NixOS-QChem;
  postOverlay = self: super:
    { qchem = super.qchem // {
        qdng = null;
        mesa = null;
        gaussview = null;
        mctdh = null;
        mesa-qc = null;
        molpro12 = null;
        molpro20 = null;
        molpro = null;
        molpro-ext = null;
      };
    };

  haskellPkgs = import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [ qchem postOverlay ];
    config = haskellNix.nixpkgsArgs.config // {
      allowUnfree = true;
      qchem-config = {
        allowEnv = true;
        optAVX = false;
      };
    };
  });

  nixpkgs = import sources.nixpkgs {
    overlays = [ qchem postOverlay ];
    config = haskellNix.nixpkgsArgs.config // {
      allowUnfree = true;
      qchem-config = {
        allowEnv = true;
        optAVX = false;
      };
    };
  };

  # Pysisyphus development version
  pysisyphus = nixpkgs.python3.pkgs.callPackage "${sources.pysisyphus}/nix" { };
in { inherit haskellPkgs nixpkgs pysisyphus; }
