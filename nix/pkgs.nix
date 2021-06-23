let
  sources = import ./sources.nix;
  haskellNix = import sources."haskell.nix" { };
  qchemOvl = import sources.NixOS-QChem;
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
    overlays = haskellNix.nixpkgsArgs.overlays;
    config = haskellNix.nixpkgsArgs.config // {
      allowUnfree = true;
      qchem-config = {
        allowEnv = true;
        optAVX = false;
      };
    };
  });

  nixpkgs = import sources.nixpkgs {
    overlays = [ qchemOvl postOverlay ];
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

  # Development version of define, which generates simple input files.
  tmDefine = with nixpkgs; if (qchem.turbomole != null)
    then callPackage ./turbomole/define.nix { turbomole = qchem.turbomole; }
    else null
  ;
in { inherit haskellPkgs nixpkgs pysisyphus tmDefine; }
