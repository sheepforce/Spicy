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

  # FIXME - would be better to use sources.nixpkgs instead of haskellNix.sources.nixpkgs-unstable.
  # Blocked by https://github.com/input-output-hk/haskell.nix/issues/1127
  # Also blocks further NixOS-QChem updates (require newer nixpkgs-unstable versions)
  pkgs = import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [ qchem postOverlay ];
    config = haskellNix.nixpkgsArgs.config // {
      allowUnfree = true;
      qchem-config = {
        allowEnv = true;
        optAVX = false;
      };
    };
  });
in pkgs
