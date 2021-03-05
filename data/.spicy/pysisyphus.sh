#! /usr/bin/env bash
nix-shell /data/WiP/Git/pysisyphus/nix/shell.nix --pure --run "pysis $@" |& tee "Pysis.log"
