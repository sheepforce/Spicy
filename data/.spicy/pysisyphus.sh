#! /usr/bin/env bash
nix-shell /scratch/Git/pysisyphus/nix/shell.nix --pure --run "pysis $@"
