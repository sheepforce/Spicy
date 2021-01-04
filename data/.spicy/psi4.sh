#!/usr/bin/env bash
nix-shell --pure -p psi4_9b60184 --run "psi4 $@"
