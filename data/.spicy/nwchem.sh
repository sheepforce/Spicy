#!/bin/env/bash
nix-shell --pure -p nwchem --run "mpiexec $@"
