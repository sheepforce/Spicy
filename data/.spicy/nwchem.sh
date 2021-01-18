#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nwchem --pure
mpiexec $@
