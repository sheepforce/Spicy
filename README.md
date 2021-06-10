# Spicy
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
[![pipeline status](https://gitlab.com/theoretical-chemistry-jena/quantum-chemistry/Spicy/badges/develop/pipeline.svg)](https://gitlab.com/theoretical-chemistry-jena/quantum-chemistry/Spicy/-/commits/develop)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Spicy is a Haskell program for quantum chemistry and quantum dynamics and aims at providing a set of composable ONIOM methods, that are not widely available.
Spicy will not implement quantum chemistry calculations itself, but rather wrap different quantum chemistry programs.

## Installing / Building
### Nix
Spicy can be build with Nix.
This guarantees fully reproducible builds and saves you from the hurdles of installing *many* packages yourself.
Make sure to have the [Nix package manager installed on your system](https://nixos.org/download.html).
To save some time (aka many hours and gigabytes of RAM ...) during building, you may want to add the [Haskell.nix hydra cache](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/#setting-up-the-binary-cache).
From within the `nix` directory execute
```
nix-build --arg wrap true -A spicy.components.exes
```
or to obtain a Haskell development shell with toolings in the top-level directory
```
nix-shell
```

### Manually
To build Spicy without Nix, you will need a [working Haskell toolchain](https://www.haskell.org/platform/#linux-generic) including a recent GHC (8.10) and cabal.
The runtime dependencies (quantum chemistry software) need to be installed and configured manually configured.
- [Psi4](https://psicode.org/)
- [GDMA](https://gitlab.com/anthonyjs/gdma)
- [XTB](https://xtb-docs.readthedocs.io/en/latest/contents.html#)
- [Pysisyphus](https://pysisyphus.readthedocs.io/en/latest/)

A YAML file is required to point Spicy to the executables (or wrapper scripts with same call convention), e.g.
```yaml
psi4: /opt/psi4/bin/psi4
xtb: /opt/xtb/bin/xtb
gdma: /opt/gdma/bin/gdma
pysisyphus: /opt/pysisyphus/bin/pysis
```
The environment variable `SPICYRC` should point to this YAML file.
