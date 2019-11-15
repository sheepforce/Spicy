# Spicy
Spicy is a Haskell program for quantum chemistry and quantum dynamics and aims at providing a set of composable ONIOM methods, that are not widely available.
Spicy will not implement quantum chemistry calculations itself, but rather wrap different quantum chemistry programs.

## Installing / Building
Spicy is written in Haskell and developed with [Stack](https://docs.haskellstack.org/en/stable/README/), which provides an easy to use build system.

To install/build Spicy from source follow the following steps:

1. Make sure to have all the dependencies installed. These are:
  - [Stack](https://docs.haskellstack.org/en/stable/README/)
  - LLVM 6.0.x and LLVM 8.0.x (both are necessary) in the dynamically linked version
  - libffi
  - Optional
    - [CUDA Toolkit](https://developer.nvidia.com/cuda-toolkit)

2. Make sure, `libffi.so.7`, the LLVM libraries and the LLVM executables are available in your path.
If you want to build with NVidia GPU support, also everything from the CUDA toolkit needs to be available on your path.

3. Clone the Spicy repository and build the code.
Cabal flags are available to use, to switch to a development build (faster and warnings are not errors), or enable NVidia GPU support. The flags are `dev` and `cuda` and can be added to the stack build line with `--flag spicy:dev` and `--flag spicy:cuda`. For example:
```
git clone https://gitlab.com/theoretical-chemistry-jena/quantum-chemistry/Spicy.git
cd Spicy
stack install --flag spicy:cuda
```
It is recommended to run the test suite for Spicy by executing
```
stack test
```
with the same flags, as the `stack install` command.

4. (Optional) If you require the documentation of the source code, build the docs with
```
stack haddock
```
with the same flags as used above.

Alternatively, you can use the executables provided for point releases for linux systems, which require no Haskell libraries and dependencies but LLVM 8.0.x with shared libraries.

## Contributing and Code
For coding style and for how to contribute, see [CONTRIBUTING.md](https://gitlab.com/theoretical-chemistry-jena/quantum-chemistry/Spicy/blob/develop/CONTRIBUTING.md).
