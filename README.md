# Clash Library Support for Floating Point Numbers with FloPoCo

This library provides Clash support for floating-point numbers as used by FloPoCo. It relies on **hmpfr** version 0.4.5, which is the Haskell binding library for **MPFR**—the same floating-point library used by FloPoCo.

## Compatibility Note

Currently, version 0.4.5 of **hmpfr** has build errors when using **GHC 9.4.4** and above. This is because GHC 9.4.4 and newer versions use the **Clang** toolchain instead of **GCC**. To use this library, you must ensure that **MPFR** and **GMP** libraries are built with **Clang**.

## Key Changes in MPFR Usage

1. Add `Prelude` to every Haskell file.
2. Define `type Exp` as `CLong`.
