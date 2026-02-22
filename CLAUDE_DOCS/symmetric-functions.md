# Feature: Symmetric Functions Library
> Five classical bases (Schur, monomial, homogeneous, elementary, power sum) with full change-of-basis and multiplication, using phantom types for compile-time basis safety.

## Overview

The `QHask.Sym.*` modules port Sage-combinat's symmetric function machinery to Haskell. All symmetric functions are represented as `SymFun b` — a `Map Partition Rational` tagged with a phantom type `b` that tracks which basis the function is expressed in. This prevents accidentally mixing bases without an explicit conversion.

All conversions route through **Schur as hub**: to go from basis A to basis B, the path is A -> Schur -> B.

### Module dependency graph

```
QHask.Partition  (containedIn, dominates)
    |
QHask.Sym.Types  (SymFun, Basis class, arithmetic)
    |
QHask.Sym.Kostka  (SSYT enumeration, Kostka numbers, inverse)
    |
QHask.Sym.LR  (Littlewood-Richardson coefficients)
    |
QHask.Sym.Schur  (s_lambda, multiplication via LR)
    |--- QHask.Sym.Monomial    (m_mu, via Kostka)
    |--- QHask.Sym.Homogeneous (h_mu, direct mul)
    |--- QHask.Sym.Elementary  (e_mu, via omega/conjugate)
    |--- QHask.Sym.PowerSum    (p_mu, Murnaghan-Nakayama)
    |
QHask.Sym  (umbrella: changeBasis, omega, innerProduct)
```

### Key algorithms
- **Kostka numbers**: SSYT enumeration via horizontal strip chains
- **Inverse Kostka**: Back-substitution over dominance order (upper-unitriangular)
- **LR rule**: Skew SSYT with incremental lattice word check
- **Murnaghan-Nakayama**: Border strip (rim hook) enumeration for characters
- **Direct multiplication**: h, e, p multiply by merging and sorting parts

### Change-of-basis matrix directions
- `toSchur` for Homo: multiply coordinate vector by K (Kostka matrix)
- `fromSchur` for Homo: multiply by K^{-1} — note indices are `(K^{-1})_{mu,lambda}`, not `(K^{-1})_{lambda,mu}`
- `toSchur` for Elem: uses K with conjugate shape: `K_{lambda', mu}`
- `toSchur` for Mono: uses transpose of inverse Kostka: `(K^{-1})_{mu, lambda}`
- Power sum uses characters via Murnaghan-Nakayama rule

## Resources
- [QHask CLAUDE.md](/Users/robin/git/haskell/CLAUDE.md) - project overview

## Assets
- `src/QHask/Sym/Types.hs` - Core types, Basis class, arithmetic
- `src/QHask/Sym/Kostka.hs` - SSYT, Kostka numbers, inverse
- `src/QHask/Sym/LR.hs` - Littlewood-Richardson rule
- `src/QHask/Sym/Schur.hs` - Schur basis
- `src/QHask/Sym/Monomial.hs` - Monomial basis
- `src/QHask/Sym/Homogeneous.hs` - Complete homogeneous basis
- `src/QHask/Sym/Elementary.hs` - Elementary basis
- `src/QHask/Sym/PowerSum.hs` - Power sum basis, characters
- `src/QHask/Sym.hs` - Umbrella module
