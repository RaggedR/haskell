# CLAUDE.md

## What This Is
`qhask` — A Haskell library for q-series algebra and partition combinatorics, porting the core of the cylindric partition research (~/git/research) into idiomatic Haskell.

## Build & Test
```bash
source ~/.ghcup/env
cabal build        # compile library
cabal test         # run hspec test suite
cabal repl         # interactive GHCi with library loaded
```

## Architecture
14 modules, no circular dependencies:
- `QHask.QSeries` — Lazy power series as infinite `[Integer]` lists. McIlroy streaming multiplication.
- `QHask.Partition` — Partition type (newtype over `[Int]`), generation, interlacing, conjugation, dominance order.
- `QHask.Profile` — Profile type `(c0,c1,c2)`, mu computation, enumeration.
- `QHask.CPP` — Cylindric plane partition enumeration, GK generating function computation.
- `QHask.Conjecture` — Q polynomial, Borodin product formula, positivity verification.
- `QHask.Sym.Types` — `SymFun b` phantom-typed symmetric functions, `Basis` class, arithmetic.
- `QHask.Sym.Kostka` — SSYT enumeration, Kostka numbers, inverse Kostka matrix.
- `QHask.Sym.LR` — Littlewood-Richardson rule (skew SSYT with lattice word condition).
- `QHask.Sym.Schur` — Schur basis, multiplication via LR rule.
- `QHask.Sym.Monomial` — Monomial basis, conversion via (transpose of inverse) Kostka.
- `QHask.Sym.Homogeneous` — Complete homogeneous basis, direct multiplication.
- `QHask.Sym.Elementary` — Elementary basis via omega involution (conjugate shapes).
- `QHask.Sym.PowerSum` — Power sum basis, Murnaghan-Nakayama characters.
- `QHask.Sym` — Umbrella: `changeBasis`, `omega`, `innerProduct`, re-exports all bases.

## Key Design Choices
- **Lazy lists for power series**: No explicit truncation — just `take n` coefficients as needed.
- **`Integer` for QSeries, `Rational` for Sym**: QSeries uses Integer (inverseQS restricted to leading coeff ±1). Symmetric functions use `Rational` (needed for inverse Kostka and power sum basis).
- **Minimal deps**: Only `base` + `containers`. Tests use `hspec`.
- **GHC2021**: Modern Haskell defaults (DerivingStrategies, etc.)

## Testing
TDD with hspec. Cross-validates against:
- OEIS partition counts (A000041)
- Known q-binomial values
- Rogers-Ramanujan identities for d=2
- Borodin product formula vs direct enumeration
- Warnaar Conjecture 2.7 positivity for d=1,2,4,5
- Known Kostka numbers, K·K^{-1}=I verification for n=3,4
- LR coefficients (Pieri rule, commutativity)
- Character table of S_3 via Murnaghan-Nakayama
- Basis change round-trips (all pairs)
