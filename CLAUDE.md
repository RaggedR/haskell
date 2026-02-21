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
5 modules, no circular dependencies:
- `QHask.QSeries` — Lazy power series as infinite `[Integer]` lists. McIlroy streaming multiplication.
- `QHask.Partition` — Partition type (newtype over `[Int]`), generation, interlacing, conjugation.
- `QHask.Profile` — Profile type `(c0,c1,c2)`, mu computation, enumeration.
- `QHask.CPP` — Cylindric plane partition enumeration, GK generating function computation.
- `QHask.Conjecture` — Q polynomial, Borodin product formula, positivity verification.

## Key Design Choices
- **Lazy lists for power series**: No explicit truncation — just `take n` coefficients as needed.
- **`Integer` not `Rational`**: `inverseQS` restricted to leading coeff ±1 (covers all our use cases).
- **Minimal deps**: Only `base` + `containers`. Tests use `hspec`.
- **GHC2021**: Modern Haskell defaults (DerivingStrategies, etc.)

## Testing
TDD with hspec. Cross-validates against:
- OEIS partition counts (A000041)
- Known q-binomial values
- Rogers-Ramanujan identities for d=2
- Borodin product formula vs direct enumeration
- Warnaar Conjecture 2.7 positivity for d=1,2,4,5
