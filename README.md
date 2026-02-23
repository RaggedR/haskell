# qhask

A Haskell library for **q-series algebra**, **partition combinatorics**, and **symmetric functions**.

qhask implements lazy formal power series with McIlroy streaming multiplication, integer partition generation with interlacing constraints, and a complete symmetric function library spanning all five classical bases. It grew out of research on cylindric partitions and Warnaar's conjectures, ported from Python into idiomatic Haskell.

## Quick start

```bash
cabal build        # compile
cabal test         # run 173 tests
cabal repl         # interactive GHCi with library loaded
```

Requires GHC 9.6+ (GHC2021 language). Only depends on `base` and `containers`.

## Example session

### q-series

```haskell
λ> import QHask.QSeries

-- Euler's partition generating function: 1 / (q;q)_inf
λ> let partGF = inverseQS (qFactorial 50)
λ> takeQS 12 partGF
[1,1,2,3,5,7,11,15,22,30,42,56]

-- Gaussian q-binomial [4 choose 2]_q
λ> takeQS 6 (qBinomial 4 2)
[1,1,2,2,2,1]
```

### Partitions

```haskell
λ> import QHask.Partition

λ> partitionsOf 4
[Partition [4], Partition [3,1], Partition [2,2], Partition [2,1,1], Partition [1,1,1,1]]

λ> conjugate (Partition [4,2,1])
Partition [3,2,1,1]
```

### Symmetric functions

```haskell
λ> import QHask.Sym

-- Schur function multiplication via Littlewood-Richardson
λ> showSym "s" $ mulS (s (Partition [2,1])) (s (Partition [2,1]))
"s_(3,2,1) + s_(3,1,1,1) + s_(2,2,2) + s_(4,1,1) + 2*s_(2,2,1,1) + s_(4,2)"

-- Change basis: Schur to monomial
λ> showSym "m" $ (changeBasis (s (Partition [2,1])) :: SymFun Mono)
"m_(2,1) + 2*m_(1,1,1)"

-- Hall inner product
λ> innerProduct (s (Partition [2,1])) (s (Partition [2,1]))
1 % 1
```

## Modules

### q-series & partitions

| Module | Description |
|--------|-------------|
| `QHask.QSeries` | Lazy power series as infinite `[Integer]` lists; McIlroy streaming multiplication |
| `QHask.Partition` | Partition type, generation, conjugation, interlacing, dominance order |
| `QHask.Profile` | Profile type `(c0,c1,c2)`, mu computation, enumeration |
| `QHask.CPP` | Cylindric plane partition enumeration, GK generating functions |
| `QHask.Conjecture` | Q polynomial, Borodin product formula, positivity verification |

### Symmetric functions

| Module | Description |
|--------|-------------|
| `QHask.Sym` | Umbrella module: `changeBasis`, `omega`, `innerProduct`, re-exports all bases |
| `QHask.Sym.Types` | `SymFun b` phantom-typed symmetric functions, `Basis` class, arithmetic |
| `QHask.Sym.Schur` | Schur basis `s`, multiplication via Littlewood-Richardson rule |
| `QHask.Sym.Monomial` | Monomial basis `m`, conversion via inverse Kostka matrix |
| `QHask.Sym.Homogeneous` | Complete homogeneous basis `h`, direct merge-based multiplication |
| `QHask.Sym.Elementary` | Elementary basis `e`, via omega involution on conjugate shapes |
| `QHask.Sym.PowerSum` | Power sum basis `p`, Murnaghan-Nakayama characters |
| `QHask.Sym.Kostka` | SSYT enumeration, Kostka numbers, inverse Kostka matrix |
| `QHask.Sym.LR` | Littlewood-Richardson rule (skew SSYT with lattice word condition) |

## Design choices

- **Lazy lists for power series** -- no explicit truncation; just `take n` coefficients as needed. Infinite products compose naturally.
- **`Integer` for q-series, `Rational` for symmetric functions** -- q-series stay exact in the integers (inverse requires leading coeff &pm;1). Symmetric functions need `Rational` for inverse Kostka and power sum conversions.
- **Phantom-typed bases** -- `SymFun b` uses phantom types (`Mono`, `Elem`, `Homo`, `Pow`, `Schur`) so the type system prevents accidentally mixing bases. All conversions route through the Schur basis via a `Basis` typeclass.
- **Schur as the hub** -- every basis converts to/from Schur. Basis changes compose as `fromSchur . toSchur`. Multiplication in any basis routes through Schur + LR rule.
- **Minimal dependencies** -- only `base` + `containers`. Tests use `hspec`.

## Testing

173 tests cross-validate against known mathematical results:

- OEIS partition counts (A000041)
- Known q-binomial values
- Rogers-Ramanujan identities (d=2)
- Borodin product formula vs direct enumeration
- Warnaar Conjecture 2.7 positivity (d=1,2,4,5)
- Kostka numbers, K &middot; K<sup>-1</sup> = I verification
- LR coefficients (Pieri rule, commutativity)
- Character table of S<sub>3</sub> via Murnaghan-Nakayama
- Basis change round-trips (all 20 pairs)

```bash
cabal test
```

## License

MIT
