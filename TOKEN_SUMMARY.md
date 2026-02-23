# Token Usage Summary: Symmetric Functions Port

## Project
Port of Sage-combinat's symmetric function machinery to Haskell (`QHask.Sym.*`).
9 new source modules, 9 new test modules, 2 existing files modified.

## Date
2026-02-22

## Model
Claude Opus 4.6 (claude-opus-4-6)

## Estimated Token Usage

| Phase | Input Tokens (est.) | Output Tokens (est.) | Notes |
|-------|--------------------:|---------------------:|-------|
| Planning & exploration | 200K | 15K | Reading existing code, designing architecture |
| Phase 1: Foundation (Partition, Types, Kostka, LR, Schur) | 800K | 60K | Sequential, growing context |
| Phase 2: Basis modules (Mono, Homo, Elem, PowerSum) | 1,200K | 50K | Sequential (should have been parallel sub-agents) |
| Phase 3: Umbrella, cabal, tests, debugging | 1,500K | 40K | Context near limit, many fix cycles |
| Bug fixes (border strips rewrite, index ordering) | 800K | 30K | Most wasteful phase |
| **Total (estimated)** | **~4,500K** | **~195K** | |

## Estimated Cost
- Input: 4.5M tokens × $15/M = **$67.50**
- Output: 195K tokens × $75/M = **$14.63**
- **Total: ~$82**

## What Was Produced
- 14 library modules (5 existing + 9 new), ~1,200 lines of Haskell
- 14 test modules (5 existing + 9 new), ~800 lines of tests
- 173 passing tests, 0 warnings
- Full change-of-basis between all 5 classical symmetric function bases
- Cross-validated against known Kostka numbers, LR coefficients, character tables

## Efficiency Analysis

### What went well
- TDD caught bugs early (wrong test expectations found quickly)
- Plan mode gave a clear roadmap before any code was written
- No wasted modules — everything that was written is used

### What was wasteful
1. **No sub-agents for Phase 2**: Monomial, Homogeneous, Elementary, and PowerSum are independent once Schur exists. Writing them as 4 parallel sub-agents would have saved ~1M input tokens (~$15).
2. **Border strips algorithm written twice**: First version was fundamentally wrong (computed strip sizes as entire rows). Complete rewrite needed. Cost: ~500K extra input tokens (~$7.50).
3. **No context clearing**: After Phase 1, should have checkpointed and suggested `/clear`. Would have reduced average context size by ~40%.
4. **Sequential file writing**: Even test files could have been written by sub-agents in parallel.

### Recommendations for future large ports
- **Parallelize with sub-agents** whenever dependency graph allows
- **Checkpoint and clear** after completing each major phase
- **Write algorithm tests first**, then implement — catches formula bugs before they cascade
- **Budget: ~$50-60** for a similar 9-module port with these optimizations applied
