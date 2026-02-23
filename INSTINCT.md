# INSTINCT.md — Multi-Agent Research Architecture

> A system where a main Claude instance directs specialist sub-agents
> that have their own tools, compressed context, and progressive disclosure.

## The Problem

Mathematical research requires three capabilities simultaneously:
1. **Reasoning** — conjectures, proof strategies, connecting ideas across domains
2. **Computation** — enumerating partitions, computing q-series, verifying identities
3. **Literature** — knowing what's been proved, finding relevant techniques, citing correctly

No single context window can hold all three at once. Papers alone can be 50+ pages each,
and a literature review might span 200 papers. Computation results can be huge.
But the *reasoning* that ties it all together is compact.

## The Architecture

```
┌─────────────────────────────────────────────────────────┐
│                     MAIN CLAUDE                         │
│                                                         │
│  Role: Research director. Mathematical reasoning.       │
│  Context: Current conjecture, proof strategy,           │
│           compact summaries from sub-agents.            │
│                                                         │
│  Does NOT: read raw papers, run computations,           │
│            hold large datasets in context.              │
│                                                         │
│  Spawns sub-agents with COMPRESSED CONTEXT:             │
│  a 3-5 sentence summary of what it's working on,       │
│  plus a specific question or task.                      │
├──────────────┬──────────────────────────┬───────────────┤
│              │                          │               │
▼              ▼                          ▼               ▼
┌──────────┐  ┌──────────────────┐  ┌──────────┐  ┌───────────┐
│  QHask   │  │   Literature     │  │  LaTeX   │  │  Future   │
│  Agent   │  │   Agent          │  │  Agent   │  │  agents   │
│          │  │                  │  │          │  │           │
│ Haskell  │  │ Vector DBs +    │  │ Compile, │  │ Sage,     │
│ REPL,    │  │ knowledge       │  │ diagrams,│  │ GAP,      │
│ qhask    │  │ graphs,         │  │ format   │  │ Magma,    │
│ library  │  │ hierarchical    │  │          │  │ ...       │
│          │  │ summaries       │  │          │  │           │
└──────────┘  └──────────────────┘  └──────────┘  └───────────┘
```

## Compressed Context Protocol

When Main Claude spawns a sub-agent, it sends:

```
CONTEXT (3-5 sentences):
  "We are investigating Warnaar's Conjecture 2.7 on positivity of Q_{n,c}(q)
  for rank-3 cylindric partitions. We have proved the cyclic descent injection
  for d not divisible by 3. Currently exploring whether L-positivity of the
  frequency generating function can be lifted to a multivariate Q_n."

TASK:
  "Compute Q_{n,(2,1,1)}(q) for n = 1..4 and check if the coefficients
  match any known combinatorial sequence in OEIS."

RETURN FORMAT:
  "List Q_n polynomials. Flag any OEIS matches. 5 sentences max."
```

The sub-agent works autonomously, using its own tools, and returns a compact result.
Main Claude never sees the intermediate computation or the 50 papers searched —
only the distilled answer.

## Sub-Agent 1: QHask Computation Agent

### What it has
- **Skill**: Execute Haskell code via `cabal repl` or compiled CLI
- **Library**: qhask (QSeries, Partition, Profile, CPP, Conjecture modules)
- **Progressive disclosure**: Reads function catalog first, loads module source only if needed

### Tool Catalog (what the agent sees first)
```
QHask.QSeries    — Lazy power series algebra (add, mul, inverse, qBinomial, qPochhammer)
QHask.Partition  — Partition generation, interlacing, conjugation
QHask.Profile    — Cylindric partition profiles (c0,c1,c2)
QHask.CPP        — Enumerate cylindric plane partitions, compute GK
QHask.Conjecture — Compute Q_n polynomials, Borodin product, verify positivity
```

### Progressive disclosure levels
1. **Catalog** (always loaded): One line per module, as above
2. **Signatures** (loaded on demand): Exported function types + one-line Haddock
3. **Source** (loaded only when debugging): Full implementation

### How it works
1. Receives compressed context + computation task
2. Reads the tool catalog (Level 1)
3. Determines which modules are needed
4. Writes a Haskell expression or short script
5. Executes via `cabal repl` or `runghc`
6. Returns formatted results to Main Claude

### Future: Sage-combinat port
As we port symmetric function machinery to Haskell, the catalog grows:
```
QHask.Symmetric.Schur     — Schur functions, Kostka numbers, LR coefficients
QHask.Symmetric.PowerSum  — Power sum basis, plethysm
QHask.Symmetric.Monomial  — Monomial symmetric functions
QHask.Symmetric.Macdonald — Macdonald polynomials, qt-Kostka
QHask.Symmetric.Bases     — Change-of-basis machinery, transition matrices
QHask.QSym                — Quasi-symmetric functions, M and L bases
```

The type system enforces basis membership: you can't accidentally add a Schur
function to a power sum polynomial without an explicit basis change.

## Sub-Agent 2: Literature Agent

### The Hierarchical Knowledge System

Raw papers are too large for context. We build a recursive compression tree.

```
LEVEL 0: Raw Papers
  └── 50 papers per thematic vector database
      Themes: "cylindric partitions", "q-series identities",
              "quasi-symmetric functions", "crystal bases", ...

LEVEL 1: Paper Summaries
  └── For each theme:
      1. Build knowledge graph over the ~50 papers
         (nodes = key concepts/results, edges = logical dependencies)
      2. For each node in the graph:
         - Query the vector DB with the node's concept
         - Retrieve relevant passages from all 50 papers
         - Generate a 2-3 page LaTeX summary
         - Include full citations to original papers
      Result: ~20-30 summaries per theme, each 2-3 pages

LEVEL 2: Meta-Summaries
  └── Take ALL Level 1 summaries (across all themes)
      1. Put them into a new vector database
      2. Build a knowledge graph over the summaries
         (nodes = research areas, edges = connections between areas)
      3. For each node:
         - Query the Level 1 vector DB
         - Generate a 2-3 page summary of summaries
         - Include references to Level 1 summaries AND original papers
      Result: ~10-15 meta-summaries covering the entire research landscape
```

### How the Literature Agent works

1. Receives compressed context + literature question from Main Claude
2. **First**: Searches Level 2 meta-summaries (fastest, broadest)
   - "Is there anything about crystal bases and cylindric partitions?"
   - Finds relevant meta-summary nodes
3. **If needed**: Drills down to Level 1 summaries
   - "What exactly did Lam prove about affine Stanley symmetric functions?"
   - Retrieves the specific 2-3 page summary with citations
4. **If needed**: Drills down to Level 0 raw papers
   - "What is the exact statement of Theorem 3.2 in Borodin (2006)?"
   - Retrieves the specific passage from the vector DB
5. Returns a compact answer with citations to Main Claude

### Key property: The agent saves context at every level

Because each level is a self-contained vector DB + knowledge graph, the agent
can start at the right level of detail. Most questions are answered at Level 2
(meta-summaries). Only specific technical questions require drilling to Level 0.

### Building the hierarchy

The hierarchy is built offline (not during a research session):

```bash
# Step 1: Organize papers into thematic directories
papers/
  cylindric-partitions/    # ~50 PDFs
  q-series-identities/     # ~50 PDFs
  quasi-symmetric/         # ~50 PDFs
  crystal-bases/           # ~50 PDFs

# Step 2: For each theme, build vector DB + knowledge graph
/lit-review papers/cylindric-partitions "cylindric partitions"
/knowledge-graph papers/cylindric-partitions

# Step 3: Generate Level 1 summaries (for each node in each graph)
# This is a new skill: /summarize-nodes <theme-dir>

# Step 4: Collect all Level 1 summaries, build Level 2
# /meta-review summaries/level-1/

# Step 5: Generate Level 2 meta-summaries
# /summarize-nodes summaries/level-1/
```

## Agent Communication Protocol

### Spawn format
```json
{
  "agent": "qhask | literature | latex",
  "context": "3-5 sentence compressed context from Main Claude",
  "task": "Specific question or computation",
  "return_format": "How to format the response (max length, structure)",
  "urgency": "blocking | background"
}
```

### Return format
```json
{
  "result": "The compact answer",
  "confidence": "high | medium | low",
  "details_available": true,
  "follow_up": "Suggested next computation or literature search"
}
```

### Blocking vs Background
- **Blocking**: Main Claude waits for the result (e.g., "compute Q_3 before I can reason further")
- **Background**: Main Claude continues reasoning while the agent works
  (e.g., "search for related results while I work on the proof")

## Why This Architecture

1. **Context efficiency**: Main Claude's context holds reasoning, not data.
   A 200-paper literature review compresses to a 5-sentence answer.

2. **Specialization**: Each agent is an expert with the right tools.
   The QHask agent knows Haskell. The Literature agent knows the paper hierarchy.
   Main Claude knows mathematics.

3. **Progressive disclosure everywhere**: Every level loads the minimum needed.
   Catalog → signatures → source. Meta-summaries → summaries → raw papers.

4. **Composability**: New agents plug in without changing the architecture.
   Sage agent, GAP agent, proof assistant agent — same spawn/return protocol.

5. **Persistence**: The literature hierarchy and knowledge graphs persist across sessions.
   Every future Claude instance benefits from the work already done.

## Status

- [x] QHask library: 14 modules (incl. symmetric functions), 67+ tests, shipped
- [ ] QHask CLI executable (Main.hs with command interface)
- [ ] QHask agent skill definition
- [x] Literature hierarchy builder (Level 0 → Level 1 → Level 2)
  - Level 0: 76 papers indexed, 4794 chunks, ChromaDB + knowledge graph (559 concepts, 486 edges)
  - Level 1: 555 concept summaries generated via parallel RAG+LLM pipeline (~/data/arxiv-rag/compressed/)
  - Level 2: 10 meta-summaries covering entire research landscape (~/data/arxiv-rag/meta-summaries/)
  - Level 2 knowledge graph + interactive visualization built
- [x] Literature agent query tool (literature_agent.py — progressive Level 2→1→0 drill-down)
- [x] Literature agent Claude Code skill definition (`/literature` command → literature_agent.py)
- [x] Sage-combinat port (symmetric functions in Haskell) — all 5 classical bases shipped
- [x] Meta-review pipeline (generate_summaries.py + build_level2.py)
