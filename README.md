# Folderol

A toy theorem prover in OCaml implementing Gentzen's LK sequent calculus, based on Lawrence C. Paulson's "Designing a Theorem Prover" paper. Performs automated proof search via backward reasoning with backtracking.

## Blog posts

- <https://vyorkin.org/posts/designing-a-theorem-prover-part-1/>
- <https://vyorkin.org/posts/designing-a-theorem-prover-part-2/>
- <https://vyorkin.org/posts/forwards-reasoning-vs-backwards-reasoning/>
- <https://vyorkin.org/posts/designing-a-theorem-prover-part-3/>
- <https://vyorkin.org/posts/designing-a-theorem-prover-part-4/>
- <https://vyorkin.org/posts/designing-a-theorem-prover-part-5/>
- <https://vyorkin.org/posts/designing-a-theorem-prover-part-6/>
- <https://vyorkin.org/posts/designing-a-theorem-prover-part-7/>
- <https://vyorkin.org/posts/designing-a-theorem-prover-part-8/>
- <https://vyorkin.org/posts/designing-a-theorem-prover-part-9/>

## Setup

```sh
opam switch create .
opam install ocaml-lsp-server odoc ocamlformat utop
```

## Build

```sh
dune build                           # Build
dune runtest                         # Run all tests
dune exec folderol -- repl           # Start interactive REPL
dune exec folderol -- file input.txt # Run commands from file
```

Run a single test by name:

```sh
dune exec -- test/testsuite.exe test --filter "test name"
```

## Formula syntax

### Propositional connectives

| Syntax          | Meaning        |
|-----------------|----------------|
| `P & Q` or `P ∧ Q` | conjunction |
| `P \| Q` or `P ∨ Q` | disjunction |
| `P --> Q` or `P → Q` | implication |
| `P <-> Q` or `P ↔ Q` | biconditional |
| `~P` or `¬P`   | negation       |

### Quantifiers

| Syntax                     | Meaning     |
|----------------------------|-------------|
| `FORALL x. P(x)` or `∀x. P(x)` | universal |
| `EXISTS x. P(x)` or `∃x. P(x)` | existential |

### Operator precedence (loosest to tightest)

`↔` > `→` > `∨` > `∧` > `¬`

All binary operators are right-associative.

### Terms

| Syntax      | Meaning               |
|-------------|-----------------------|
| `x`         | variable              |
| `?a`        | meta-variable         |
| `f(x, y)`   | function application  |
| `f(g(x))`   | nested functions      |

### Sequents

A sequent `Γ |- Δ` asserts that the conjunction of `Γ` proves the disjunction of `Δ`.

```
|- P --> Q          # prove a formula (empty antecedent)
P |- Q              # prove Q from assumption P
P, Q |- R, S        # prove R or S from P and Q
```

Comments: `#` or `--` start a line comment.

## REPL

```sh
dune exec folderol -- repl
```

### Commands

| Command              | Alias | Description                              |
|----------------------|-------|------------------------------------------|
| `read <formula>`     |       | Load a formula as goal `|- formula`      |
| `readn <f1> <f2> …`  |       | Load multiple formulas as `|- f1, f2, …` |
| `load <file>`        | `l`   | Load goal from file                      |
| `step`               | `s`   | Apply one proof rule                     |
| `stepn <N>`          |       | Apply N proof rules                      |
| `run`                | `r`   | Run full proof search (depth limit 100)  |
| `apply <rule>`       |       | Apply a specific rule manually           |
| `cut <formula>`      |       | Apply the cut rule                       |
| `undo`               | `u`   | Undo the last step                       |
| `print`              | `p`   | Print current goal table                 |
| `trace`              | `t`   | Show proof trace                         |
| `save <file>`        |       | Save current proof state to file         |
| `clear`              |       | Clear all goals                          |
| `help`               | `h`   | Show help                                |
| `quit` / `exit`      | `q`   | Exit                                     |

### Rule names

Rules can be given in Unicode or ASCII when using `apply`:

| Rule   | Unicode | ASCII     |
|--------|---------|-----------|
| ¬R/L   | `¬R`, `¬L` | `NotR`, `NotL` |
| ∧R/L   | `∧R`, `∧L` | `ConjR`, `ConjL` |
| ∨R/L   | `∨R`, `∨L` | `DisjR`, `DisjL` |
| →R/L   | `→R`, `→L` | `ImplR`, `ImplL` |
| ↔R/L   | `↔R`, `↔L` | `IffR`, `IffL` |
| ∀R/L   | `∀R`, `∀L` | `ForallR`, `ForallL` |
| ∃R/L   | `∃R`, `∃L` | `ExistsR`, `ExistsL` |

## Examples

### Distributivity (step by step)

```
Γ|-∆> read ((P | Q) & (P | R)) --> (P | (Q & R))
P ∨ Q ∧ P ∨ R → P ∨ Q ∧ R
Γ|-∆> step
[→R]
|- (cost=1) P ∨ Q ∧ P ∨ R → P ∨ Q ∧ R
0: P ∨ Q, P ∨ R |- P ∨ Q ∧ R
Γ|-∆> step
[∧L]
(cost=1) P ∨ Q ∧ P ∨ R |-
0: P ∨ Q, P ∨ R |- P ∨ Q ∧ R
Γ|-∆> step
[∨R]
|- (cost=1) P ∨ Q ∧ R
0: P ∨ Q, P ∨ R |- Q ∧ R, P
Γ|-∆> step
[∧R]
|- (cost=2) Q ∧ R
0: P ∨ Q, P ∨ R |- R, P
1: P ∨ Q, P ∨ R |- Q, P
Γ|-∆> step
[∨L]
(cost=2) P ∨ Q |-:
P
0: P ∨ R, Q |- R, P
1: P ∨ Q, P ∨ R |- Q, P
Γ|-∆> step
[∨L]
(cost=2) P ∨ R |-:
R, P
0: P ∨ Q, P ∨ R |- Q, P
Γ|-∆> step
[∨L]
(cost=2) P ∨ Q |-:
Q, P
∅
```

### Automated proof search

```
Γ|-∆> read P --> ~~P
P → ¬¬P
Γ|-∆> run
[→R]
[¬R]
[¬L]
∅
```

### Contrapositive

```
Γ|-∆> read (P --> Q) --> (~Q --> ~P)
Γ|-∆> run
∅
```

### Excluded middle

```
Γ|-∆> read P | ~P
Γ|-∆> run
∅
```

### De Morgan

```
Γ|-∆> read ~(P & Q) --> (~P | ~Q)
Γ|-∆> run
∅
```

### Sequent with assumptions

```
Γ|-∆> read (P <-> Q) <-> R |- P <-> (Q <-> R)
Γ|-∆> run
∅
```

### Proof trace

After a proof, inspect the steps taken:

```
Γ|-∆> read P --> ~~P
Γ|-∆> run
∅
Γ|-∆> trace
Step 1: [→R]  |- P → ¬¬P
Step 2: [¬R]  P |- ¬¬P
Step 3: [¬L]  P |- P  (axiom)
```

### Manual rule application

```
Γ|-∆> read P & Q --> P
Γ|-∆> apply →R
Γ|-∆> apply ∧L
Γ|-∆> step
∅
```

### Cut rule

```
Γ|-∆> read P --> R
Γ|-∆> cut P --> Q
# Splits into: |- P → Q  and  P → Q |- R
```

## Architecture

```
folderol/
├── lib/           # Core theorem prover engine
│   ├── term.ml(i)           # Terms: variables, parameters, functions
│   ├── formula.ml(i)        # Formulas: predicates, connectives, quantifiers
│   ├── goal.ml(i)           # Sequents Γ ⊢ Δ with cost-based ordering
│   ├── goal_entry.ml(i)     # Sequent entries (cost × side × formula)
│   ├── goal_table.ml(i)     # Proof state: list of open goals
│   ├── env.ml(i)            # Variable-to-term binding environment
│   ├── unification.ml(i)    # Unification with occurs check
│   ├── instantiation.ml(i)  # Substitution of environments into formulas
│   ├── rule.ml              # 14 LK rules (propositional + quantifier)
│   ├── proof.ml(i)          # Proof search engine with backtracking
│   ├── proof_tree.ml(i)     # Proof tree data structure
│   ├── symbol.ml(i)         # Fresh symbol generator
│   ├── lexer.mll            # OCamllex lexer
│   └── parser.mly           # Menhir parser
├── bin/           # User-facing CLI
│   ├── main.ml              # Cmdliner entry point (repl / file subcommands)
│   ├── repl.ml              # Interactive REPL loop
│   ├── action.ml            # Command dispatch + undo history
│   └── parser.ml            # REPL command parser (Angstrom)
└── test/          # Alcotest test suite (~100 tests)
```

### Key algorithms

**Backward reasoning** — proof search starts from the goal and applies rules in reverse until all branches close (reach axioms via unification).

**Cost-based ordering** — each formula carries a cost that guides rule selection. Quantifier rules (∀L, ∃R) cost 3; propositional rules cost the number of premises. Cheaper goals are reduced first.

**Backtracking** — when unification admits multiple solutions, the search saves choice points and backtracks on failure. Implemented via a choice point stack.

**Lemma cache** — closed goals are cached by their string representation. Identical subgoals encountered later are skipped immediately.

**Meta-variables & parameters** — universally quantified variables become fresh parameters; existentials introduce meta-variables unified during proof search.

## Dependencies

| Package          | Purpose                          |
|------------------|----------------------------------|
| `menhir`         | Parser generator for `parser.mly` |
| `ppx_let`        | Monadic let bindings             |
| `ppx_deriving`   | Deriving `show` and `eq`         |
| `cmdliner`       | CLI framework                    |
| `angstrom`       | REPL command parser combinators  |
| `core`           | Jane Street standard library     |
| `alcotest`       | Test framework                   |
