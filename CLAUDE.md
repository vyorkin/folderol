# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Folderol is a toy theorem prover in OCaml implementing Gentzen's LK sequent calculus, based on Lawrence C. Paulson's "Designing a Theorem Prover" paper. It performs automated proof search via backward reasoning.

## Build Commands

```bash
dune build                        # Build
dune exec folderol -- repl        # Run interactive REPL
dune exec folderol -- file        # Run from file input
dune runtest                      # Run all tests (Alcotest)
```

To run a single test by name, use Alcotest's `--filter` flag via:
```bash
dune exec -- test/testsuite.exe test --filter "test name"
```

## Setup

```bash
opam switch create .
opam install ocaml-lsp-server odoc ocamlformat utop
```

## Architecture

The codebase splits into three layers:

**Core library (`lib/`)** — the theorem prover engine:
- `term.ml` / `formula.ml` — data types for terms (meta-variables, parameters, bound variables, function applications) and formulas (predicates, connectives ∧∨→↔¬, quantifiers ∀∃)
- `goal.ml`, `goal_entry.ml`, `goal_table.ml` — sequent representation (Γ ⊢ Δ) with cost-based ordering to guide proof search
- `env.ml` — variable-to-term binding environments
- `unification.ml` / `instantiation.ml` — core algorithms for matching and substituting formulas
- `rule.ml` — the 14 LK sequent calculus rules (propositional: ¬R/L, ∧R/L, ∨R/L, →R/L, ↔R/L; quantifier: ∀R/L, ∃L/R)
- `proof.ml` — proof search engine that applies rules via backward reasoning
- `lexer.mll` / `parser.mly` — OCamllex lexer and Menhir parser for formula syntax

**CLI (`bin/`)** — user-facing interface:
- `main.ml` — Cmdliner-based CLI with `repl` and `file` subcommands
- `repl.ml` — interactive proof exploration loop
- `parser.ml` / `action.ml` — command parsing and action dispatch

**Tests (`test/`)** — Alcotest suite with one test module per library module, aggregated in `testsuite.ml`.

## Key Dependencies

- `menhir` (=20240715) — parser generator for `parser.mly`
- `ppx_let` / `ppx_deriving` — monadic let bindings and deriving (show, eq)
- `cmdliner` — CLI framework
- `angstrom` — parser combinators
- `core` — Jane Street standard library
- `alcotest` — test framework

## Formatting

Uses `ocamlformat` v0.27.0 with the conventional profile and 80-char margin.
