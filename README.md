# Folderol

My little theorem prover. Based on the Designing a Theorem Prover paper by Lawrence C. Paulson.

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

## REPL

```sh
dune exec folderol -- repl
```

```sh

Γ|-∆> read ((P | Q) & (P | R)) --> (P | (Q & R))
P ∨ Q ∧ P ∨ R → P ∨ Q ∧ R
Γ|-∆> step
[→R]
|- (cost=1) P ∨ Q ∧ P ∨ R → P ∨ Q ∧ R
0: P ∨ Q ∧ P ∨ R |- P ∨ Q ∧ R
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
