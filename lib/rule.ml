open Pretty_printing

(** Gentzen's LK sequent calculus rule. *)
type t =
  (* Propositional logic rules. *)
  | NotR
  | NotL
  | ConjR
  | ConjL
  | DisjR
  | DisjL
  | ImplR
  | ImplL
  | IffR
  | IffL
  (* Quantifier rules. *)
  | ForallR
  | ForallL
  | ExistsL
  | ExistsR
[@@deriving eq, show { with_path = false }]

let pp fmt rule =
  let open Format in
  match rule with
  | NotR -> fprintf fmt "¬R"
  | NotL -> fprintf fmt "¬L"
  | ConjR -> fprintf fmt "∧R"
  | ConjL -> fprintf fmt "∧L"
  | DisjR -> fprintf fmt "∨R"
  | DisjL -> fprintf fmt "∨L"
  | ImplR -> fprintf fmt "→R"
  | ImplL -> fprintf fmt "→L"
  | IffR -> fprintf fmt "↔R"
  | IffL -> fprintf fmt "↔L"
  | ForallR -> fprintf fmt "∀R"
  | ForallL -> fprintf fmt "∀L"
  | ExistsL -> fprintf fmt "∃L"
  | ExistsR -> fprintf fmt "∃R"

let to_string = format_to_string pp
