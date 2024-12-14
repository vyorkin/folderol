open Pretty_print
module List = Core.List

type connective = Conj | Disj | Impl | Iff | Not
[@@deriving eq, show { with_path = false }]

let pp_connective fmt = function
  | Conj -> Format.fprintf fmt "∧"
  | Disj -> Format.fprintf fmt "∨"
  | Impl -> Format.fprintf fmt "→"
  | Iff -> Format.fprintf fmt "↔"
  | Not -> Format.fprintf fmt "¬"

type quantifier = Forall | Exists [@@deriving eq, show { with_path = false }]

let pp_quantifier fmt = function
  | Forall -> Format.fprintf fmt "∀"
  | Exists -> Format.fprintf fmt "∃"

type t =
  | Pred of string * Term.t list
  | Conn of connective * t list
  | Quant of quantifier * string * t
[@@deriving eq, show { with_path = false }]

let is_pred = function Pred _ -> true | _ -> false

let rec pp_formula fmt = function
  | Pred (name, terms) ->
      Format.fprintf fmt "%s(%a)" name
        (Format.pp_print_list ~pp_sep:pp_comma Term.pp_term)
        terms
  | Conn (connective, subformulas) -> (
      match connective with
      | Not -> pp_not fmt (List.hd_exn subformulas)
      | conn ->
          Format.open_vbox 0;
          (Format.pp_print_list
             ~pp_sep:(fun fmt () ->
               Format.pp_print_space fmt ();
               pp_connective fmt conn;
               Format.pp_print_space fmt ())
             pp_formula)
            fmt subformulas;
          Format.close_box ())
  | Quant (quantifier, var, body) ->
      Format.open_hovbox 2;
      Format.fprintf fmt "%a%s." pp_quantifier quantifier var;
      if is_pred body then pp_formula fmt body
      else Format.fprintf fmt "(%a)" (fun fmt body -> pp_formula fmt body) body;
      Format.close_box ()

(* formats a negation of a formula *)
and pp_not fmt subformula =
  pp_connective fmt Not;
  Format.open_box 0;
  pp_formula fmt subformula;
  Format.close_box ()

let formula_to_string = format_to_string pp_formula

let abstract term formula =
  let rec abs ix = function
    | Pred (name, args) ->
        Pred (name, List.map args ~f:(Term.replace (term, Term.Bound ix)))
    | Conn (name, subformulas) -> Conn (name, List.map subformulas ~f:(abs ix))
    | Quant (quantifier, var_name, body) ->
        Quant (quantifier, var_name, abs (ix + 1) body)
  in
  abs 0 formula

let subst_bound_var term formula =
  let rec subst ix = function
    | Pred (name, args) ->
        Pred (name, List.map args ~f:(Term.replace (Term.Bound ix, term)))
    | Conn (name, subformulas) -> Conn (name, List.map subformulas ~f:(subst ix))
    | Quant (quantifier, var_name, body) ->
        Quant (quantifier, var_name, subst (ix + 1) body)
  in
  subst 0 formula

let%test "abstract simple term" =
  let term = Term.Var "x" in
  let formula =
    Pred
      ( "P",
        [ Term.Var "x"; Term.Function ("f", [ Term.Var "y"; Term.Var "x" ]) ] )
  in
  let expected =
    Pred
      ( "P",
        [ Term.Bound 0; Term.Function ("f", [ Term.Var "y"; Term.Bound 0 ]) ] )
  in
  expected = abstract term formula

let%test "abstract with nested formula" =
  let term = Term.Var "x" in
  let formula =
    Conn
      ( Conj,
        [
          Pred ("P", [ Term.Var "x" ]);
          Quant (Forall, "y", Pred ("Q", [ Term.Var "x" ]));
        ] )
  in
  let expected =
    Conn
      ( Conj,
        [
          Pred ("P", [ Term.Bound 0 ]);
          Quant (Forall, "y", Pred ("Q", [ Term.Bound 1 ]));
        ] )
  in
  expected = abstract term formula

let%test "subst_bound_var simple term" =
  let term = Term.Var "z" in
  let formula =
    Pred
      ( "P",
        [ Term.Bound 0; Term.Function ("f", [ Term.Var "y"; Term.Bound 0 ]) ] )
  in
  let expected =
    Pred
      ( "P",
        [ Term.Var "z"; Term.Function ("f", [ Term.Var "y"; Term.Var "z" ]) ] )
  in
  expected = subst_bound_var term formula

let%test "subst_bound_var with nested formula" =
  let term = Term.Var "z" in
  let formula =
    Conn
      ( Disj,
        [
          Pred ("P", [ Term.Bound 0 ]);
          Quant (Exists, "y", Pred ("Q", [ Term.Bound 1 ]));
        ] )
  in
  let expected =
    Conn
      ( Disj,
        [
          Pred ("P", [ Term.Var "z" ]);
          Quant (Exists, "y", Pred ("Q", [ Term.Var "z" ]));
        ] )
  in
  expected = subst_bound_var term formula

let test_pp_formula expected formula =
  let actual = formula_to_string formula in
  if actual <> expected then
    failwith (Printf.sprintf "Expected: %s\nActual: %s" expected actual)

let%test_unit "Pretty-print conjunction" =
  test_pp_formula "P(x) ∧ Q(y)"
    (Conn (Conj, [ Pred ("P", [ Var "x" ]); Pred ("Q", [ Var "y" ]) ]))

let%test_unit "Pretty-print quantified formula" =
  test_pp_formula "∀x.P(x)" (Quant (Forall, "x", Pred ("P", [ Var "x" ])))

let%test_unit "Pretty-print implication" =
  test_pp_formula "P(x) → ∃y.(Q(y) ∨ R(z))"
    (Conn
       ( Impl,
         [
           Pred ("P", [ Var "x" ]);
           Quant
             ( Exists,
               "y",
               Conn (Disj, [ Pred ("Q", [ Var "y" ]); Pred ("R", [ Var "z" ]) ])
             );
         ] ))
