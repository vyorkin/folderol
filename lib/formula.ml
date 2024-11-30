module List = Core.List

type t =
  | Pred of string * Term.t list
  | Conn of string * t list
  | Quant of string * string * t
[@@deriving eq, show { with_path = false }]

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
      ( "&",
        [
          Pred ("P", [ Term.Var "x" ]);
          Quant ("forall", "y", Pred ("Q", [ Term.Var "x" ]));
        ] )
  in
  let expected =
    Conn
      ( "&",
        [
          Pred ("P", [ Term.Bound 0 ]);
          Quant ("forall", "y", Pred ("Q", [ Term.Bound 1 ]));
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
      ( "|",
        [
          Pred ("P", [ Term.Bound 0 ]);
          Quant ("exists", "y", Pred ("Q", [ Term.Bound 1 ]));
        ] )
  in
  let expected =
    Conn
      ( "|",
        [
          Pred ("P", [ Term.Var "z" ]);
          Quant ("exists", "y", Pred ("Q", [ Term.Var "z" ]));
        ] )
  in
  expected = subst_bound_var term formula
