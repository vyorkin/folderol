module List = Core.List

type t =
  | Var of string
  | Param of string * string list
  | Bound of int
  | Function of string * t list
[@@deriving eq, show { with_path = false }]

let rec replace (old_term, new_term) term =
  if term = old_term then new_term
  else
    match term with
    | Function (name, args) ->
        Function (name, List.map args ~f:(replace (old_term, new_term)))
    | _ -> term

let%test "replace in a simple term" =
  let term = Var "x" in
  let result = replace (Var "x", Bound 42) term in
  result = Bound 42

let%test "replace in a nested function" =
  let term = Function ("f", [ Var "x"; Bound 1; Var "x" ]) in
  let result = replace (Var "x", Bound 42) term in
  let expected = Function ("f", [ Bound 42; Bound 1; Bound 42 ]) in
  result = expected

let%test "replace does not modify unmatched terms" =
  let term = Function ("f", [ Param ("y", [ "a"; "b" ]); Bound 1 ]) in
  let result = replace (Var "x", Bound 42) term in
  result = term

let%test "replace works with deeply nested functions" =
  let term =
    Function ("f", [ Function ("g", [ Var "x"; Bound 2 ]); Var "x" ])
  in
  let result = replace (Var "x", Bound 99) term in
  let expected =
    Function ("f", [ Function ("g", [ Bound 99; Bound 2 ]); Bound 99 ])
  in
  result = expected
