open Pretty_print
module List = Core.List

type t =
  | Var of string
  | Param of string * string list
  | Bound of int
  | Function of string * t list
[@@deriving eq, show { with_path = false }]

let rec pp_term fmt = function
  | Var x -> Format.fprintf fmt "%s" x
  | Param (name, vars) ->
      Format.fprintf fmt "%s(%a)" name
        (fun fmt vars ->
          Format.open_hvbox 0;
          Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string fmt vars;
          Format.close_box ())
        vars
  | Bound ix -> Format.fprintf fmt "%d" ix
  | Function (name, args) ->
      Format.fprintf fmt "%s(%a)" name
        (fun fmt args ->
          Format.open_hvbox 2;
          Format.pp_print_list ~pp_sep:pp_comma pp_term fmt args;
          Format.close_box ())
        args

let term_to_string = format_to_string pp_term

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

(* Custom assertion for tests *)
let test_pp_term ~expected term =
  let actual = term_to_string term in
  if actual <> expected then
    failwith (Printf.sprintf "Expected: %s\nActual: %s" expected actual)

let%test_unit "Pretty-print variable" = test_pp_term ~expected:"x" (Var "x")

let%test_unit "Pretty-print parameter" =
  test_pp_term ~expected:"f(x, y, z)" (Param ("f", [ "x"; "y"; "z" ]))

let%test_unit "Pretty-print bound variable" =
  test_pp_term ~expected:"42" (Bound 42)

let%test_unit "Pretty-print function with no arguments" =
  test_pp_term ~expected:"g()" (Function ("g", []))

let%test_unit "Pretty-print function with arguments" =
  test_pp_term ~expected:"h(x, f(a, b))"
    (Function ("h", [ Var "x"; Function ("f", [ Var "a"; Var "b" ]) ]))

let%test_unit "Pretty-print nested functions" =
  test_pp_term ~expected:"k(h(f(a), g(b, c)), x)"
    (Function
       ( "k",
         [
           Function
             ( "h",
               [
                 Function ("f", [ Var "a" ]);
                 Function ("g", [ Var "b"; Var "c" ]);
               ] );
           Var "x";
         ] ))
