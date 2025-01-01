open Pretty_printing
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

let to_string = format_to_string pp_formula

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
