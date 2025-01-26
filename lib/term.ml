open Pretty_printing
module List = Core.List

type t =
  | Var of string
  | Param of string * string list
  | Bound of int
  | Function of string * t list
[@@deriving eq, show { with_path = false }]

let rec variable_names vars term =
  let ins x xs = if List.mem xs x ~equal:String.equal then xs else x :: xs in
  match term with
  | Var name -> ins name vars
  | Function (_, args) -> List.fold ~init:vars ~f:variable_names args
  | _ -> vars

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

let to_string = format_to_string pp_term

let rec replace (old_term, new_term) term =
  if term = old_term then new_term
  else
    match term with
    | Function (name, args) ->
        Function (name, List.map args ~f:(replace (old_term, new_term)))
    | _ -> term
