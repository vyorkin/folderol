open Core

type t = (string, Term.t, String.comparator_witness) Map.t

let empty = Map.empty (module String)
let add env (var, term) = Map.add_exn env ~key:var ~data:term
let find env var = Map.find env var

let pp fmt env =
  let pp_binding fmt (key, term) =
    Format.fprintf fmt "%s -> %s" key (Term.to_string term)
  in
  let bindings = Map.to_alist env in
  Format.fprintf fmt "{%a}"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       pp_binding)
    bindings

let equal = Map.equal Term.equal
