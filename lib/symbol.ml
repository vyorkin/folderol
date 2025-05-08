let id = ref (-1)

let next_id () =
  incr id;
  !id

let reset () = id := -1
let mk_char_string n = Char.code 'a' + n |> Char.chr |> String.make 1

let rec mk_varname (n, tail) =
  if n < 26 then mk_char_string n ^ tail
  else mk_varname (n / 26, mk_char_string (Int.rem n 26) ^ tail)

let mk () =
  let id = next_id () in
  mk_varname (id, "")
