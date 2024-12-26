let pp_comma ppf () = Format.fprintf ppf ", "

let format_to_string pp v =
  let open Format in
  let buffer = Buffer.create 16 in
  let fmt = formatter_of_buffer buffer in
  pp_open_hvbox fmt 0;
  pp fmt v;
  pp_close_box fmt ();
  pp_print_flush fmt ();
  Buffer.contents buffer
