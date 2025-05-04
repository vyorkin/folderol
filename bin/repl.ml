open Core

let print_error () =
  Out_channel.eprintf "Error:\n%!";
  Out_channel.flush stderr

let prompt = "folderol> "

let prompt_repl () =
  Out_channel.printf "%s%!" prompt;
  match In_channel.input_line In_channel.stdin with
  | Some line -> Some (line, ())
  | None -> None

let file_repl channel =
  match In_channel.input_line channel with
  | Some line -> Some (line, channel)
  | None ->
      In_channel.close channel;
      None

let prompt_seq () = Sequence.unfold ~init:() ~f:prompt_repl

let file_seq filename =
  let channel =
    try In_channel.create filename
    with Sys_error _ ->
      print_error ();
      exit 1
  in
  Sequence.unfold ~init:channel ~f:file_repl

let process_line line =
  match Parser.parse_line line with
  | Error _ -> print_error ()
  | Ok action -> Action.run action

let run = Sequence.iter ~f:process_line
