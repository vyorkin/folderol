open Core

let print_error s =
  Out_channel.eprintf "Error:\n%s\n" s;
  Out_channel.flush stderr

let prompt = "Γ|-∆> "

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
    with Sys_error e ->
      print_error e;
      exit 1
  in
  Sequence.unfold ~init:channel ~f:file_repl

let process_line line =
  match Parser.parse_line line with
  | Error e -> print_error e
  | Ok action -> Action.run action

let run = Sequence.iter ~f:process_line
