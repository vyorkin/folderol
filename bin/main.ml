open Core
open Cmdliner
open Cmdliner.Term.Syntax

let cmd_repl =
  let doc = "Start REPL" in
  Cmd.(v (info "repl" ~doc))
  @@
  let+ () = Term.const () in
  Repl.(run (prompt_seq ()));
  Cmdliner.Cmd.Exit.ok

let cmd_file =
  let doc = "Use file as REPL input" in
  Cmd.(v (info "file" ~doc))
  @@
  let+ () = Term.const () in
  Repl.(run (file_seq "file.flr"));
  Cmdliner.Cmd.Exit.ok

let cmd_main =
  let doc = "Toy theorem prover" in
  Cmd.(group (info "folderol" ~version:"0.1.0-dev" ~doc))
  @@ [ cmd_repl; cmd_file ]

let main () = Cmd.eval' cmd_main
let () = if !Sys.interactive then () else exit (main ())
