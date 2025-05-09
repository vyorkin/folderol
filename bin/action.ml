open Core
open FolderolLib
module FolderolLexer = FolderolLib.Lexer
module FolderolParser = FolderolLib.Parser

type t =
  | NoOp
  | Load of string
  | Read of string
  | ReadN of string list
  | Step
  | StepN of int
  | Run
  | Clear
  | Print
  | Help
  | Quit
[@@deriving show { with_path = false }]

let load_formula filepath =
  let parse channel =
    let lexbuf = Lexing.from_channel channel in
    FolderolParser.main FolderolLexer.read lexbuf
  in
  In_channel.with_file filepath ~f:parse

let load filepath =
  let formula = load_formula filepath in
  let formula_str = Formula.to_string formula in
  print_endline formula_str

let parse_formula s =
  let lexbuf = Lexing.from_string s in
  try Ok (FolderolParser.main FolderolLexer.read lexbuf) with
  | Lexer.LexingError msg -> Error ("Lexing error: " ^ msg)
  | Parsing.Parse_error -> Error "Parsing error"

let read line =
  (* print_endline @@ Printf.sprintf ":: read(%s)" line; *)
  match parse_formula line with
  | Ok formula ->
      let formula_str = Formula.to_string formula in
      Proof.init (Goal_table.mk ([], [ formula ]));
      print_endline formula_str
  | Error err -> print_endline err

let readn lines = failwith "not supported yet"

let step () =
  match Proof.step () with
  | Ok table -> print_endline (Goal_table.to_string table)
  | Error err -> print_endline err

let stepn n =
  match Proof.steps n with
  | Ok table -> print_endline (Goal_table.to_string table)
  | Error err -> print_endline err

let run () = stepn Int.max_value
let print () = Proof.print_goal_table ()

let clear () =
  Proof.clear ();
  print_endline "Goal table cleared"

let help () =
  print_endline "Available commands:";
  print_endline "load, l - Load goal from file";
  print_endline "read - Read goal";
  print_endline "readn - Read mutliple comma-separated goals";
  print_endline "step, s - Reduce goal";
  print_endline "stepn - Perform N steps at once";
  print_endline "run, r - Run all steps";
  print_endline "print, p - Print goal table";
  print_endline "clear - Clear all goals";
  print_endline "help, h - Show this help text";
  print_endline "quit, q, exit - Exit REPL"

let quit () = exit 0

let run = function
  | NoOp -> ()
  | Load filepath -> load filepath
  | Read line -> read line
  | ReadN lines -> readn lines
  | Step -> step ()
  | StepN n -> stepn n
  | Run -> run ()
  | Print -> print ()
  | Clear -> clear ()
  | Help -> help ()
  | Quit -> quit ()
