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
      print_endline formula_str
  | Error err -> print_endline err

let readn lines = failwith "not supported yet"
let step () = failwith "todo"
let stepn n = failwith "todo"
let run () = failwith "todo"

let help () =
  print_endline "Available commands:";
  print_endline "load, l - Load goal from file";
  print_endline "read - Read goal";
  print_endline "readn - Read mutliple comma-separated goals";
  print_endline "step, s - Reduce goal";
  print_endline "stepn - Perform N steps at once";
  print_endline "run, r - Run all steps";
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
  | Help -> help ()
  | Quit -> quit ()
