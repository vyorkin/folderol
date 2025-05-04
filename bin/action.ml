open Core
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
  | Quit
[@@deriving show { with_path = false }]

let load_formula filepath =
  let parse channel =
    let lexbuf = Lexing.from_channel channel in
    FolderolParser.main FolderolLexer.read lexbuf
  in
  In_channel.with_file filepath ~f:parse

let load filepath =
  let _formula = load_formula filepath in
  failwith "todo"

let read line = failwith "todo"
let readn lines = failwith "not supported yet"
let step () = failwith "todo"
let stepn n = failwith "todo"
let run () = failwith "todo"
let quit () = exit 0

let run = function
  | NoOp -> ()
  | Load filepath -> load filepath
  | Read line -> read line
  | ReadN lines -> readn lines
  | Step -> step ()
  | StepN n -> stepn n
  | Run -> run ()
  | Quit -> quit ()
