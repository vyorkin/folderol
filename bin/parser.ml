open Core
open Angstrom

(* helpers *)

let is_newline = function '\n' | '\r' | '\t' -> true | _ -> false
let newline = take_while is_newline

(* actions *)

let load = choice [ string "load"; string "l" ] <* newline
let read = string "read" <* newline
let readn = string "readn" <* newline
let step = string "step" <* newline
let stepn = string "stepn" <* newline
let run = choice [ string "run"; string "r" ] <* newline
let help = choice [ string "help"; string "h" ] <* newline
let quit = choice [ string "quit"; string "q"; string "exit" ] <* newline

(* token *)

let is_token_char c = not @@ is_newline c
let token = take_while1 is_token_char <* newline

(* parsing *)

let parser =
  choice
    [
      (load *> token >>| fun filepath -> Action.Load filepath);
      (read *> token >>| fun line -> Action.Read line);
      (readn *> many token >>| fun lines -> Action.ReadN lines);
      step *> return Action.Step;
      (stepn *> token >>| fun n -> Action.StepN (Core.Int.of_string n));
      run *> return Action.Run;
      help *> return Action.Help;
      quit *> return Action.Quit;
    ]

let parse_line line =
  match String.strip line with
  | "" -> Result.Ok Action.NoOp
  | trimmed -> parse_string ~consume:All parser trimmed
