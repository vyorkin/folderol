open Core
open Angstrom

(* helpers *)

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let ws = take_while is_whitespace

(* actions *)

let load = string "load" <* ws
let read = string "read" <* ws
let readn = string "readn" <* ws
let step = string "step" <* ws
let stepn = string "stepn" <* ws
let run = string "run" <* ws
let quit = string "quote" <* ws

(* token *)

let is_token_char c = not @@ is_whitespace c
let token = take_while1 is_token_char <* ws

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
      quit *> return Action.Quit;
    ]

let parse_line line =
  match String.strip line with
  | "" -> Result.Ok Action.NoOp
  | trimmed -> parse_string ~consume:All parser trimmed
