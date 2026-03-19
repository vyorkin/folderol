open Core
open Angstrom

(* helpers *)

let is_whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false
let ws = take_while is_whitespace
let is_newline = function '\n' | '\r' | '\t' -> true | _ -> false

(* actions *)

let load = choice [ string "load"; string "l" ] <* ws
let read = string "read" <* ws
let readn = string "readn" <* ws
let step = choice [ string "step"; string "s" ] <* ws
let stepn = string "stepn" <* ws
let run = choice [ string "run"; string "r" ] <* ws
let apply = string "apply" <* ws
let undo = choice [ string "undo"; string "u" ] <* ws
let cut = string "cut" <* ws
let save = string "save" <* ws
let print = choice [ string "print"; string "p" ] <* ws
let trace = choice [ string "trace"; string "t" ] <* ws
let clear = string "clear" <* ws
let help = choice [ string "help"; string "h" ] <* ws
let quit = choice [ string "quit"; string "q"; string "exit" ] <* ws

(* token *)

let is_token_char c = not @@ is_newline c
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
      (apply *> token >>| fun rule -> Action.Apply rule);
      undo *> return Action.Undo;
      (cut *> token >>| fun formula -> Action.Cut formula);
      (save *> token >>| fun filepath -> Action.Save filepath);
      print *> return Action.Print;
      trace *> return Action.Trace;
      clear *> return Action.Clear;
      help *> return Action.Help;
      quit *> return Action.Quit;
    ]

let parse_line line =
  match String.strip line with
  | "" -> Result.Ok Action.NoOp
  | trimmed -> parse_string ~consume:All parser trimmed
