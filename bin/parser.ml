open Core
open Angstrom

(* helpers *)

let is_whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false
let ws = take_while is_whitespace
let is_newline = function '\n' | '\r' | '\t' -> true | _ -> false

(* token: everything up to newline *)

let is_token_char c = not @@ is_newline c
let token = take_while1 is_token_char <* ws

(* command word: letters, digits, hyphens *)

let is_cmd_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let command_word = take_while1 is_cmd_char <* ws

(* number *)

let digits = take_while1 (function '0' .. '9' -> true | _ -> false)

(* Dispatch on the command word to avoid ambiguity between single-letter
   shortcuts and full command names (e.g. "s" for step vs "save"). *)

let parser =
  command_word >>= fun cmd ->
  match cmd with
  | "load" | "l" -> token >>| fun filepath -> Action.Load filepath
  | "read" -> token >>| fun line -> Action.Read line
  | "readn" -> many token >>| fun lines -> Action.ReadN lines
  | "step" | "s" -> return Action.Step
  | "stepn" -> token >>| fun n -> Action.StepN (Core.Int.of_string n)
  | "run" | "r" ->
      choice
        [
          (digits >>| fun n -> Action.RunN (Core.Int.of_string n));
          return Action.Run;
        ]
  | "apply" -> token >>| fun rule -> Action.Apply rule
  | "undo" | "u" -> return Action.Undo
  | "cut" -> token >>| fun formula -> Action.Cut formula
  | "save" ->
      choice
        [
          ( string "--latex" *> ws *> token >>| fun filepath ->
            Action.SaveLatex filepath );
          (token >>| fun filepath -> Action.Save filepath);
        ]
  | "print" | "p" -> return Action.Print
  | "tree" -> return Action.Tree
  | "trace" | "t" -> return Action.Trace
  | "quit" | "q" | "exit" -> return Action.Quit
  | "explain" | "e" -> return Action.Explain
  | "hint" -> return Action.Hint
  | "clear" -> return Action.Clear
  | "help" | "h" -> return Action.Help
  | unknown -> fail (Printf.sprintf "Unknown command: '%s'" unknown)

let parse_line line =
  match String.strip line with
  | "" -> Result.Ok Action.NoOp
  | trimmed
    when String.is_prefix trimmed ~prefix:"#"
         || String.is_prefix trimmed ~prefix:"--" ->
      Result.Ok Action.NoOp
  | trimmed -> (
      match parse_string ~consume:All parser trimmed with
      | Ok _ as ok -> ok
      | Error _ ->
          Error
            (Printf.sprintf
               "Unknown command: '%s'. Type 'help' for available commands."
               (if String.length trimmed > 40 then
                  String.prefix trimmed 40 ^ "..."
                else trimmed)))
