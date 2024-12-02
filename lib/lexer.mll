{
  open Lexing
  open Parser

  exception LexingError of string
}

(* Regular expressions *)

let whitespaces = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let uint = digit+
let string = ['a'-'z' 'A'-'Z']+

(* Lexing rules *)

rule read = parse
  | whitespaces { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }

  | "(" { LPAREN }
  | ")" { RPAREN }
  | "." { DOT }
  | "," { COMMA }
  | "?" { QUESTION }

  | "|-" { TURNSTYLE }
  | "∧" | "&" { CONJ }
  | "∨" | "|" { DISJ }
  | "→" | "-->" { IMPL }
  | "↔" | "<->" { IFF }
  | "¬" | "~" { NOT }
  | "∀" | "FORALL" { FORALL }
  | "∃" | "EXISTS" { EXISTS }

  | uint as ix { IX(int_of_string ix) }
  | string as str { STRING(str) }

  | _   { raise (LexingError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

