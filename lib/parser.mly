%token <int>      IX
%token <string>   STRING "string"

%token LPAREN     "("
%token RPAREN     ")"
%token DOT        "."
%token COMMA      ","
%token QUESTION   "?"

%token TURNSTYLE  "|-"
%token CONJ       "∧" (* "&" *)
%token DISJ       "∨" (* "|" *)
%token IMPL       "→" (* "-->" *)
%token IFF        "↔" (* "<->" *)
%token NOT        "¬" (* "~" *)
%token FORALL     "∀" (* "FORALL" *)
%token EXISTS     "∃" (* "EXISTS" *)
%token EOF

%left IFF
%left IMPL
%left DISJ
%left CONJ
%right NOT

%start <Formula.t> main
%{
  open Formula
  open Term
%}
%%

main:
  | formula EOF { $1 }
  ;

formula:
  | q_formula { $1 }
  | l_formula { $1 }
  ;

q_formula:
  | FORALL STRING DOT formula { Quant(Forall, $2, $4) }
  | EXISTS STRING DOT formula { Quant(Exists, $2, $4) }
  ;

l_formula:
  | l_formula IMPL l_formula  { Conn(Impl, [$1; $3]) }
  | l_formula CONJ l_formula  { Conn(Conj, [$1; $3]) }
  | l_formula DISJ l_formula  { Conn(Disj, [$1; $3]) }
  | l_formula IFF  l_formula  { Conn(Iff, [$1; $3]) }
  | NOT l_formula             { Conn(Not, [$2]) }
  | STRING "(" terms ")"      { Pred($1, $3) }
  | LPAREN formula RPAREN     { $2 }
  ;

terms:
  | term              { [$1] }
  | term COMMA terms  { $1 :: $3 }
  ;

term:
  | STRING                { Var($1) }
  | QUESTION STRING       { Param($2, []) } (* Meta-variable *)
  | IX                    { Bound($1) }
  | STRING "(" terms ")"  { Function($1, $3) }
  ;

