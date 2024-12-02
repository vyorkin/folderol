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

%start <Formula.t> main
%%

main:
    | i = IX { Formula.Pred ("&", [Term.Var "x"; Term.Var "y"; Bound i ]) }

