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
  | Undo
  | Cut of string
  | Save of string
  | Apply of string
  | Clear
  | Print
  | Trace
  | Help
  | Quit
[@@deriving show { with_path = false }]

(* Undo history stack *)
let history : Goal_table.t Stack.t = Stack.create ()

let save_state () =
  Proof.with_goal_table (fun table -> Stack.push history table)

let load_formula filepath =
  let parse channel =
    let lexbuf = Lexing.from_channel channel in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
    try Ok (FolderolParser.main FolderolLexer.read lexbuf) with
    | Lexer.LexingError msg -> Error ("Lexing error: " ^ msg)
    | FolderolParser.Error ->
        let pos = lexbuf.lex_curr_p in
        let line = pos.pos_lnum in
        let col = pos.pos_cnum - pos.pos_bol in
        Error
          (Printf.sprintf
             "Parse error in %s at line %d, column %d: unexpected token '%s'"
             filepath line col (Lexing.lexeme lexbuf))
  in
  In_channel.with_file filepath ~f:parse

let load_sequent filepath =
  let parse channel =
    let lexbuf = Lexing.from_channel channel in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
    try Ok (FolderolParser.sequent FolderolLexer.read lexbuf)
    with Lexer.LexingError _ | FolderolParser.Error | Parsing.Parse_error ->
      Error "not a sequent"
  in
  In_channel.with_file filepath ~f:parse

let load filepath =
  match load_sequent filepath with
  | Ok (gamma, delta) ->
      Stack.clear history;
      Proof.init (Goal_table.mk (gamma, delta));
      let table_str = Goal_table.to_string (Goal_table.mk (gamma, delta)) in
      print_endline table_str
  | Error _ -> (
      match load_formula filepath with
      | Ok formula ->
          Stack.clear history;
          Proof.init (Goal_table.mk ([], [ formula ]));
          print_endline (Formula.to_string formula)
      | Error err -> print_endline err)

let parse_formula s =
  let lexbuf = Lexing.from_string s in
  try Ok (FolderolParser.main FolderolLexer.read lexbuf) with
  | Lexer.LexingError msg -> Error ("Lexing error: " ^ msg)
  | FolderolParser.Error ->
      let pos = lexbuf.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      Error
        (Printf.sprintf "Parse error at position %d: unexpected token '%s'" col
           (Lexing.lexeme lexbuf))
  | Parsing.Parse_error -> Error "Parsing error"

let parse_sequent s =
  let lexbuf = Lexing.from_string s in
  try Ok (FolderolParser.sequent FolderolLexer.read lexbuf)
  with Lexer.LexingError _ | FolderolParser.Error | Parsing.Parse_error ->
    Error "not a sequent"

let read line =
  let result =
    match parse_sequent line with
    | Ok (gamma, delta) -> Ok (gamma, delta)
    | Error _ -> (
        match parse_formula line with
        | Ok formula -> Ok ([], [ formula ])
        | Error err -> Error err)
  in
  match result with
  | Ok (gamma, delta) ->
      Stack.clear history;
      Proof.init (Goal_table.mk (gamma, delta));
      let table_str = Goal_table.to_string (Goal_table.mk (gamma, delta)) in
      print_endline table_str
  | Error err -> print_endline err

let readn lines =
  match lines |> List.map ~f:parse_formula |> Core.Result.all with
  | Ok formulas ->
      let goal_table = Goal_table.mk ([], formulas) in
      Stack.clear history;
      Proof.init goal_table;
      List.iter formulas ~f:(fun f -> print_endline (Formula.to_string f))
  | Error err -> print_endline err

let step () =
  save_state ();
  match Proof.step () with
  | Ok table -> print_endline (Goal_table.to_string table)
  | Error err ->
      ignore (Stack.pop history);
      print_endline err

let stepn n =
  save_state ();
  match Proof.steps n with
  | Ok table -> print_endline (Goal_table.to_string table)
  | Error err ->
      ignore (Stack.pop history);
      print_endline err

let run_proof () =
  save_state ();
  match Proof.run () with
  | Ok table ->
      if Goal_table.is_empty table then print_endline "Proof complete!"
      else (
        print_endline
          (Printf.sprintf "Search stopped after %d steps. Remaining goals:"
             Proof.default_depth_limit);
        print_endline (Goal_table.to_string table))
  | Error err ->
      ignore (Stack.pop history);
      print_endline err

let apply_cut formula_str =
  match parse_formula formula_str with
  | Ok cut_formula -> (
      save_state ();
      let result =
        Proof.with_goal_table (fun table ->
            match table with
            | [] -> Error "No goals to cut"
            | (goal_entry :: goal) :: rest ->
                let open Core.Result.Let_syntax in
                let%bind rule, subgoals =
                  Goal.cut (goal_entry :: goal) cut_formula
                in
                let formulas, table' =
                  Goal_table.insert_goals rest (subgoals, [])
                in
                Proof.print_step rule goal_entry formulas;
                Proof.init table';
                Ok table'
            | [] :: _ -> Error "Empty goal")
      in
      match result with
      | Ok table -> print_endline (Goal_table.to_string table)
      | Error err ->
          ignore (Stack.pop history);
          print_endline err)
  | Error err -> print_endline err

let undo () =
  match Stack.pop history with
  | Some table ->
      Proof.init table;
      print_endline "Undone.";
      print_endline (Goal_table.to_string table)
  | None -> print_endline "Nothing to undo"

let save_proof filepath =
  Proof.with_goal_table (fun table ->
      let content = Goal_table.to_string table in
      Out_channel.write_all filepath ~data:(content ^ "\n");
      print_endline (Printf.sprintf "Proof state saved to %s" filepath))

let apply_tactic rule_str =
  match Rule.of_string rule_str with
  | None -> print_endline (Printf.sprintf "Unknown rule: %s" rule_str)
  | Some target_rule -> (
      save_state ();
      match
        Proof.with_goal_table (fun table -> Proof.apply_rule target_rule table)
      with
      | Ok table ->
          Proof.init table;
          print_endline (Goal_table.to_string table)
      | Error err ->
          ignore (Stack.pop history);
          print_endline err)

let print () = Proof.print_goal_table ()
let trace () = Proof.print_proof_trace ()

let clear () =
  Stack.clear history;
  Proof.clear ();
  print_endline "Goal table cleared"

let help () =
  print_endline "Available commands:";
  print_endline "  load, l <file> - Load goal from file";
  print_endline "  read <formula> - Read goal";
  print_endline "  readn <f1> <f2> ... - Read multiple goals";
  print_endline "  step, s        - Reduce goal (one step)";
  print_endline "  stepn <n>      - Perform N steps at once";
  print_endline "  run, r         - Run proof search";
  print_endline "  apply <rule>   - Apply specific rule (e.g., apply ConjR)";
  print_endline "  undo, u        - Undo last step";
  print_endline "  cut <formula>  - Apply cut rule with formula";
  print_endline "  save <file>    - Save proof state to file";
  print_endline "  print, p       - Print goal table";
  print_endline "  trace, t       - Show proof trace";
  print_endline "  clear          - Clear all goals";
  print_endline "  help, h        - Show this help text";
  print_endline "  quit, q, exit  - Exit REPL"

let quit () = exit 0

let run = function
  | NoOp -> ()
  | Load filepath -> load filepath
  | Read line -> read line
  | ReadN lines -> readn lines
  | Step -> step ()
  | StepN n -> stepn n
  | Run -> run_proof ()
  | Undo -> undo ()
  | Apply rule_str -> apply_tactic rule_str
  | Cut formula_str -> apply_cut formula_str
  | Save filepath -> save_proof filepath
  | Print -> print ()
  | Trace -> trace ()
  | Clear -> clear ()
  | Help -> help ()
  | Quit -> quit ()
