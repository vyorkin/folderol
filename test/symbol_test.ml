open FolderolLib

let symbol_testable = Symbol.(Alcotest.testable pp equal)

let test_mk_symbol () =
  let expected = Symbol.of_string "a" in
  let s0 = Symbol.mk () in
  Alcotest.(check symbol_testable) "mk: simple" expected s0

let test_mk_multiple_symbols () =
  let expected =
    [
      Symbol.of_string "e";
      Symbol.of_string "d";
      Symbol.of_string "c";
      Symbol.of_string "b";
    ]
  in
  let actual = [ Symbol.mk (); Symbol.mk (); Symbol.mk (); Symbol.mk () ] in
  Alcotest.(check (list symbol_testable)) "mk: multiple" expected actual
