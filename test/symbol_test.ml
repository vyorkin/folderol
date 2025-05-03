open FolderolLib

let test_mk_symbol () =
  let expected = "a" in
  let s0 = Symbol.mk () in
  Alcotest.(check string) "mk: simple" expected s0

let test_mk_multiple_symbols () =
  let expected = [ "e"; "d"; "c"; "b"; ] in
  let actual = [ Symbol.mk (); Symbol.mk (); Symbol.mk (); Symbol.mk () ] in
  Alcotest.(check (list string)) "mk: multiple" expected actual
