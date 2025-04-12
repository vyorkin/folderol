open FolderolLib

let test_accumulate_sum_numbers () =
  let init = 10 in
  let xs = ([ 1; 2; 3; 4; 5 ], init) in
  let add (x, y) = x + y in
  let actual = Util.accumulate add xs in
  let expected = init + 15 in
  Alcotest.(check int) "accumulate: sum numbers" expected actual

