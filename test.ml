open OUnit2

(** Test suite of scrabble test*)
let suite =
  "test suite of all Scrabble Tests"
  >::: List.flatten [ (* Insert test suite names here *)]

(** Runs the test suite on run *)
let _ = run_test_tt_main suite