open OUnit2
open Enigma

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let map_r_to_l_test name wiring top_letter input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l wiring top_letter input_pos)

let map_l_to_r_test name wiring top_letter input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_l_to_r wiring top_letter input_pos)

let identity_wiring = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor_i_wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

let suite =
  "Enigma test suite" >::: [
    index_test "index of A is 0" 'A' 0;
    index_test "index of B is 1" 'B' 1;
    index_test "index of M is 12" 'M' 12;
    index_test "index of R is 17" 'R' 17;
    index_test "index of Z is 25" 'Z' 25;
    map_r_to_l_test "identity wiring, top A, input A" identity_wiring 'A' 0 0;
    map_r_to_l_test "identity wiring, top A, input Z" identity_wiring 'A' 25 25;
    map_r_to_l_test "identity wiring remains identity at top Q" identity_wiring 'Q' 7 7;
    map_r_to_l_test "rotor I at top B maps input A to J" rotor_i_wiring 'B' 0 9;
    map_r_to_l_test "rotor I at top A maps input A to E" rotor_i_wiring 'A' 0 4;
    map_l_to_r_test "identity wiring, top A, input A" identity_wiring 'A' 0 0;
    map_l_to_r_test "identity wiring, top A, input Z" identity_wiring 'A' 25 25;
    map_l_to_r_test "identity wiring remains identity at top Q" identity_wiring 'Q' 7 7;
    map_l_to_r_test "rotor I inverse maps E to A at top A" rotor_i_wiring 'A' 4 0;
    map_l_to_r_test "rotor I inverse maps K to B at top A" rotor_i_wiring 'A' 10 1;
    map_l_to_r_test "rotor I inverse with top B" rotor_i_wiring 'B' 0 21;
  ]

let () = run_test_tt_main suite
