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

let map_refl_test name wiring input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_refl wiring input_pos)

let identity_wiring = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor_i_wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let reflector_b_wiring = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

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
    map_refl_test "identity reflector maps A to A" identity_wiring 0 0;
    map_refl_test "identity reflector maps N to N" identity_wiring 13 13;
    map_refl_test "identity reflector maps Z to Z" identity_wiring 25 25;
    map_refl_test "historical reflector B maps A to Y" reflector_b_wiring 0 24;
    map_refl_test "historical reflector B maps B to R" reflector_b_wiring 1 17;
    map_refl_test "historical reflector B maps Y to A" reflector_b_wiring 24 0;
    map_refl_test "historical reflector B maps Z to T" reflector_b_wiring 25 19;
  ]

let () = run_test_tt_main suite
