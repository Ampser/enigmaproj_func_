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

let map_plug_test name plugs input expected =
  name >:: fun _ ->
    assert_equal expected (map_plug plugs input)

let cipher_char_test name config input expected =
  name >:: fun _ ->
    assert_equal expected (cipher_char config input)

let identity_wiring = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor_i_wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_ii_wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotor_iii_wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let reflector_b_wiring = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let rotor_i = { wiring = rotor_i_wiring; turnover = 'Q' }
let rotor_ii = { wiring = rotor_ii_wiring; turnover = 'E' }
let rotor_iii = { wiring = rotor_iii_wiring; turnover = 'V' }

let identity_config = {
  refl = identity_wiring;
  rotors = [];
  plugboard = [];
}

let no_rotors_config = {
  refl = reflector_b_wiring;
  rotors = [];
  plugboard = [];
}

let aaa_config = {
  refl = reflector_b_wiring;
  rotors = [
    { rotor = rotor_i; top_letter = 'A' };
    { rotor = rotor_ii; top_letter = 'A' };
    { rotor = rotor_iii; top_letter = 'A' };
  ];
  plugboard = [];
}

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
    map_plug_test "empty plugboard leaves character unchanged" [] 'M' 'M';
    map_plug_test "plugboard maps first direction of pair" [('A', 'B')] 'A' 'B';
    map_plug_test "plugboard maps reverse direction of pair" [('A', 'B')] 'B' 'A';
    map_plug_test "plugboard supports multiple pairs" [('A', 'B'); ('C', 'D')] 'C' 'D';
    map_plug_test "unplugged character remains unchanged" [('A', 'B'); ('C', 'D')] 'Z' 'Z';
    cipher_char_test "identity configuration keeps A" identity_config 'A' 'A';
    cipher_char_test "configuration with no rotors uses reflector" no_rotors_config 'A' 'Y';
    cipher_char_test "historical I-II-III at AAA maps G to P" aaa_config 'G' 'P';
    cipher_char_test "historical I-II-III at AAA maps A to U" aaa_config 'A' 'U';
    cipher_char_test "historical I-II-III at AAA maps Z to H" aaa_config 'Z' 'H';
  ]

let () = run_test_tt_main suite
