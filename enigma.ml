type rotor = {
  wiring : string;
  turnover : char;
}

type oriented_rotor = {
  rotor : rotor;
  top_letter : char;
}

type config = {
  refl : string;
  rotors : oriented_rotor list;
  plugboard : (char * char) list;
}

let index c =
  Char.code c - Char.code 'A'

let map_r_to_l wiring top_letter input_pos =
  let modulo_26 value = ((value mod 26) + 26) mod 26 in
  let offset = index top_letter in
  let internal_pos = modulo_26 (input_pos + offset) in
  let wired_pos = index wiring.[internal_pos] in
  modulo_26 (wired_pos - offset)

let modulo_26 value = ((value mod 26) + 26) mod 26

let position_of_char wiring target =
  let rec search position =
    if position = String.length wiring then
      failwith "Character not found in rotor wiring"
    else if wiring.[position] = target then
      position
    else
      search (position + 1)
  in
  search 0

let map_l_to_r wiring top_letter input_pos =
  let offset = index top_letter in
  let internal_pos = modulo_26 (input_pos + offset) in
  let target = Char.chr (Char.code 'A' + internal_pos) in
  let wired_pos = position_of_char wiring target in
  modulo_26 (wired_pos - offset)

let map_refl wiring input_pos =
  index wiring.[input_pos]

let rec map_plug plugs c =
  match plugs with
  | [] -> c
  | (left, right) :: rest ->
      if c = left then right
      else if c = right then left
      else map_plug rest c

let cipher_char config c =
  let rec through_rotors_right_to_left rotors input_pos =
    match rotors with
    | [] -> input_pos
    | oriented_rotor :: rest ->
        let output_pos = through_rotors_right_to_left rest input_pos in
        map_r_to_l oriented_rotor.rotor.wiring oriented_rotor.top_letter output_pos
  in
  let rec through_rotors_left_to_right rotors input_pos =
    match rotors with
    | [] -> input_pos
    | oriented_rotor :: rest ->
        let output_pos =
          map_l_to_r oriented_rotor.rotor.wiring oriented_rotor.top_letter input_pos
        in
        through_rotors_left_to_right rest output_pos
  in
  let plugged_input = map_plug config.plugboard c in
  let right_output =
    through_rotors_right_to_left config.rotors (index plugged_input)
  in
  let reflected_output = map_refl config.refl right_output in
  let left_output =
    through_rotors_left_to_right config.rotors reflected_output
  in
  let unboarded_output = Char.chr (Char.code 'A' + left_output) in
  map_plug config.plugboard unboarded_output

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0
