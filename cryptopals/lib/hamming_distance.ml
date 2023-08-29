let bytes_to_bits bytes =
  let bits = ref [] in
  for i = 0 to Bytes.length bytes - 1 do
    let byte = Bytes.get bytes i in
    for j = 0 to 7 do
      bits := (Char.code (byte) land (1 lsl j) <> 0) :: !bits
    done
  done;
  List.rev !bits
;;

let hamming_distance b1 b2 =
  let bits1 = bytes_to_bits b1 in
  let bits2 = bytes_to_bits b2 in
  List.fold_left2 (fun acc b1 b2 -> if b1 <> b2 then acc + 1 else acc) 0 bits1 bits2
;;


let%test "hamming_distance" =
  hamming_distance (Bytes.of_string "this is a test") (Bytes.of_string "wokka wokka!!!") = 37
