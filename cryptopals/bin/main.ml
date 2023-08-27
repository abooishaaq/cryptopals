open Cryptopals.Hex_to_bytes
open Cryptopals.Bytes_to_hex
open Cryptopals.Hex_xor
open Cryptopals.Utils

let score s = 
  let sc = ref 0.0 in
  for i = 0 to String.length s - 1 do
    let c = Char.code s.[i] in
    if c > 127 then
      sc := !sc -. 5.0
    else if c < 32 && not (c = 10 || c = 13) then
      sc := !sc -. 7.0
    else if is_alpha c then
      sc := !sc +. 12.0
  done;
  !sc /. float_of_int (String.length s)
;;

let challenge3 =
  let hex = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" in
  let l = String.length hex / 2 in
  let rec repeat str times aux = 
    if times = 0 then aux
    else repeat str (times - 1) (str ^ aux) in
  let rec aux i acc =
    if i = 256 then acc
    else
      let byte = Char.chr i in
      let byte_hexed = repeat (String.of_bytes (bytes_to_hex (Bytes.make 1 byte))) l "" in
      let xor = hex_xor hex byte_hexed in
      let res = hex_to_bytes (String.of_bytes xor) in
      let score = score (Bytes.to_string res) in
      aux (i + 1) ((score, res) :: acc);
  in
  aux 0 [] |> List.sort (fun (a, _) (b, _) -> compare b a) |> List.hd |> snd |> Bytes.to_string |> print_endline
;;

let challenge4 = 
  let lines = read_file "data/4.txt" in
  (* select the string with the most english letters and then brute force xor it *)
  let best = List.fold_left (fun acc line -> 
      let score = score (Bytes.to_string (hex_to_bytes line)) in
      if score > fst acc then (score, line) else acc
    ) (0.0, "") lines in
  let hex = snd best in
  let l = String.length hex / 2 in
  let rec repeat str times aux =
    if times = 0 then aux
    else repeat str (times - 1) (str ^ aux) in
  let rec aux i acc =
    if i = 256 then acc
    else
      let byte = Char.chr i in
      let byte_hexed = repeat (String.of_bytes (bytes_to_hex (Bytes.make 1 byte))) l "" in
      let xor = hex_xor hex byte_hexed in
      let res = hex_to_bytes (String.of_bytes xor) in
      let score = score (Bytes.to_string res) in
      aux (i + 1) ((score, res) :: acc);
  in
  aux 0 [] |> List.sort (fun (a, _) (b, _) -> compare b a) |> List.hd |> snd |> Bytes.to_string |> print_string
;;

let challenge5 = 
  let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" in
  let key = "ICE" in
  let rec repeat str times aux =
    if times = 0 then aux
    else repeat str (times - 1) (str ^ aux) in
  let key_hexed = repeat (String.of_bytes (bytes_to_hex (Bytes.of_string key))) (String.length input / String.length key + 1) "" in
  let xor = hex_xor (String.of_bytes (bytes_to_hex (Bytes.of_string input))) key_hexed in
  print_endline (String.of_bytes xor)
;;

challenge3;;
challenge4;;
challenge5;;

