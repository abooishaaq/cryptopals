open Cryptopals.Hex_to_bytes
open Cryptopals.Bytes_to_hex
open Cryptopals.Base64_to_bytes
open Cryptopals.Hamming_distance
open Cryptopals.Bytes_xor
open Cryptopals.Utils

let asciifreq c =
  match c with
  | 'a' -> 0.0651738
  | 'b' -> 0.0124248
  | 'c' -> 0.0217339
  | 'd' -> 0.0349835
  | 'e' -> 0.1041442
  | 'f' -> 0.0197881
  | 'g' -> 0.0158610
  | 'h' -> 0.0492888
  | 'i' -> 0.0558094
  | 'j' -> 0.0009033
  | 'k' -> 0.0050529
  | 'l' -> 0.0331490
  | 'm' -> 0.0202124
  | 'n' -> 0.0564513
  | 'o' -> 0.0596302
  | 'p' -> 0.0137645
  | 'q' -> 0.0008606
  | 'r' -> 0.0497563
  | 's' -> 0.0515760
  | 't' -> 0.0729357
  | 'u' -> 0.0225134
  | 'v' -> 0.0082903
  | 'w' -> 0.0171272
  | 'x' -> 0.0013692
  | 'y' -> 0.0145984
  | 'z' -> 0.0007836
  | ' ' -> 0.1918182
  | _ -> 0.0


let score s = 
  let sc = ref 0.0 in
  for i = 0 to String.length s - 1 do
    let c = Char.code s.[i] in
    if c > 127 then
      sc := !sc -. 1.0
    else if c < 32 && not (c = 10 || c = 13) then
      sc := !sc -. 1.0
    else
      sc := !sc +. asciifreq s.[i]
  done;
  !sc /. float_of_int (String.length s)
;;

let rec repeat str times aux =
  if times = 0 then aux
  else repeat str (times - 1) (str ^ aux)
;;

let challenge3 =
  let hex = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" in
  let enc = hex_to_bytes hex in
  let l = String.length hex / 2 in
  let rec aux i =
    if i = 256 then []
    else
      let byte = Char.chr i in
      let keyy = Bytes.make l byte in
      let xor = bytes_xor enc keyy in
      let res = String.of_bytes xor in
      let score = score res in
      (score, res) :: aux (i + 1);
  in
  aux 0 |> List.sort (fun (a, _) (b, _) -> compare b a) |> List.hd |> snd |> print_endline
;;

let challenge4 = 
  let lines = read_file "data/4.txt" in
  (* select the string with the most english letters and then brute force xor it *)
  let best = List.fold_left (fun acc line -> 
      let score = score (Bytes.to_string (hex_to_bytes line)) in
      if score > fst acc then (score, line) else acc
    ) (0.0, "") lines in
  let hex = snd best in
  let enc = hex_to_bytes hex in
  let l = Bytes.length enc in
  let rec aux i =
    if i = 256 then []
    else
      let byte = Char.chr i in
      let keyy = Bytes.make l byte in
      let xor = bytes_xor enc keyy in
      let score = score (Bytes.to_string xor) in
      (score, xor) :: aux (i + 1);
  in
  aux 0 |> List.sort (fun (a, _) (b, _) -> compare b a) |> List.hd |> snd |> Bytes.to_string |> print_endline
;;

let challenge5 = 
  let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" in
  let key = "ICE" in
  let key = Bytes.of_string key in
  let times = (String.length input / Bytes.length key) + 1 in
  let xor = bytes_xor (Bytes.of_string input) (repeat_bytes key times) in
  print_endline (bytes_to_hex xor |> Bytes.to_string)
;;

let challenge6 = 
  let input = read_file "data/6.txt" |> List.fold_left (fun acc line -> acc ^ (base64_to_bytes line)) "" |> Bytes.of_string in
  let transpose_bytes_list bytes_list =
    let array_of_array_of_bytes = Array.of_list (List.map (fun bytes -> bytes_to_char_list bytes |> Array.of_list) bytes_list) in
    let res = Array.make (Bytes.length (List.hd bytes_list)) [] in
    for i = 0 to Array.length array_of_array_of_bytes - 1 do
      for j = 0 to Array.length array_of_array_of_bytes.(i) - 1 do
        res.(j) <- array_of_array_of_bytes.(i).(j) :: res.(j)
      done
    done;
    res |> Array.map (fun l -> List.rev l |> Array.of_list)
  in
    (* brute force to find KEYSIZE *)
  let rec aux keysize =
    let block1 = Bytes.sub input 0 keysize in
    let block2 = Bytes.sub input keysize keysize in
    let block3 = Bytes.sub input (keysize * 2) keysize in
    let block4 = Bytes.sub input (keysize * 3) keysize in
    let dist1 = hamming_distance block1 block2 in
    let dist2 = hamming_distance block2 block3 in
    let dist3 = hamming_distance block3 block4 in
    let dist4 = hamming_distance block1 block3 in
    let dist5 = hamming_distance block1 block4 in
    let dist6 = hamming_distance block2 block4 in
    let dist = (dist1 + dist2 + dist3 + dist4 + dist5 + dist6) / 6 in
    if keysize > 50 then []
    else
      (float_of_int dist /. float_of_int keysize, keysize) ::  aux (keysize + 1)
  in
  let keysizes = aux 2 |> List.sort (fun (a, _) (b, _) -> compare a b) in
  let keysize = List.hd keysizes |> snd in
  let blocks =  split (bytes_to_char_list input) keysize |> List.map char_list_to_bytes in
  let blocks = transpose_bytes_list blocks in
  let key = ref (Bytes.make keysize ' ') in
  for i = 0 to Array.length blocks - 1 do
    let block = Array.to_list blocks.(i) |> char_list_to_bytes in
    let len = Array.length blocks.(i) in
    let rec aux j sc =
      if j = 256 then ()
      else
        let byte = Char.chr j in
        let charkey = Bytes.make len byte in
        let xor = bytes_xor block charkey in
        let res = Bytes.to_string xor in
        let score = score res in
        if score > sc then
          begin
            Bytes.set !key i byte;
            aux (j + 1) score
          end
        else
          aux (j + 1) sc
    in
    aux 0 0.0
  done;
  let key = Bytes.to_string !key in
  let key_hexed = repeat (String.of_bytes (bytes_to_hex (Bytes.of_string key))) (Bytes.length input / String.length key + 1) "" in
  let xor = bytes_xor input (hex_to_bytes key_hexed) in
  print_endline (Bytes.to_string xor)
;;

challenge3;;
challenge4;;
challenge5;;
challenge6;;
