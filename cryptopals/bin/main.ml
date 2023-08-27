open Cryptopals.Hex_to_base64
open Cryptopals.Bytes_to_base64

let () =
  let hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" in
  let base64 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" in
  let result = hex_to_base64 hex in
  if result = base64 then
    Printf.printf "OK\n"
  else
    Printf.printf "KO\n"
    ;
  let res1 = Base64.encode_string "any pleasure." in
  let res2 = bytes_to_base64 (Bytes.of_string "any pleasure.") in
  print_endline res1;
  print_endline res2;
;;
