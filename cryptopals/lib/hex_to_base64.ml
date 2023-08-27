open Hex_to_bytes
open Bytes_to_base64

let hex_to_base64 s =
  s |> hex_to_bytes |> bytes_to_base64

(* let%test  "hex to base64" = let res = (hex_to_base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") in 
  print_string res ;
  print_string "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" ;

;; *)
