(* import hex_to_bin *)
open Hex_to_bytes
open Bytes_to_hex

let hex_xor a b =
  let a = hex_to_bytes a in
  let b = hex_to_bytes b in
  let l = Bytes.length a in 
  let c = Bytes.create l in
  for i = 0 to (l-1) do
    Bytes.set c i (Char.chr (Char.code (Bytes.get a i) lxor Char.code (Bytes.get b i)))
  done;
  bytes_to_hex c
;;
