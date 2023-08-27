(* allow A-Z, a-z, 0-9 *)
let hex_to_byte b = 
  if b >= 48 && b <= 57 then b - 48
  else if b >= 65 && b <= 70 then b - 55
  else if b >= 97 && b <= 102 then b - 87
  else failwith "invalid hex character"
;;

let hex_to_bytes s =
  let len = String.length s in
  let res = Bytes.create (len / 2) in
  let rec loop i =
    if i < len then
      let b1 = hex_to_byte (Char.code s.[i]) in
      let b2 = hex_to_byte (Char.code s.[i + 1]) in
      Bytes.set res (i / 2) (Char.chr ((b1 lsl 4) lor b2));
      loop (i + 2)
  in
  loop 0;
  res
;;

let%test "hex_to_bytes" = 
  let res = hex_to_bytes "0cff" in
  Bytes.get res 0 = '\x0c' && Bytes.get res 1 = '\xff'
