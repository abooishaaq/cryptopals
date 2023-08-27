let base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";;

(* replace 'A's in the end which don't belong there to = *)
let padd len b64 =
  let left = len mod 3 in
  if left = 0 then b64
  else if left = 1 then String.sub b64 0 (String.length b64 - 2) ^ "=="
  else String.sub b64 0 (String.length b64 - 1) ^ "="

let bytes_to_base64 bytes =
  let len = Bytes.length bytes in
  let rec aux i acc =
    if i >= len then acc
    else
      let b1 = Bytes.get bytes i in
      let b2 = if i + 1 < len then Bytes.get bytes (i + 1) else '\x00' in
      let b3 = if i + 2 < len then Bytes.get bytes (i + 2) else '\x00' in
      let b1 = int_of_char b1 in
      let b2 = int_of_char b2 in
      let b3 = int_of_char b3 in
      let c1 = b1 lsr 2 in
      let c2 = ((b1 land 0x03) lsl 4) lor (b2 lsr 4) in
      let c3 = ((b2 land 0x0F) lsl 2) lor (b3 lsr 6) in
      let c4 = b3 land 0x3F in
      let c1 = String.get base64 c1 in
      let c2 = String.get base64 c2 in
      let c3 = String.get base64 c3 in
      let c4 = String.get base64 c4 in
      let acc = String.concat String.empty [acc; (String.make 1 c1); (String.make 1 c2); (String.make 1 c3); (String.make 1 c4)] in
      aux (i + 3) acc
  in
  padd len (aux 0 String.empty)
;;

let%test "bytes to base64" = 
  let res1 = Base64.encode_string "any pleasure." in
  let res2 = bytes_to_base64 (Bytes.of_string "any pleasure.") in
  res1 = res2
;;
