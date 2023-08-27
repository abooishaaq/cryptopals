let byte_to_hex = 
  let hex = "0123456789abcdef" in
  fun b -> 
    let i = int_of_char b in
    hex.[i lsr 4], hex.[i land 0xf]
  ;;

let bytes_to_hex bytes = 
  let len = Bytes.length bytes in
  let hex = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    let c1, c2 = byte_to_hex (Bytes.get bytes i) in
    Bytes.set hex (i * 2) c1;
    Bytes.set hex (i * 2 + 1) c2
  done;
  hex

let%test "bytes to hex" =   
  let bytes = Bytes.of_string "\xff\xcf\xc0\x01" in
  bytes_to_hex bytes = Bytes.of_string "ffcfc001"
