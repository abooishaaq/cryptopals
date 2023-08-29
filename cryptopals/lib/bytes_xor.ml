let bytes_xor b1 b2 =
  let len = min (Bytes.length b1) (Bytes.length b2) in
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set b i (Char.chr ((Char.code (Bytes.get b1 i)) lxor (Char.code (Bytes.get b2 i))))
  done;
  b
;;
