let char_list_to_string l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | h::t -> aux t (acc ^ (Char.escaped h))
  in aux l ""
;;

let is_alpha code = 
  code >= 65 && code <= 90 || code >= 97 && code <= 122
;;

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines 
;;

let repeat_bytes b r =
  let byts = Bytes.make (r * (Bytes.length b)) ' ' in
  let rec aux b r ix =
    if r = 0 then
      byts
    else
      begin
        Bytes.blit b 0 byts ix (Bytes.length b);
        aux b (r - 1) (ix + (Bytes.length b))
      end
  in aux b r 0
;;

let drop l n =
  let rec aux l n =
    match n with
    | 0 -> l
    | _ -> 
      match l with
      | [] -> []
      | _ -> aux (List.tl l) (n - 1)
  in aux l n
;;

let take l n =
  let rec aux l n acc =
    match n with
    | 0 -> acc
    | _ -> 
      match l with
      | [] -> acc
      | h::t -> aux t (n - 1) (acc @ [h])
  in aux l n []
;;

let split l n =
  let rec aux l n acc =
    match l with
    | [] -> acc
    | _ -> aux (drop l n) n (acc @ [take l n])
  in aux l n []
;;

let%test _ = drop [1;2;3;4;5;6;7;8;9;10] 3 = [4;5;6;7;8;9;10]

let%test _ = split [1;2;3;4;5;6;7;8;9;10] 3 = [[1;2;3];[4;5;6];[7;8;9];[10]]

let bytes_to_char_list bytes =
  let rec aux bytes ix acc =
    if ix = Bytes.length bytes then
      acc
    else
      aux bytes (ix + 1) (acc @ [Bytes.get bytes ix])
  in aux bytes 0 []
;;

let char_list_to_bytes chars =
  let rec aux chars ix acc =
    match chars with
    | [] -> acc
    | h::t -> 
      Bytes.set acc ix h;
      aux t (ix + 1) acc
  in aux chars 0 (Bytes.create (List.length chars))
;;
