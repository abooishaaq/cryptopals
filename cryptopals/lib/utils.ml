let char_list_to_string l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | h::t -> aux t (acc ^ (Char.escaped h))
  in aux l ""
;;
