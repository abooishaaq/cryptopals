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
