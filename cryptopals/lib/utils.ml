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
  