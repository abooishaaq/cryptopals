let base64_to_bytes b = 
  match Base64.decode b with 
  | Ok b -> b
  | Error (`Msg e) -> failwith e
;;