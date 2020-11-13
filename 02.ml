let rec last_two = function
  | [] -> None
  | [_] -> None
  | [x;y] -> Some (x,y)
  | _ :: tl -> last_two tl
