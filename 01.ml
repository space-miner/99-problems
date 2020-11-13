let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl
