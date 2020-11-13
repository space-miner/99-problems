let length ls =
  let rec aux n = function
    | [] -> n
    | _ :: tl -> aux (n+1) tl
  in aux 0 ls
