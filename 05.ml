let rev ls =
  let rec aux res = function
    | [] -> res
    | hd :: tl -> aux (hd::res) tl
  in aux [] ls
