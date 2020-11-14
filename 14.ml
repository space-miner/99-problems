let duplicate ls =
  let rec aux res = function
    | [] -> res
    | hd::tl -> aux (hd::hd::res) tl
  in 
  aux [] ls
  |> List.rev
