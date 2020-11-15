let drop ls k =
  let rec aux res n = function
    | [] -> res
    | hd::tl ->
        if n == 1 then aux res k tl
        else aux (hd::res) (n-1) tl
  in 
  aux [] k ls
  |> List.rev
