let slice ls i k =
  let rec aux res n = function
    | [] -> res
    | hd::tl ->
        if n > k then res
        else if n < i then aux res (n+1) tl
        else aux (hd::res) (n+1) tl
  in 
  aux [] 0 ls
  |> List.rev
