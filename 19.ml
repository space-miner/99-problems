let rotate ls k =
  let l = List.length ls in
  let i = (k+l) mod l in
  let rec aux res n = function
    | [] -> res
    | hd::tl ->
        if n == i then ((hd::tl) @ res)
        else aux (res @ [hd]) (n+1) tl
  in aux [] 0 ls
