let insert_at elem k ls =
  let rec aux res n = function
    | [] -> 
        if n == k then res @ [elem]
        else res
    | hd::tl ->
        if n == k then res @ [elem] @ (hd::tl)
        else aux (res @ [hd]) (n+1) tl
  in
  aux [] 0 ls
