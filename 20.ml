let remove_at k ls =
  let rec aux res n = function
    | [] -> res
    | hd::tl ->
        if n == k then res @ tl
        else aux (res @ [hd]) (n+1) tl
  in
  aux [] 0 ls
