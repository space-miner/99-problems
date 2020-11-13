let pack ls =
  let rec aux res l = function
    | [] -> res
    | [x] -> res @ [x::l]
    | x::(y::ys as tl) -> 
        if x <> y then aux (res @ [x::l]) [] tl
        else aux res (x::l) tl
  in aux [] [] ls
