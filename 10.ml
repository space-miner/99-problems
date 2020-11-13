let encode ls =
  let rec aux res n = function 
    | [] -> res
    | [x] -> res @ [(n+1, x)]
    | x::(y::ys as tl) ->
        if x <> y then aux (res @ [(n+1, x)]) 0 tl
        else aux res (n+1) tl
  in aux [] 0 ls
