let compress ls = 
  let rec aux res = function
    | [] -> res
    | [x] -> res @ [x]
    | x::y::ys -> if x = y then aux res (y::ys) 
                  else aux (res @ [x]) (y::ys)
  in aux [] ls
