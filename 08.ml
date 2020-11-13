let compress ls =
  let rec aux res = function
    | [] -> res
    | [x] -> res @ [x]
    | x::(y::ys as tl) -> if x = y then aux res tl
                  else aux (res @ [x]) tl
  in aux [] ls
