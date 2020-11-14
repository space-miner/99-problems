type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode ls =
  let rec aux res n = function
    | [] -> res
    | [x] -> 
        if n = 0 then res @ [One x]
        else res @ [Many (n+1, x)]
    | x::(y::ys as tl) ->
        if x <> y then
          if n = 0 then aux (res @ [One x]) 0 tl 
          else aux (res @ [Many (n+1, x)]) 0 tl
        else aux res (n+1) tl
  in aux [] 0 ls
