type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode ls =
  let rle n x =
    if n = 0 then One x
    else Many (n+1, x)
  in
  let rec aux res n = function
    | [] -> res
    | [x] -> (rle n x)::res
    | x::(y::ys as tl) -> 
        if x <> y then aux ((rle n x)::res) 0 tl
        else aux res (n+1) tl
  in
  aux [] 0 ls
  |> List.rev
