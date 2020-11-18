let permutation ls =
  let rand_elem ls =
    let hd = List.hd ls in 
    let tl = List.tl ls in
    let rec helper (selected, rest) i = function
      | [] -> (selected, rest)
      | x::xs -> 
          if Random.float 1. <= 1. /. i 
            then helper (x, selected :: rest) (1. +. i) xs
          else helper (selected, x :: rest) (1. +. i) xs
    in helper (hd, []) 2. tl
  in
  let rec aux res = function
    | [] -> res
    | x::xs -> 
        let (selected, rest) = rand_elem (x::xs) in
        aux (selected :: res) rest
  in
  aux [] ls
