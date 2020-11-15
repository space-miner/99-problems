let split ls k =
  let rec aux res n = function
    | [] -> (res, [])
    | hd::tl -> 
        if n = 1 then (res@[hd], tl)
        else aux (res@[hd]) (n-1) tl
  in 
  aux [] k ls 
