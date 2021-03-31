let factors n =
    let rec aux n d acc =
        if n = 1 then acc
        else
            if n mod d = 0 then aux (n/d) d (acc @ [d])
            else aux n (d+1) acc
    in
    aux n 2 []
