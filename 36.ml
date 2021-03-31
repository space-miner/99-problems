let factors n =
    let rec aux n d c acc =
        if n = 1 then
            if c > 0 then acc @ [(d, c)]
            else acc
        else
            if n mod d = 0 then aux (n/d) d (c+1) acc
            else 
                if c > 0 then aux n (d+1) 0 (acc @ [(d, c)])
                else aux n (d+1) 0 acc
    in
    aux n 2 0 []
