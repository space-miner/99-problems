let phi_improved n = 
    let pow n m =
        let rec pow_aux n m acc =
            if m = 0 then acc
            else pow_aux n (m-1) (n * acc)
        in
        if m < 1 then 1
        else pow_aux n m 1
    in
    let factors n =
        let rec factors_aux n d c acc =
            if n = 1 then
                if c > 0 then acc @ [(d, c)]
                else acc
            else
                if n mod d = 0 then factors_aux (n/d) d (c+1) acc
                else
                    if c > 0 then factors_aux n (d+1) 0 (acc @ [(d, c)])
                    else factors_aux n (d+1) 0 acc
        in
        factors_aux n 2 0 []
    in
    List.fold_left ( * ) 1
        (List.map (fun (p, c) -> (p-1) * (pow p (c-1))) (factors n))
