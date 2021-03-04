let phi n =
    let coprime n m = 
        let rec gcd n m =
            if m = 0 then n else gcd m (n mod m)
        in
        gcd n m = 1
    in
    let rec aux n d acc =
        if n <= d then acc
        else
            if coprime n d then aux n (d+1) (acc+1)
            else aux n (d+1) acc
    in
    aux n 1 0
