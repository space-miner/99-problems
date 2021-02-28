let is_prime p =
    let rec aux p n =
        n * n > p || (p mod n <> 0 && aux p (n+1))
    in
    aux p 2
