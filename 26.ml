let rec extract k ls =
  if k = 0 then [[]]
  else
    match ls with
    | [] -> []
    | x::xs -> 
        let include_x = List.map (fun ys -> x::ys)
          (extract (k-1) xs)
        in
        let exclude_x = extract k xs
        in 
        include_x @ exclude_x
