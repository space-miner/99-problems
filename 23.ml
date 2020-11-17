let rand_select ls n =
  if List.length ls == 0 then []
  else
    let init = List.hd ls in
    let rec sample res i = function 
      | [] -> res
      | hd::tl ->
          if (Random.float 1.) <= (1. /. i) then sample hd (i +. 1.) tl
          else sample res (i +. 1.) tl
    in 
    let rand_elem ls = sample init 2. (List.tl ls)
    in
    let rec aux res i =
      if i == n then res
      else aux ((rand_elem ls) :: res) (i+1)
    in aux [] 0
