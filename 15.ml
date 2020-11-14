let rec repeat x n res =
  if n = 0 then res
  else repeat x (n-1) (x::res)

let replicate ls n =
  let rec aux res = function
    | [] -> res
    | hd::tl -> aux (res@(repeat hd n [])) tl
  in
  aux [] ls
