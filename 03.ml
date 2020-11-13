let rec at k = function
  | [] -> None
  | hd :: tl -> if k = 1 then Some hd
                else at (k-1) tl
