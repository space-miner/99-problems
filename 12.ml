type 'a rle =
  | One of 'a
  | Many of int * 'a

let repeat x n =
  let rec aux res x = function
    | 0 -> res
    | n -> aux (x::res) x (n-1)
  in aux [] x n

let convert = function
  | One x -> repeat x 1
  | Many (n, x) -> repeat x n

let decode ls =
  let rec aux res = function
    | [] -> res
    | hd::tl -> aux (res@(convert hd)) tl
  in aux [] ls
