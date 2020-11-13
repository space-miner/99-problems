type 'a node =
    | One of 'a 
    | Many of 'a node list

let flatten ls =
  let rec aux res = function
    | [] -> res
    | One x :: tl -> aux (res @ [x]) tl
    | Many l :: tl -> aux (aux res l) tl
  in aux [] ls
