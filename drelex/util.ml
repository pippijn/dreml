let reduce f = function
  | [] -> []
  | x :: xs -> List.fold_left f x xs


let rec rewrite f x =
  let x' = f x in
  if x = x' then
    x
  else
    rewrite f x'

  
let equal_fst a b =
  fst a = fst b
