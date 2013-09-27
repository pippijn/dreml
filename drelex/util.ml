let reduce f = function
  | [] -> []
  | x :: xs -> List.fold_left f x xs


let equal_fst a b =
  fst a = fst b
