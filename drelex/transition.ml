open CorePervasives
open Types


let rec update x pos = function
  | [] -> raise Not_found
  | (y, position) :: env ->
      if y = x then
        let (Pos (lo, hi)) = position in
        (y, Pos (lo, pos)) :: env
      else
        (y, position) :: update x pos env


let update x pos l =
  try
    update x pos l
  with Not_found ->
    (x, Pos (pos, pos)) :: l


let rename vars pos =
  (* rename the previous match *)
  List.map (fun (x, w) ->
    if List.mem x vars then
      (-x, w)
    else
      ( x, w)
  )


let iterate p =
  (* find all variable names in the pattern *)
  rename (Pattern.vars_of_pattern p)


let compose f g =
  fun pos env -> f pos (g pos env)


let execute f pos env =
  f pos env


let to_string varmap f = ""
