open CorePervasives
open Types


let update x = Update x
let iterate p = Iterate (Pattern.vars_of_pattern p)
let compose f g = Compose (f, g)

let rec execute f pos env =
  match f with
  | Update x -> Transition.update x pos env
  | Iterate vars -> Transition.rename vars pos env
  | Compose (f, g) -> execute f pos (execute g pos env)

(*let execute f pos env = env*)


let to_string string_of_label f =
  let rec to_string = function
    | Update x -> "update(" ^ string_of_label x ^ ")"
    | Iterate vars -> "iterate_{" ^ String.concat "," (List.map string_of_label vars) ^ "}"
    | Compose (f, g) -> to_string f ^ "; " ^ to_string g
  in

  "{" ^ to_string f ^ "}"
