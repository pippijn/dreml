open CorePervasives
open Types


type t = instruction


let update (x, l) = Update (x, l)
let iterate p = Iterate (p)
let combine i1 i2 = Combine (i1, i2)


let rec execute insn env =
  match insn with
  | Update (x, l) -> Transition.update (x, l) env
  | Iterate (p) -> Transition.iterate p env
  | Combine (i1, i2) -> (execute i1 -| execute i2) env


let rec to_string = function
  | Update (x, l) ->
      x ^ "+=" ^ Char.escaped l
  | Iterate (p) ->
      "iterate_{" ^ String.concat "," (Pattern.vars_of_pattern p) ^ "}"
  | Combine (i1, i2) ->
      to_string i1 ^ "; " ^ to_string i2


let to_string insn =
  "{" ^ to_string insn ^ "}"
