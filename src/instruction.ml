open CorePervasives
open Types

type 'label transition = 'label instruction

module Make(Lbl : Types.LabelType) = struct

  module Transition = Transition.Make(Lbl)

  type t = Lbl.t transition


  let update x = Update x
  let iterate p = Iterate (Pattern.vars_of_pattern p)
  let combine i1 i2 = Combine (i1, i2)


  let rec execute pos insn env =
    match insn with
    | Update x -> Transition.update x pos env
    | Iterate (vars) -> Transition.rename vars env
    | Combine (i1, i2) -> (execute pos i1 % execute pos i2) env


  let rec to_string = function
    | Update x ->
        Lbl.to_string x
    | Iterate (vars) ->
        "iterate_{" ^ String.concat "," (List.map Lbl.to_string vars) ^ "}"
    | Combine (i1, i2) ->
        to_string i1 ^ "; " ^ to_string i2


  let to_string insn =
    "{" ^ to_string insn ^ "}"


end
