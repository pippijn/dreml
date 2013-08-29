open CorePervasives

type 'label transition = 'label Types.env -> 'label Types.env

module Make(Lbl : Types.LabelType) = struct

  type t = Lbl.t transition


  let rec update x p = let open Types in function
    | [] -> [(x, Pos (p, p))]
    | (y, pos) :: env ->
        if y = x then
          let (Pos (lo, hi)) = pos in
          (y, Pos (lo, p)) :: env
        else
          (y, pos) :: update x p env


  let rename vars =
    (* rename the previous match *)
    List.map (fun (x, w) ->
      if List.mem x vars then
        (Lbl.rename x, w)
      else
        (x, w)
    )


  let iterate p =
    (* find all variable names in the pattern *)
    rename (Pattern.vars_of_pattern p)


  let combine = (%)
  let execute = identity
  let to_string _ = ""


  (*
  let update (x, l) env =
    let w =
      try
        StringMap.find x env
      with Not_found ->
        []
    in
    StringMap.add x (l :: w) env


  let iterate p env =
    (* find all variable names in the pattern *)
    let vars = Pattern.vars_of_pattern p in
    (* rename the previous match *)
    List.fold_left (fun env var ->
      try
        let w = StringMap.find var env in

        env
        |> StringMap.remove var
        |> StringMap.add ("'" ^ var) w
      with Not_found ->
        env
    ) env vars
  *)

end
