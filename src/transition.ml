open CorePervasives


type t = Types.env -> Types.env


let _reverse = true


let rec update (x, l) = function
  | [] -> [(x, [l])]
  | (y, w) :: env ->
      if y = x then
        if _reverse then
          (y, l :: w) :: env
        else
          (y, w @ [l]) :: env
      else
        (y, w) :: update (x, l) env



let rename =
  let iterations = Hashtbl.create 10 in
  fun var ->
    let count =
      try
        Hashtbl.find iterations var
      with Not_found ->
        0
    in
    Hashtbl.replace iterations var (count + 1);
    "'" ^ var ^ string_of_int count


let rename var =
  "'" ^ var


let rename vars =
  (* rename the previous match *)
  List.map (fun (x, w) ->
    if List.mem x vars then
      (rename x, w)
    else
      (x, w)
  )


let iterate p =
  (* find all variable names in the pattern *)
  rename (Pattern.vars_of_pattern p)


let combine = (-|)
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
