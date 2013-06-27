open Types


let rec vars_of_pattern vars = function
  | VarBase (x, r) ->
      x :: vars
  | VarGroup (x, p) ->
      x :: vars_of_pattern vars p
  | PatIntersect (p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | PatChoice (p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | PatConcat (p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | PatStar (p) ->
      vars_of_pattern vars p
  | PatRepeat (p, _) ->
      vars_of_pattern vars p

let vars_of_pattern = vars_of_pattern []


let variable_clashes p =
  let vars = vars_of_pattern p in
  let seen = Hashtbl.create (List.length vars) in
  List.fold_left (fun clashes var ->
    if Hashtbl.mem seen var then
      var :: clashes
    else (
      Hashtbl.add seen var true;
      clashes
    )
  ) [] vars
