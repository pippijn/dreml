open Types


let rec vars_of_pattern vars = function
  | VarBase (_, x, r) ->
      x :: vars
  | VarGroup (_, x, p) ->
      x :: vars_of_pattern vars p
  | PatIntersect (_, p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | PatChoice (_, p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | PatConcat (_, p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | PatStar (p) ->
      vars_of_pattern vars p
  | PatRepeat (_, p, _) ->
      vars_of_pattern vars p
  | PatNot (_, p) ->
      vars_of_pattern vars p

let vars_of_pattern p = vars_of_pattern [] p


let variable_clashes p =
  let vars = vars_of_pattern p in
  let seen = Hashtbl.create (List.length vars) in
  BatList.sort_unique compare (List.fold_left (fun clashes var ->
    if Hashtbl.mem seen var then
      var :: clashes
    else (
      Hashtbl.add seen var true;
      clashes
    )
  ) [] vars)


let rec number_pattern names = function
  | VarBase (null, x, r) ->
      VarBase (null, Hashtbl.find names x, r)
  | VarGroup (null, x, p) ->
      VarGroup (null, Hashtbl.find names x, number_pattern names p)
  | PatIntersect (null, p1, p2) ->
      PatIntersect (null, number_pattern names p1, number_pattern names p2)
  | PatChoice (null, p1, p2) ->
      PatChoice (null, number_pattern names p1, number_pattern names p2)
  | PatConcat (null, p1, p2) ->
      PatConcat (null, number_pattern names p1, number_pattern names p2)
  | PatStar (p) ->
      PatStar (number_pattern names p)
  | PatRepeat (null, p, n) ->
      PatRepeat (null, number_pattern names p, n)
  | PatNot (null, p) ->
      PatNot (null, number_pattern names p)


let number_pattern p =
  let vars = vars_of_pattern p in
  (* 'label -> int *)
  let names = Hashtbl.create (List.length vars) in
  (* int -> 'label *)
  let map = Array.make (List.length vars) "" in

  BatList.iteri (fun i var ->
    Hashtbl.add names var (i + 1);
    map.(i) <- var;
  ) vars;

  let numbered = number_pattern names p in

  numbered, map
