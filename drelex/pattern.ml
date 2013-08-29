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
  | PatNot (p) ->
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
  | VarBase (x, r) ->
      VarBase (Hashtbl.find names x, r)
  | VarGroup (x, p) ->
      VarGroup (Hashtbl.find names x, number_pattern names p)
  | PatIntersect (p1, p2) ->
      PatIntersect (number_pattern names p1, number_pattern names p2)
  | PatChoice (p1, p2) ->
      PatChoice (number_pattern names p1, number_pattern names p2)
  | PatConcat (p1, p2) ->
      PatConcat (number_pattern names p1, number_pattern names p2)
  | PatStar (p) ->
      PatStar (number_pattern names p)
  | PatRepeat (p, n) ->
      PatRepeat (number_pattern names p, n)
  | PatNot (p) ->
      PatNot (number_pattern names p)


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
