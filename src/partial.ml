open CorePervasives
open Types


(* Deterministic derivative (not partial) computation *)
let derive_det derive l r =
  BatList.reduce (fun rs r -> Choice (r, rs)) (derive l r)


(* ·\p· :: l -> r -> [r] *)
let rec derive l : regex -> regex list =
  let module Enum = List in function
  | Epsilon
  | Phi -> [Phi]
  | Letter l2 ->
      if l == l2 then
        [Epsilon]
      else
        [Phi]
  | Choice (r1, r2) ->
      derive l r1 @ derive l r2
  | Concat (r1, r2) ->
      if Language.nullable r1 then
        [? Concat (r, r2) | r <- derive l r1 ?]
        @
        derive l r2
      else
        [? Concat (r, r2) | r <- derive l r1 ?]
  | Star r as star ->
      [? Concat (r, star) | r <- derive l r ?]
  | Repeat (r, 1) ->
      derive l r
  | Repeat (r, n) ->
      assert (n > 1);
      derive l (Concat (r, Repeat (r, n - 1)))
  (* these degenerate to a derivative computation *)
  | Not r ->
      [Not (derive_det derive l r)]
  | Intersect (r1, r2) ->
      [Intersect (derive_det derive l r1, derive_det derive l r2)]


module Make(Lbl : LabelType)(Tag : TransitionType) = struct

  module Tag = Tag.Make(Lbl)

  (* ·\p· :: l -> p -> [p] *)
  let rec derive_pat l : Lbl.t pattern -> (Lbl.t pattern * Tag.t) list =
    let module Enum = List in function
    | VarBase (x, r) ->
        let f = Tag.update x in
        [? VarBase (x, r), f | r <- derive l r ?]
    | VarGroup (x, p) ->
        let update = Tag.update x in
        [? VarGroup (x, p'), Tag.combine update f | (p', f) <- derive_pat l p ?]
    (*| PatIntersect (p1, p2) ->*)
        (*[PatIntersect (derive_pat l p1, derive_pat l p2)]*)
    | PatChoice (p1, p2) ->
        derive_pat l p1 @ derive_pat l p2
    | PatConcat (p1, p2) ->
        let p1_derived =
          [? PatConcat (p', p2), f | (p', f) <- derive_pat l p1 ?]
        in
        if Language.nullable (Language.regex_of_pattern p1) then
          p1_derived @ derive_pat l p2
        else
          p1_derived
    | PatStar (p) as star ->
        let iterate = Tag.iterate p in
        [? PatConcat (p', star), Tag.combine f iterate | (p', f) <- derive_pat l p ?]
    | PatRepeat (p, 1) ->
        let iterate = Tag.iterate p in
        [? p', Tag.combine f iterate | (p', f) <- derive_pat l p ?]
    | PatRepeat (p, n) ->
        assert (n > 1);
        let iterate = Tag.iterate p in
        let repeat = PatRepeat (p, n - 1) in
        [? PatConcat (p', repeat), Tag.combine f iterate | (p', f) <- derive_pat l p ?]

end
