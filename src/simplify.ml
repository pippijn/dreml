open Types


(* 4.1 Weaker notions of RE equivalence *)
let rec simplify = function
  (* & *)
  | Intersect (r1, r2) when r1 = r2 -> r1

  | Intersect (Phi, _)
  | Intersect (_, Phi) -> Phi

  | Intersect (Not Phi, r)
  | Intersect (r, Not Phi) -> r

  (* juxtaposition *)
  | Concat (Phi, _)
  | Concat (_, Phi) -> Phi

  | Concat (Epsilon, r)
  | Concat (r, Epsilon) -> r

  (* + *)
  | Choice (r1, r2) when r1 = r2 -> r1

  | Choice (Not Phi, _)
  | Choice (_, Not Phi) -> Not Phi

  | Choice (Phi, r)
  | Choice (r, Phi) -> r

  (* * *)
  | Star (Star _ as inner) -> inner
  | Star Epsilon -> Epsilon
  | Star Phi -> Epsilon

  (* ~(~r) -> r *)
  | Not (Not r) -> r

  (* ~eps -> phi *)
  | Not Epsilon -> Phi


  (* XXX: this simplification makes this testcase trivial and useless:
     (a: ((a+b)*a(a+b)^3) & ((~((~a)&(~b)))*a(a+b)^3) )
  *)

  (* remove negation by De Morgan, if possible *)
  (*| Not (Intersect (Not r1, Not r2)) -> Choice (r1, r2)*)
  (*| Not (Choice (Not r1, Not r2)) -> Intersect (r1, r2)*)

  | Repeat (_, 0) -> Epsilon
  | Repeat (r, 1) -> r

  (* Recursive simplify *)
  | Concat (r1, r2) -> Concat (simplify r1, simplify r2)
  | Choice (r1, r2) -> Choice (simplify r1, simplify r2)
  | Intersect (r1, r2) -> Intersect (simplify r1, simplify r2)
  | Star r -> Star (simplify r)
  | Repeat (r, n) -> Repeat (simplify r, n)
  | Not r -> Not (simplify r)

  (* Keep atoms *)
  | Phi
  | Epsilon
  | Letter _ as r -> r

let simplify = Util.rewrite simplify

  
let rec simplify_pat = function
  (* We need to catch this here to ensure termination *)
  | PatConcat (VarBase (_, Epsilon), r)
  | PatConcat (r, VarBase (_, Epsilon)) -> r

  (* * *)
  | PatStar (PatStar _ as inner) -> inner

  (* Recursive simplify *)
  | VarBase (x, r) -> VarBase (x, simplify r)
  | VarGroup (x, p) -> VarGroup (x, simplify_pat p)

  | PatIntersect (p1, p2) -> PatIntersect (simplify_pat p1, simplify_pat p2)
  | PatConcat (p1, p2) -> PatConcat (simplify_pat p1, simplify_pat p2)
  | PatChoice (p1, p2) -> PatChoice (simplify_pat p1, simplify_pat p2)
  | PatStar p -> PatStar (simplify_pat p)
  | PatRepeat (p, n) -> PatRepeat (simplify_pat p, n)

let simplify_pat = Util.rewrite simplify_pat

let simplify x : regex = x
let simplify_pat x : pattern = x
