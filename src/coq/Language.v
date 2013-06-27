Require Import Coq.Bool.Bool.
Require Import Regex.

Fixpoint is_empty_language r :=
  match r with
  | Phi => True

  | Epsilon
  | Star _
  | Letter _ => False

  | Choice r1 r2 => is_empty_language r1 /\ is_empty_language r2
  | Concat r1 r2 => is_empty_language r1 \/ is_empty_language r2
  | Intersect r1 r2 => is_empty_language r1 \/ is_empty_language r2

  | Not r => not_is_empty_language r
  end
with not_is_empty_language r :=
  match r with
  | Epsilon => True

  (* \neg( \Sigma^* ) is the empty language, but this is an approximation *)
  | Star _
  | Letter _
  | Phi => False

  (* De Morgan *)
  | Intersect r1 r2 => not_is_empty_language r1 /\ not_is_empty_language r2
  | Choice r1 r2 => not_is_empty_language r1 \/ not_is_empty_language r2

  (* TODO: unsure.. *)
  | Concat r1 r2 => not_is_empty_language r1 \/ not_is_empty_language r2

  (* Double negation *)
  | Not r => is_empty_language r
  end.


Fixpoint nullable r :=
  match r with
  | Epsilon
  | Star _ => true

  | Not r => negb (nullable r)

  | Phi
  | Letter _ => false

  (* e.g. a | a* is nullable *)
  | Choice r1 r2 => nullable r1 || nullable r2
  (* e.g. a & a* is not nullable *)
  | Intersect r1 r2
  (* e.g. a a* is not nullable *)
  | Concat r1 r2 => nullable r1 && nullable r2
  end.


Fixpoint regex_of_pattern p :=
  match p with
  | VarBase x r =>
      r
  | VarGroup x p =>
      regex_of_pattern p
  | PatIntersect p1 p2 =>
      Intersect (regex_of_pattern p1) (regex_of_pattern p2)
  | PatChoice p1 p2 =>
      Choice (regex_of_pattern p1) (regex_of_pattern p2)
  | PatConcat p1 p2 =>
      Concat (regex_of_pattern p1) (regex_of_pattern p2)
  | PatStar p =>
      Star (regex_of_pattern p)
  end.
