Require Import Coq.Arith.Peano_dec.
Require Import Coq.Lists.List.
Require Import Coq.Lists.ListSet.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.


Section syntax.

  Inductive regex : Type :=
    | Intersect : regex -> regex -> regex
    | Choice : regex -> regex -> regex
    | Concat : regex -> regex -> regex
    | Star : regex -> regex
    | Not : regex -> regex
    | Epsilon : regex
    | Phi : regex
    | Letter : ascii -> regex
    .

  Inductive pattern : Type :=
    | VarBase : string -> regex -> pattern
    | VarGroup : string -> pattern -> pattern
    | PatIntersect : pattern -> pattern -> pattern
    | PatChoice : pattern -> pattern -> pattern
    | PatConcat : pattern -> pattern -> pattern
    | PatStar : pattern -> pattern
    .

  Definition env := list (string * string).

  Definition exprsets := set (set regex).
  Definition exprsets_pat := set (set pattern * (env -> env)).

End syntax.


Section equality.

  Definition regex_eq : forall x y : regex, {x = y} + {x <> y}.
    decide equality; apply ascii_dec.
  Defined.

  Definition pattern_eq : forall x y : pattern, {x = y} + {x <> y}.
    generalize string_dec regex_eq.
    decide equality.
  Defined.

  Definition transition_eq : forall x y : pattern * (env -> env), {x = y} + {x <> y}.
    refine (fun x y =>
      match x, y with
      | (x, xtag), (y, ytag) =>
          if pattern_eq x y then
            left _ _
          else
            right _ _
      end
    ).
    congruence.
  Defined.

End equality.
