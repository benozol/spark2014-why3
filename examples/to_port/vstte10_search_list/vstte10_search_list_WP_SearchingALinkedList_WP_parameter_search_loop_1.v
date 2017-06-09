(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require list.List.
Require list.Length.
Require list.Nth.
Require option.Option.
Require list.HdTl.

(* Why3 assumption *)
Definition unit := unit.

(* Why3 assumption *)
Definition zero_at (l:(list Z)) (i:Z): Prop := ((list.Nth.nth i
  l) = (Some 0%Z)) /\ forall (j:Z), ((0%Z <= j)%Z /\ (j < i)%Z) ->
  ~ ((list.Nth.nth j l) = (Some 0%Z)).

(* Why3 assumption *)
Definition no_zero (l:(list Z)): Prop := forall (j:Z), ((0%Z <= j)%Z /\
  (j < (list.Length.length l))%Z) -> ~ ((list.Nth.nth j l) = (Some 0%Z)).

(* Why3 assumption *)
Inductive ref (a:Type) {a_WT:WhyType a} :=
  | mk_ref : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Implicit Arguments mk_ref [[a] [a_WT]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a} (v:(@ref a a_WT)): a :=
  match v with
  | (mk_ref x) => x
  end.


(* Why3 goal *)
Theorem WP_parameter_search_loop : forall (l:(list Z)), forall (s:(list Z))
  (i:Z), ((0%Z <= i)%Z /\
  (((i + (list.Length.length s))%Z = (list.Length.length l)) /\
  ((forall (j:Z), (0%Z <= j)%Z -> ((list.Nth.nth j
  s) = (list.Nth.nth (i + j)%Z l))) /\ forall (j:Z), ((0%Z <= j)%Z /\
  (j < i)%Z) -> ~ ((list.Nth.nth j l) = (Some 0%Z))))) -> ((~ (s = nil)) ->
  ((~ (s = nil)) -> forall (o:Z),
  match s with
  | nil => False
  | (cons h _) => (o = h)
  end -> ((o = 0%Z) -> ((((0%Z <= i)%Z /\ (i < (list.Length.length l))%Z) /\
  (zero_at l i)) \/ ((i = (list.Length.length l)) /\ (no_zero l)))))).
(* Why3 intros l s i (h1,(h2,(h3,h4))) h5 h6 o h7 h8. *)
intuition.
destruct s.
destruct H4.
subst; subst.
clear H0 H1.
left.
split.
change (Length.length (cons 0 s))%Z with (1 + Length.length s)%Z in H.
generalize (Length.Length_nonnegative s).
omega.
red; intuition.
assert (H0: (0 <= 0)%Z) by omega.
generalize (H3 0%Z H0).
generalize (Nth.nth_def 0%Z (cons 0%Z s)).
ring_simplify (i+0)%Z.
intuition.
Qed.

