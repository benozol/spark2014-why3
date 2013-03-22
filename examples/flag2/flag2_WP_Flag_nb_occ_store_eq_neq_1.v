(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require map.Map.

(* Why3 assumption *)
Definition unit  := unit.

(* Why3 assumption *)
Inductive ref (a:Type) {a_WT:WhyType a} :=
  | mk_ref : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Implicit Arguments mk_ref [[a] [a_WT]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a}(v:(ref a)): a :=
  match v with
  | (mk_ref x) => x
  end.

(* Why3 assumption *)
Inductive color  :=
  | Blue : color 
  | White : color 
  | Red : color .
Axiom color_WhyType : WhyType color.
Existing Instance color_WhyType.

(* Why3 assumption *)
Definition monochrome(a:(map.Map.map Z color)) (i:Z) (j:Z) (c:color): Prop :=
  forall (k:Z), ((i <= k)%Z /\ (k < j)%Z) -> ((map.Map.get a k) = c).

Parameter nb_occ: (map.Map.map Z color) -> Z -> Z -> color -> Z.

Axiom nb_occ_null : forall (a:(map.Map.map Z color)) (i:Z) (j:Z) (c:color),
  (j <= i)%Z -> ((nb_occ a i j c) = 0%Z).

Axiom nb_occ_add_eq : forall (a:(map.Map.map Z color)) (i:Z) (j:Z) (c:color),
  ((i < j)%Z /\ ((map.Map.get a (j - 1%Z)%Z) = c)) -> ((nb_occ a i j
  c) = ((nb_occ a i (j - 1%Z)%Z c) + 1%Z)%Z).

Axiom nb_occ_add_neq : forall (a:(map.Map.map Z color)) (i:Z) (j:Z)
  (c:color), ((i < j)%Z /\ ~ ((map.Map.get a (j - 1%Z)%Z) = c)) -> ((nb_occ a
  i j c) = (nb_occ a i (j - 1%Z)%Z c)).

Axiom nb_occ_split : forall (a:(map.Map.map Z color)) (i:Z) (j:Z) (k:Z)
  (c:color), ((i <= j)%Z /\ (j <= k)%Z) -> ((nb_occ a i k c) = ((nb_occ a i j
  c) + (nb_occ a j k c))%Z).

Axiom nb_occ_store_outside_up : forall (a:(map.Map.map Z color)) (i:Z) (j:Z)
  (k:Z) (c:color), ((i <= j)%Z /\ (j <= k)%Z) -> ((nb_occ (map.Map.set a k c)
  i j c) = (nb_occ a i j c)).

Axiom nb_occ_store_outside_down : forall (a:(map.Map.map Z color)) (i:Z)
  (j:Z) (k:Z) (c:color), ((k < i)%Z /\ (i <= j)%Z) -> ((nb_occ (map.Map.set a
  k c) i j c) = (nb_occ a i j c)).

Axiom nb_occ_store_eq_eq : forall (a:(map.Map.map Z color)) (i:Z) (j:Z) (k:Z)
  (c:color), ((i <= k)%Z /\ (k < j)%Z) -> (((map.Map.get a k) = c) ->
  ((nb_occ (map.Map.set a k c) i j c) = (nb_occ a i j c))).

Open Scope Z_scope.
Require Import Why3.
Ltac ae := why3 "Alt-Ergo,0.95.1," timelimit 15.

(* Why3 goal *)
Theorem nb_occ_store_eq_neq : forall (a:(map.Map.map Z color)) (i:Z) (j:Z)
  (k:Z) (c:color), ((i <= k)%Z /\ (k < j)%Z) -> ((~ ((map.Map.get a
  k) = c)) -> ((nb_occ (map.Map.set a k c) i j c) = ((nb_occ a i j
  c) + 1%Z)%Z)).
intros a i j k c (Hik & Hkj) H.
rewrite nb_occ_split with (j:=k); auto with zarith.
rewrite nb_occ_store_outside_up; auto with zarith.
rewrite nb_occ_split with (i:=k) (j:=k+1); auto with zarith.
rewrite nb_occ_split with (i:=i) (j:=k) (k:=j); auto with zarith.
rewrite nb_occ_split with (i:=k) (j:=k+1) (k:=j); auto with zarith.
rewrite nb_occ_store_outside_down with (i:=k+1); auto with zarith.
ae.
Qed.


