(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.

(* Why3 comment *)
(* min is replaced with (Zmin x x1) by the coq driver *)

(* Why3 comment *)
(* max is replaced with (Zmax x x1) by the coq driver *)

(* Why3 goal *)
Lemma Max_is_ge : forall (x:Z) (y:Z), (x <= (Zmax x y))%Z /\
  (y <= (Zmax x y))%Z.
split.
apply Zle_max_l.
apply Zle_max_r.
Qed.

(* Why3 goal *)
Lemma Max_is_some : forall (x:Z) (y:Z), ((Zmax x y) = x) \/ ((Zmax x y) = y).
intros x y.
unfold Zmax.
case Zcompare.
now left.
now right.
now left.
Qed.

(* Why3 goal *)
Lemma Min_is_le : forall (x:Z) (y:Z), ((Zmin x y) <= x)%Z /\
  ((Zmin x y) <= y)%Z.
split.
apply Zle_min_l.
apply Zle_min_r.
Qed.

(* Why3 goal *)
Lemma Min_is_some : forall (x:Z) (y:Z), ((Zmin x y) = x) \/ ((Zmin x y) = y).
intros x y.
unfold Zmin.
case Zcompare.
now left.
now left.
now right.
Qed.

(* Why3 goal *)
Lemma Max_x : forall (x:Z) (y:Z), (y <= x)%Z -> ((Zmax x y) = x).
exact Zmax_l.
Qed.

(* Why3 goal *)
Lemma Max_y : forall (x:Z) (y:Z), (x <= y)%Z -> ((Zmax x y) = y).
exact Zmax_r.
Qed.

(* Why3 goal *)
Lemma Min_x : forall (x:Z) (y:Z), (x <= y)%Z -> ((Zmin x y) = x).
exact Zmin_l.
Qed.

(* Why3 goal *)
Lemma Min_y : forall (x:Z) (y:Z), (y <= x)%Z -> ((Zmin x y) = y).
exact Zmin_r.
Qed.

(* Why3 goal *)
Lemma Max_sym : forall (x:Z) (y:Z), (y <= x)%Z -> ((Zmax x y) = (Zmax y x)).
intros x y _.
apply Zmax_comm.
Qed.

(* Why3 goal *)
Lemma Min_sym : forall (x:Z) (y:Z), (y <= x)%Z -> ((Zmin x y) = (Zmin y x)).
intros x y _.
apply Zmin_comm.
Qed.

