(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2019   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.

(* Why3 comment *)
(* min is replaced with (ZArith.BinInt.Z.min x x1) by the coq driver *)

(* Why3 goal *)
Lemma min_def :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z),
  ((x <= y)%Z -> ((ZArith.BinInt.Z.min x y) = x)) /\
  (~ (x <= y)%Z -> ((ZArith.BinInt.Z.min x y) = y)).
Proof.
intros x y.
split ; intros H.
now apply Z.min_l.
apply Z.min_r.
omega.
Qed.

(* Why3 comment *)
(* max is replaced with (ZArith.BinInt.Z.max x x1) by the coq driver *)

(* Why3 goal *)
Lemma max_def :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z),
  ((x <= y)%Z -> ((ZArith.BinInt.Z.max x y) = y)) /\
  (~ (x <= y)%Z -> ((ZArith.BinInt.Z.max x y) = x)).
Proof.
intros x y.
split ; intros H.
now apply Z.max_r.
apply Z.max_l.
omega.
Qed.

(* Why3 goal *)
Lemma Min_r :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z), (y <= x)%Z ->
  ((ZArith.BinInt.Z.min x y) = y).
exact Z.min_r.
Qed.

(* Why3 goal *)
Lemma Max_l :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z), (y <= x)%Z ->
  ((ZArith.BinInt.Z.max x y) = x).
exact Z.max_l.
Qed.

(* Why3 goal *)
Lemma Min_comm :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z),
  ((ZArith.BinInt.Z.min x y) = (ZArith.BinInt.Z.min y x)).
exact Z.min_comm.
Qed.

(* Why3 goal *)
Lemma Max_comm :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z),
  ((ZArith.BinInt.Z.max x y) = (ZArith.BinInt.Z.max y x)).
exact Z.max_comm.
Qed.

(* Why3 goal *)
Lemma Min_assoc :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z) (z:Numbers.BinNums.Z),
  ((ZArith.BinInt.Z.min (ZArith.BinInt.Z.min x y) z) =
   (ZArith.BinInt.Z.min x (ZArith.BinInt.Z.min y z))).
Proof.
intros x y z.
apply eq_sym, Z.min_assoc.
Qed.

(* Why3 goal *)
Lemma Max_assoc :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z) (z:Numbers.BinNums.Z),
  ((ZArith.BinInt.Z.max (ZArith.BinInt.Z.max x y) z) =
   (ZArith.BinInt.Z.max x (ZArith.BinInt.Z.max y z))).
Proof.
intros x y z.
apply eq_sym, Z.max_assoc.
Qed.

