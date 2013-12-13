(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require Import R_sqrt.
Require BuiltIn.
Require real.Real.

(* Why3 goal *)
Lemma sqr_def : forall (x:R), ((Rsqr x) = (x * x)%R).
reflexivity.
Qed.

(* Why3 comment *)
(* sqrt is replaced with (sqrt x) by the coq driver *)

(* Why3 goal *)
Lemma Sqrt_positive : forall (x:R), (0%R <= x)%R -> (0%R <= (sqrt x))%R.
intros x _.
apply sqrt_pos.
Qed.

(* Why3 goal *)
Lemma Sqrt_square : forall (x:R), (0%R <= x)%R -> ((Rsqr (sqrt x)) = x).
exact sqrt_sqrt.
Qed.

(* Why3 goal *)
Lemma Square_sqrt : forall (x:R), (0%R <= x)%R -> ((sqrt (x * x)%R) = x).
exact sqrt_square.
Qed.

(* Why3 goal *)
Lemma Sqrt_mul : forall (x:R) (y:R), ((0%R <= x)%R /\ (0%R <= y)%R) ->
  ((sqrt (x * y)%R) = ((sqrt x) * (sqrt y))%R).
intros x y (hx & hy); now apply sqrt_mult.
Qed.

(* Why3 goal *)
Lemma Sqrt_le : forall (x:R) (y:R), ((0%R <= x)%R /\ (x <= y)%R) ->
  ((sqrt x) <= (sqrt y))%R.
intros x y (h1 & h2); apply sqrt_le_1; auto.
apply Rle_trans with x; auto.
Qed.

