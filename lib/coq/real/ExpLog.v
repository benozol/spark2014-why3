(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require Import Rtrigo_def.
Require Import Rpower.
Require BuiltIn.
Require real.Real.

(* Why3 comment *)
(* exp is replaced with (exp x) by the coq driver *)

(* Why3 goal *)
Lemma Exp_zero : ((exp 0%R) = 1%R).
exact exp_0.
Qed.

Require Import Exp_prop.

(* Why3 goal *)
Lemma Exp_sum : forall (x:R) (y:R),
  ((exp (x + y)%R) = ((exp x) * (exp y))%R).
exact exp_plus.
Qed.

(* Why3 comment *)
(* log is replaced with (ln x) by the coq driver *)

(* Why3 goal *)
Lemma Log_one : ((ln 1%R) = 0%R).
exact ln_1.
Qed.

(* Why3 goal *)
Lemma Log_mul : forall (x:R) (y:R), ((0%R < x)%R /\ (0%R < y)%R) ->
  ((ln (x * y)%R) = ((ln x) + (ln y))%R).
intros x y (Hx,Hy).
now apply ln_mult.
Qed.

(* Why3 goal *)
Lemma Log_exp : forall (x:R), ((ln (exp x)) = x).
exact ln_exp.
Qed.

(* Why3 goal *)
Lemma Exp_log : forall (x:R), (0%R < x)%R -> ((exp (ln x)) = x).
exact exp_ln.
Qed.

(* Why3 assumption *)
Definition log2 (x:R): R := (Rdiv (ln x) (ln 2%R))%R.

(* Why3 assumption *)
Definition log10 (x:R): R := (Rdiv (ln x) (ln 10%R))%R.

