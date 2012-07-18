(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require Import Rbasic_fun.
Require Import R_sqrt.
Require Import Rtrigo.
Require Import AltSeries. (* for def of pi *)
Require real.Real.
Require real.Abs.
Require real.FromInt.
Require int.Int.
Require real.Square.

Axiom Pi_interval : ((314159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196 / 100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)%R < PI)%R /\
  (PI < (314159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038197 / 100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)%R)%R.

Axiom Cos_plus_pi : forall (x:R),
  ((Rtrigo_def.cos (x + PI)%R) = (-(Rtrigo_def.cos x))%R).

Axiom Sin_plus_pi : forall (x:R),
  ((Rtrigo_def.sin (x + PI)%R) = (-(Rtrigo_def.sin x))%R).

Axiom Cos_plus_pi2 : forall (x:R),
  ((Rtrigo_def.cos (x + ((05 / 10)%R * PI)%R)%R) = (-(Rtrigo_def.sin x))%R).

Axiom Sin_plus_pi2 : forall (x:R),
  ((Rtrigo_def.sin (x + ((05 / 10)%R * PI)%R)%R) = (Rtrigo_def.cos x)).

Axiom Cos_neg : forall (x:R), ((Rtrigo_def.cos (-x)%R) = (Rtrigo_def.cos x)).

Axiom Sin_neg : forall (x:R),
  ((Rtrigo_def.sin (-x)%R) = (-(Rtrigo_def.sin x))%R).

Axiom Cos_sum : forall (x:R) (y:R),
  ((Rtrigo_def.cos (x + y)%R) = (((Rtrigo_def.cos x) * (Rtrigo_def.cos y))%R - ((Rtrigo_def.sin x) * (Rtrigo_def.sin y))%R)%R).

Axiom Sin_sum : forall (x:R) (y:R),
  ((Rtrigo_def.sin (x + y)%R) = (((Rtrigo_def.sin x) * (Rtrigo_def.cos y))%R + ((Rtrigo_def.cos x) * (Rtrigo_def.sin y))%R)%R).

Parameter atan: R -> R.

Axiom Tan_atan : forall (x:R), ((Rtrigo.tan (atan x)) = x).

(* Why3 assumption *)
Inductive mode  :=
  | NearestTiesToEven : mode 
  | ToZero : mode 
  | Up : mode 
  | Down : mode 
  | NearestTiesToAway : mode .

Parameter single : Type.

Parameter round: mode -> R -> R.

Parameter round_logic: mode -> R -> single.

Parameter value: single -> R.

Parameter exact: single -> R.

Parameter model: single -> R.

(* Why3 assumption *)
Definition round_error(x:single): R := (Rabs ((value x) - (exact x))%R).

(* Why3 assumption *)
Definition total_error(x:single): R := (Rabs ((value x) - (model x))%R).

(* Why3 assumption *)
Definition no_overflow(m:mode) (x:R): Prop := ((Rabs (round m
  x)) <= (33554430 * 10141204801825835211973625643008)%R)%R.

Axiom Bounded_real_no_overflow : forall (m:mode) (x:R),
  ((Rabs x) <= (33554430 * 10141204801825835211973625643008)%R)%R ->
  (no_overflow m x).

Axiom Round_monotonic : forall (m:mode) (x:R) (y:R), (x <= y)%R -> ((round m
  x) <= (round m y))%R.

Axiom Round_idempotent : forall (m1:mode) (m2:mode) (x:R), ((round m1
  (round m2 x)) = (round m2 x)).

Axiom Round_value : forall (m:mode) (x:single), ((round m
  (value x)) = (value x)).

Axiom Bounded_value : forall (x:single),
  ((Rabs (value x)) <= (33554430 * 10141204801825835211973625643008)%R)%R.

Axiom Exact_rounding_for_integers : forall (m:mode) (i:Z),
  (((-16777216%Z)%Z <= i)%Z /\ (i <= 16777216%Z)%Z) -> ((round m
  (IZR i)) = (IZR i)).

Axiom Round_down_le : forall (x:R), ((round Down x) <= x)%R.

Axiom Round_up_ge : forall (x:R), (x <= (round Up x))%R.

Axiom Round_down_neg : forall (x:R), ((round Down (-x)%R) = (-(round Up
  x))%R).

Axiom Round_up_neg : forall (x:R), ((round Up (-x)%R) = (-(round Down x))%R).

(* Why3 assumption *)
Definition of_real_post(m:mode) (x:R) (res:single): Prop :=
  ((value res) = (round m x)) /\ (((exact res) = x) /\ ((model res) = x)).

(* Why3 assumption *)
Definition add_post(m:mode) (x:single) (y:single) (res:single): Prop :=
  ((value res) = (round m ((value x) + (value y))%R)) /\
  (((exact res) = ((exact x) + (exact y))%R) /\
  ((model res) = ((model x) + (model y))%R)).

(* Why3 assumption *)
Definition sub_post(m:mode) (x:single) (y:single) (res:single): Prop :=
  ((value res) = (round m ((value x) - (value y))%R)) /\
  (((exact res) = ((exact x) - (exact y))%R) /\
  ((model res) = ((model x) - (model y))%R)).

(* Why3 assumption *)
Definition mul_post(m:mode) (x:single) (y:single) (res:single): Prop :=
  ((value res) = (round m ((value x) * (value y))%R)) /\
  (((exact res) = ((exact x) * (exact y))%R) /\
  ((model res) = ((model x) * (model y))%R)).

(* Why3 assumption *)
Definition div_post(m:mode) (x:single) (y:single) (res:single): Prop :=
  ((value res) = (round m (Rdiv (value x) (value y))%R)) /\
  (((exact res) = (Rdiv (exact x) (exact y))%R) /\
  ((model res) = (Rdiv (model x) (model y))%R)).

(* Why3 assumption *)
Definition neg_post(x:single) (res:single): Prop :=
  ((value res) = (-(value x))%R) /\ (((exact res) = (-(exact x))%R) /\
  ((model res) = (-(model x))%R)).

(* Why3 assumption *)
Definition implb(x:bool) (y:bool): bool := match (x,
  y) with
  | (true, false) => false
  | (_, _) => true
  end.

(* Why3 assumption *)
Definition lt(x:single) (y:single): Prop := ((value x) < (value y))%R.

(* Why3 assumption *)
Definition gt(x:single) (y:single): Prop := ((value y) < (value x))%R.

Require Import Interval_tactic.

(* Why3 goal *)
Theorem MethodError : forall (x:R), ((Rabs x) <= (1 / 32)%R)%R ->
  ((Rabs ((1%R - ((05 / 10)%R * (x * x)%R)%R)%R - (Rtrigo_def.cos x))%R) <= (1 / 16777216)%R)%R.
(* YOU MAY EDIT THE PROOF BELOW *)
intros x H.
interval with (i_bisect_diff x).
Qed.


