(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require int.Int.
Require int.Abs.
Require int.EuclideanDivision.
Require int.ComputerDivision.
Require number.Parity.
Require number.Divisibility.
Require number.Prime.
Require map.Map.

(* Why3 assumption *)
Definition lt_nat (x:Z) (y:Z): Prop := (0%Z <= y)%Z /\ (x < y)%Z.

(* Why3 assumption *)
Inductive lex: (Z* Z)%type -> (Z* Z)%type -> Prop :=
  | Lex_1 : forall (x1:Z) (x2:Z) (y1:Z) (y2:Z), (lt_nat x1 x2) -> (lex (x1,
      y1) (x2, y2))
  | Lex_2 : forall (x:Z) (y1:Z) (y2:Z), (lt_nat y1 y2) -> (lex (x, y1) (x,
      y2)).

(* Why3 assumption *)
Inductive ref (a:Type) :=
  | mk_ref : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Implicit Arguments mk_ref [[a]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a} (v:(ref a)): a :=
  match v with
  | (mk_ref x) => x
  end.

Axiom array : forall (a:Type), Type.
Parameter array_WhyType : forall (a:Type) {a_WT:WhyType a},
  WhyType (array a).
Existing Instance array_WhyType.

Parameter elts: forall {a:Type} {a_WT:WhyType a}, (array a) -> (Z -> a).

Parameter length: forall {a:Type} {a_WT:WhyType a}, (array a) -> Z.

Axiom array'invariant : forall {a:Type} {a_WT:WhyType a}, forall (self:(array
  a)), (0%Z <= (length self))%Z.

(* Why3 assumption *)
Definition mixfix_lbrb {a:Type} {a_WT:WhyType a} (a1:(array a)) (i:Z): a :=
  ((elts a1) i).

Parameter mixfix_lblsmnrb: forall {a:Type} {a_WT:WhyType a}, (array a) ->
  Z -> a -> (array a).

Axiom mixfix_lblsmnrb_spec : forall {a:Type} {a_WT:WhyType a},
  forall (a1:(array a)) (i:Z) (v:a), ((length (mixfix_lblsmnrb a1 i
  v)) = (length a1)) /\ ((elts (mixfix_lblsmnrb a1 i
  v)) = (map.Map.set (elts a1) i v)).

(* Why3 assumption *)
Definition no_prime_in (l:Z) (u:Z): Prop := forall (x:Z), ((l < x)%Z /\
  (x < u)%Z) -> ~ (number.Prime.prime x).

(* Why3 assumption *)
Definition first_primes (p:(array Z)) (u:Z): Prop := ((mixfix_lbrb p
  0%Z) = 2%Z) /\ ((forall (i:Z) (j:Z), ((0%Z <= i)%Z /\ ((i < j)%Z /\
  (j < u)%Z)) -> ((mixfix_lbrb p i) < (mixfix_lbrb p j))%Z) /\
  ((forall (i:Z), ((0%Z <= i)%Z /\ (i < u)%Z) -> (number.Prime.prime
  (mixfix_lbrb p i))) /\ forall (i:Z), ((0%Z <= i)%Z /\
  (i < (u - 1%Z)%Z)%Z) -> (no_prime_in (mixfix_lbrb p i) (mixfix_lbrb p
  (i + 1%Z)%Z)))).

(* Why3 goal *)
Theorem exists_prime : forall (p:(array Z)) (u:Z), (1%Z <= u)%Z ->
  ((first_primes p u) -> forall (d:Z), ((2%Z <= d)%Z /\ (d <= (mixfix_lbrb p
  (u - 1%Z)%Z))%Z) -> ((number.Prime.prime d) -> exists i:Z, ((0%Z <= i)%Z /\
  (i < u)%Z) /\ (d = (mixfix_lbrb p i)))).
(* Why3 intros p u h1 h2 d (h3,h4) h5. *)
Proof.
intros p u hu. generalize hu.
pattern u; apply natlike_ind; intros. 3: omega.
apply False_ind; omega.
assert (case: (x=0 \/ 0 < x)%Z) by omega. destruct case.
subst x.
exists Z0; split.
omega.
red in H1.
simpl in H2.
assert (d = 2)%Z by omega.
subst; omega.
ring_simplify (Zsucc x - 1)%Z in H2.
assert (case: (d <= elts p (x-1) \/ elts p (x-1) < d)%Z) by omega. destruct case.
destruct H0 with (d := d) as (i, (hi1, hi2)); intuition.
destruct H1 as (p0, (sorted, (only_primes, all_primes))).
red; split.
auto.
split; intros.
apply sorted; omega.
split; intros.
apply only_primes; omega.
apply all_primes; omega.
exists i; intuition.
unfold mixfix_lbrb in H2.
assert (case: (d = elts p x \/ d < elts p x)%Z) by omega. destruct case.
exists x; intuition.
apply False_ind.
destruct H1 as (_, (_, (_, h))).
revert H3.
apply (h (x-1)%Z); try omega.
unfold mixfix_lbrb.
ring_simplify (x-1+1)%Z; omega.
Qed.
