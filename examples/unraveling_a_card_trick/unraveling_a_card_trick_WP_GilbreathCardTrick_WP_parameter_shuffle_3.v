(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.

(* Why3 assumption *)
Definition unit := unit.

(* Why3 assumption *)
Inductive list
  (a:Type) {a_WT:WhyType a} :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Axiom list_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (list a).
Existing Instance list_WhyType.
Implicit Arguments Nil [[a] [a_WT]].
Implicit Arguments Cons [[a] [a_WT]].

(* Why3 assumption *)
Fixpoint length {a:Type} {a_WT:WhyType a} (l:(list a)) {struct l}: Z :=
  match l with
  | Nil => 0%Z
  | (Cons _ r) => (1%Z + (length r))%Z
  end.

Axiom Length_nonnegative : forall {a:Type} {a_WT:WhyType a}, forall (l:(list
  a)), (0%Z <= (length l))%Z.

Axiom Length_nil : forall {a:Type} {a_WT:WhyType a}, forall (l:(list a)),
  ((length l) = 0%Z) <-> (l = (Nil :(list a))).

(* Why3 assumption *)
Fixpoint infix_plpl {a:Type} {a_WT:WhyType a} (l1:(list a)) (l2:(list
  a)) {struct l1}: (list a) :=
  match l1 with
  | Nil => l2
  | (Cons x1 r1) => (Cons x1 (infix_plpl r1 l2))
  end.

Axiom Append_assoc : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)) (l3:(list a)), ((infix_plpl l1 (infix_plpl l2
  l3)) = (infix_plpl (infix_plpl l1 l2) l3)).

Axiom Append_l_nil : forall {a:Type} {a_WT:WhyType a}, forall (l:(list a)),
  ((infix_plpl l (Nil :(list a))) = l).

Axiom Append_length : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)), ((length (infix_plpl l1
  l2)) = ((length l1) + (length l2))%Z).

(* Why3 assumption *)
Fixpoint mem {a:Type} {a_WT:WhyType a} (x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem x r)
  end.

Axiom mem_append : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l1:(list
  a)) (l2:(list a)), (mem x (infix_plpl l1 l2)) <-> ((mem x l1) \/ (mem x
  l2)).

Axiom mem_decomp : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l:(list
  a)), (mem x l) -> exists l1:(list a), exists l2:(list a),
  (l = (infix_plpl l1 (Cons x l2))).

(* Why3 assumption *)
Fixpoint reverse {a:Type} {a_WT:WhyType a} (l:(list a)) {struct l}: (list
  a) :=
  match l with
  | Nil => (Nil :(list a))
  | (Cons x r) => (infix_plpl (reverse r) (Cons x (Nil :(list a))))
  end.

Axiom reverse_append : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)) (x:a), ((infix_plpl (reverse (Cons x l1))
  l2) = (infix_plpl (reverse l1) (Cons x l2))).

Axiom reverse_reverse : forall {a:Type} {a_WT:WhyType a}, forall (l:(list
  a)), ((reverse (reverse l)) = l).

Axiom Reverse_length : forall {a:Type} {a_WT:WhyType a}, forall (l:(list a)),
  ((length (reverse l)) = (length l)).

(* Why3 assumption *)
Inductive option
  (a:Type) {a_WT:WhyType a} :=
  | None : option a
  | Some : a -> option a.
Axiom option_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (option a).
Existing Instance option_WhyType.
Implicit Arguments None [[a] [a_WT]].
Implicit Arguments Some [[a] [a_WT]].

Parameter nth: forall {a:Type} {a_WT:WhyType a}, Z -> (list a) -> (option a).

Axiom nth_def : forall {a:Type} {a_WT:WhyType a}, forall (n:Z) (l:(list a)),
  match l with
  | Nil => ((nth n l) = (None :(option a)))
  | (Cons x r) => ((n = 0%Z) -> ((nth n l) = (Some x))) /\ ((~ (n = 0%Z)) ->
      ((nth n l) = (nth (n - 1%Z)%Z r)))
  end.

Parameter m: Z.

Axiom m_positive : (0%Z < m)%Z.

Parameter n: Z.

Axiom n_nonnegative : (0%Z <= n)%Z.

(* Why3 assumption *)
Inductive shuffle{a:Type} {a_WT:WhyType a}  : (list a) -> (list a) -> (list
  a) -> Prop :=
  | Shuffle_nil_left : forall (l:(list a)), (shuffle l (Nil :(list a)) l)
  | Shuffle_nil_right : forall (l:(list a)), (shuffle (Nil :(list a)) l l)
  | Shuffle_cons_left : forall (x:a) (a1:(list a)) (b:(list a)) (c:(list a)),
      (shuffle a1 b c) -> (shuffle (Cons x a1) b (Cons x c))
  | Shuffle_cons_right : forall (x:a) (a1:(list a)) (b:(list a)) (c:(list
      a)), (shuffle a1 b c) -> (shuffle a1 (Cons x b) (Cons x c)).

Axiom shuffle_nil_nil_nil : forall {a:Type} {a_WT:WhyType a}, (shuffle
  (Nil :(list a)) (Nil :(list a)) (Nil :(list a))).

Axiom shuffle_sym : forall {a:Type} {a_WT:WhyType a}, forall (a1:(list a))
  (b:(list a)) (c:(list a)), (shuffle a1 b c) -> (shuffle b a1 c).

Axiom shuffle_length : forall {a:Type} {a_WT:WhyType a}, forall (a1:(list a))
  (b:(list a)) (c:(list a)), (shuffle a1 b c) ->
  (((length a1) + (length b))%Z = (length c)).

(* Why3 assumption *)
Definition suit_ordered (l:(list Z)): Prop := forall (i:Z) (j:Z),
  ((0%Z <= i)%Z /\ (i < n)%Z) -> (((0%Z <= j)%Z /\ (j < m)%Z) ->
  ((nth ((i * m)%Z + j)%Z l) = (Some j))).

(* Why3 assumption *)
Definition suit_sorted (l:(list Z)): Prop := (forall (i:Z) (v:Z), ((nth i
  l) = (Some v)) -> ((0%Z <= v)%Z /\ (v < m)%Z)) /\ forall (i:Z) (j1:Z)
  (j2:Z), ((0%Z <= i)%Z /\ (i < n)%Z) -> (((0%Z <= j1)%Z /\ (j1 < m)%Z) ->
  (((0%Z <= j2)%Z /\ (j2 < m)%Z) -> ~ ((nth ((i * m)%Z + j1)%Z
  l) = (nth ((i * m)%Z + j2)%Z l)))).

Axiom gilbreath_card_trick : forall (a:(list Z)), ((length a) = (n * m)%Z) ->
  ((suit_ordered a) -> forall (c:(list Z)) (d:(list Z)), (a = (infix_plpl c
  d)) -> forall (b:(list Z)), (shuffle c (reverse d) b) -> (suit_sorted b)).

(* Why3 assumption *)
Inductive t (a:Type) {a_WT:WhyType a} :=
  | mk_t : (list a) -> t a.
Axiom t_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (t a).
Existing Instance t_WhyType.
Implicit Arguments mk_t [[a] [a_WT]].

(* Why3 assumption *)
Definition elts {a:Type} {a_WT:WhyType a} (v:(t a)): (list a) :=
  match v with
  | (mk_t x) => x
  end.

(* Why3 assumption *)
Definition length1 {a:Type} {a_WT:WhyType a} (s:(t a)): Z :=
  (length (elts s)).

Require Import Why3.

(* Why3 goal *)
Theorem WP_parameter_shuffle : forall (b:(list Z)) (a:(list Z)),
  forall (c:(list Z)), (c = (Nil :(list Z))) -> forall (c1:(list Z))
  (b1:(list Z)) (a1:(list Z)), (exists a':(list Z), (exists b':(list Z),
  ((reverse a) = (infix_plpl (reverse a1) a')) /\
  (((reverse b) = (infix_plpl (reverse b1) b')) /\ (shuffle a' b' c1)))) ->
  forall (result:bool), ((result = true) <-> (a1 = (Nil :(list Z)))) ->
  ((~ (result = true)) -> forall (o:bool), ((o = true) <-> (a1 = (Nil :(list
  Z)))) -> ((~ (o = true)) -> forall (result1:bool), ((result1 = true) <->
  (b1 = (Nil :(list Z)))) -> ((~ (result1 = true)) -> forall (result2:bool),
  (~ (result2 = true)) -> forall (b2:(list Z)), forall (o1:Z),
  match b1 with
  | Nil => False
  | (Cons x t1) => (o1 = x) /\ (b2 = t1)
  end -> forall (c2:(list Z)), (c2 = (Cons o1 c1)) -> exists a':(list Z),
  exists b':(list Z), ((reverse a) = (infix_plpl (reverse a1) a')) /\
  (((reverse b) = (infix_plpl (reverse b2) b')) /\ (shuffle a' b' c2))))).
 intros b a c h1 c1 b1 a1 (a',(b',(h2,(h3,h4)))) result h5 h6 o h7 h8
   result1 h9 h10 result2 h11 b2 o1 h12 c2 h13. 
exists a'; exists (Cons o1 b').
why3 "Alt-Ergo,0.95.1,".
Qed.


