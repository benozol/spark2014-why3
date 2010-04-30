Declare ML Module "whytac".
Require Export ZArith.
Open Scope Z_scope.
Require Export List.

Ltac ae := why "alt-ergo".
Ltac z3 := why "z3".
Ltac spass := why "spass".

(* type definitions *)

Definition t := list.

Inductive foo : Set :=
  C : t nat -> foo.

Goal forall x:foo, x=x.
intros.
ae.
Qed.

(* predicate definition *)

Definition p (x:nat) := x=O.

Goal p O.
spass.
Qed.

Definition eq (A:Set) (x y : A) := x=y.

Goal eq nat O O.
why "z3".
ae.
(*
why "z3".  (* BUG encoding decorate ici ? *)
Qed.
*)
Admitted.

Parameter mem : forall (A:Set), A -> list A -> Prop.

Definition q (A:Set) (x:A) (y:list A) := mem A x y.

Goal q nat O (cons O nil).
(*why.*)
Admitted.

(* function definition *)

Definition f (x:Z) (y:Z) := x+y.

Goal f 1 2 = 3.
ae.
Qed.

Definition id A (x:A) := x.

Goal id nat O = O.
spass.
Qed.

(* inductive types *)

Parameter P : (nat -> nat) -> Prop.

Goal forall (a:Set), forall x:nat, x=S O -> P S -> 
  let y := (S (S O)) in S x=y.
intros.
spass.
Qed.

Goal  forall (a:Set), forall x:Z, x=1 -> P S -> let y := 2 in x+1=y.
intros.
ae.
Qed.

Goal  forall x: list nat, x=x.
intros.
spass.
Qed.

(* Mutually inductive types *)

Inductive tree : Set :=
  | Leaf : tree
  | Node : Z -> forest -> tree

with forest : Set :=
  | Nil : forest
  | Cons : tree -> forest -> forest.

Goal forall x : tree, x=x.
spass.
Qed.

(* Polymorphic, Mutually inductive types *)

Inductive ptree (a:Set) : Set :=
  | PLeaf : ptree a
  | PNode : a -> pforest a -> ptree a

with pforest (a:Set) : Set :=
  | PNil : pforest a
  | PCons : ptree a -> pforest a -> pforest a.

Goal forall x : ptree Z, x=x.
spass.
Qed.

(* the same, without parameters *)

Inductive ptree' : Type -> Type :=
  | PLeaf' : forall (a:Type), ptree' a
  | PNode' : forall (a:Type), a -> pforest' a -> ptree' a

with pforest' : Type -> Type :=
  | PNil' : forall  (a:Type), pforest' a
  | PCons' : forall (a:Type), ptree' a -> pforest' a -> pforest' a.

Goal forall x : ptree' Z, x=x.
spass.
Qed.

