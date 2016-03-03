(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require int.Int.
Require int.Abs.
Require int.EuclideanDivision.
Require list.List.
Require list.Length.
Require list.Mem.
Require map.Map.
Require bool.Bool.
Require list.Append.

(* Why3 assumption *)
Definition unit := unit.

Axiom map : forall (a:Type) (b:Type), Type.
Parameter map_WhyType : forall (a:Type) {a_WT:WhyType a}
  (b:Type) {b_WT:WhyType b}, WhyType (map a b).
Existing Instance map_WhyType.

Parameter get: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  (map a b) -> a -> b.

Parameter set: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  (map a b) -> a -> b -> (map a b).

Axiom Select_eq : forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  forall (m:(map a b)), forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) ->
  ((get (set m a1 b1) a2) = b1).

Axiom Select_neq : forall {a:Type} {a_WT:WhyType a}
  {b:Type} {b_WT:WhyType b}, forall (m:(map a b)), forall (a1:a) (a2:a),
  forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1) a2) = (get m a2)).

(* Why3 assumption *)
Inductive id :=
  | Id : Z -> id.
Axiom id_WhyType : WhyType id.
Existing Instance id_WhyType.

(* Why3 assumption *)
Definition state := (map id Z).

(* Why3 assumption *)
Inductive aexpr :=
  | Anum : Z -> aexpr
  | Avar : id -> aexpr
  | Aadd : aexpr -> aexpr -> aexpr
  | Asub : aexpr -> aexpr -> aexpr
  | Amul : aexpr -> aexpr -> aexpr.
Axiom aexpr_WhyType : WhyType aexpr.
Existing Instance aexpr_WhyType.

(* Why3 assumption *)
Inductive bexpr :=
  | Btrue : bexpr
  | Bfalse : bexpr
  | Band : bexpr -> bexpr -> bexpr
  | Bnot : bexpr -> bexpr
  | Beq : aexpr -> aexpr -> bexpr
  | Ble : aexpr -> aexpr -> bexpr.
Axiom bexpr_WhyType : WhyType bexpr.
Existing Instance bexpr_WhyType.

(* Why3 assumption *)
Inductive com :=
  | Cskip : com
  | Cassign : id -> aexpr -> com
  | Cseq : com -> com -> com
  | Cif : bexpr -> com -> com -> com
  | Cwhile : bexpr -> com -> com.
Axiom com_WhyType : WhyType com.
Existing Instance com_WhyType.

(* Why3 assumption *)
Fixpoint aeval (st:(map id Z)) (e:aexpr) {struct e}: Z :=
  match e with
  | (Anum n) => n
  | (Avar x) => (get st x)
  | (Aadd e1 e2) => ((aeval st e1) + (aeval st e2))%Z
  | (Asub e1 e2) => ((aeval st e1) - (aeval st e2))%Z
  | (Amul e1 e2) => ((aeval st e1) * (aeval st e2))%Z
  end.

Parameter beval: (map id Z) -> bexpr -> bool.

Axiom beval_def : forall (st:(map id Z)) (b:bexpr),
  (match b with
  | Btrue => True
  | Bfalse => False
  | (Bnot b') => ((Init.Datatypes.negb (beval st b')) = true)
  | (Band b1 b2) => ((Init.Datatypes.andb (beval st b1) (beval st
      b2)) = true)
  | (Beq a1 a2) => ((aeval st a1) = (aeval st a2))
  | (Ble a1 a2) => ((aeval st a1) <= (aeval st a2))%Z
  end -> ((beval st b) = true)) /\
  ((~ match b with
  | Btrue => True
  | Bfalse => False
  | (Bnot b') => ((Init.Datatypes.negb (beval st b')) = true)
  | (Band b1 b2) => ((Init.Datatypes.andb (beval st b1) (beval st
      b2)) = true)
  | (Beq a1 a2) => ((aeval st a1) = (aeval st a2))
  | (Ble a1 a2) => ((aeval st a1) <= (aeval st a2))%Z
  end) -> ((beval st b) = false)).

(* Why3 assumption *)
Inductive ceval: (map id Z) -> com -> (map id Z) -> Prop :=
  | E_Skip : forall (m:(map id Z)), (ceval m Cskip m)
  | E_Ass : forall (m:(map id Z)) (a:aexpr) (x:id), (ceval m (Cassign x a)
      (set m x (aeval m a)))
  | E_Seq : forall (cmd1:com) (cmd2:com) (m0:(map id Z)) (m1:(map id Z))
      (m2:(map id Z)), (ceval m0 cmd1 m1) -> ((ceval m1 cmd2 m2) -> (ceval m0
      (Cseq cmd1 cmd2) m2))
  | E_IfTrue : forall (m0:(map id Z)) (m1:(map id Z)) (cond:bexpr) (cmd1:com)
      (cmd2:com), ((beval m0 cond) = true) -> ((ceval m0 cmd1 m1) -> (ceval
      m0 (Cif cond cmd1 cmd2) m1))
  | E_IfFalse : forall (m0:(map id Z)) (m1:(map id Z)) (cond:bexpr)
      (cmd1:com) (cmd2:com), (~ ((beval m0 cond) = true)) -> ((ceval m0 cmd2
      m1) -> (ceval m0 (Cif cond cmd1 cmd2) m1))
  | E_WhileEnd : forall (cond:bexpr) (m:(map id Z)) (body:com), (~ ((beval m
      cond) = true)) -> (ceval m (Cwhile cond body) m)
  | E_WhileLoop : forall (mi:(map id Z)) (mj:(map id Z)) (mf:(map id Z))
      (cond:bexpr) (body:com), ((beval mi cond) = true) -> ((ceval mi body
      mj) -> ((ceval mj (Cwhile cond body) mf) -> (ceval mi (Cwhile cond
      body) mf))).

Axiom ceval_deterministic_aux : forall (c:com) (mi:(map id Z)) (mf1:(map id
  Z)), (ceval mi c mf1) -> forall (mf2:(map id Z)), (ceval mi c mf2) ->
  (mf1 = mf2).

Axiom ceval_deterministic : forall (c:com) (mi:(map id Z)) (mf1:(map id Z))
  (mf2:(map id Z)), (ceval mi c mf1) -> ((ceval mi c mf2) -> (mf1 = mf2)).

(* Why3 assumption *)
Definition pos := Z.

(* Why3 assumption *)
Definition stack := (list Z).

(* Why3 assumption *)
Inductive machine_state :=
  | VMS : Z -> (list Z) -> (map id Z) -> machine_state.
Axiom machine_state_WhyType : WhyType machine_state.
Existing Instance machine_state_WhyType.

(* Why3 assumption *)
Definition ofs := Z.

(* Why3 assumption *)
Inductive instr :=
  | Iconst : Z -> instr
  | Ivar : id -> instr
  | Isetvar : id -> instr
  | Ibranch : Z -> instr
  | Iadd : instr
  | Isub : instr
  | Imul : instr
  | Ibeq : Z -> instr
  | Ibne : Z -> instr
  | Ible : Z -> instr
  | Ibgt : Z -> instr
  | Ihalt : instr.
Axiom instr_WhyType : WhyType instr.
Existing Instance instr_WhyType.

(* Why3 assumption *)
Definition code := (list instr).

(* Why3 assumption *)
Inductive codeseq_at: (list instr) -> Z -> (list instr) -> Prop :=
  | codeseq_at_intro : forall (c1:(list instr)) (c2:(list instr))
      (c3:(list instr)), (codeseq_at
      (Init.Datatypes.app (Init.Datatypes.app c1 c2) c3)
      (list.Length.length c1) c2).

Axiom codeseq_at_app_right : forall (c:(list instr)) (c1:(list instr))
  (c2:(list instr)) (p:Z), (codeseq_at c p (Init.Datatypes.app c1 c2)) ->
  (codeseq_at c (p + (list.Length.length c1))%Z c2).

Axiom codeseq_at_app_left : forall (c:(list instr)) (c1:(list instr))
  (c2:(list instr)) (p:Z), (codeseq_at c p (Init.Datatypes.app c1 c2)) ->
  (codeseq_at c p c1).

(* Why3 assumption *)
Definition iconst (n:Z): (list instr) :=
  (Init.Datatypes.cons (Iconst n) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ivar (x:id): (list instr) :=
  (Init.Datatypes.cons (Ivar x) Init.Datatypes.nil).

(* Why3 assumption *)
Definition isetvar (x:id): (list instr) :=
  (Init.Datatypes.cons (Isetvar x) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ibeq (ofs1:Z): (list instr) :=
  (Init.Datatypes.cons (Ibeq ofs1) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ible (ofs1:Z): (list instr) :=
  (Init.Datatypes.cons (Ible ofs1) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ibne (ofs1:Z): (list instr) :=
  (Init.Datatypes.cons (Ibne ofs1) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ibgt (ofs1:Z): (list instr) :=
  (Init.Datatypes.cons (Ibgt ofs1) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ibranch (ofs1:Z): (list instr) :=
  (Init.Datatypes.cons (Ibranch ofs1) Init.Datatypes.nil).

(* Why3 assumption *)
Inductive transition: (list instr) -> machine_state -> machine_state ->
  Prop :=
  | trans_const : forall (c:(list instr)) (p:Z) (n:Z), (codeseq_at c p
      (iconst n)) -> forall (s:(list Z)) (m:(map id Z)), (transition c (VMS p
      s m) (VMS (p + 1%Z)%Z (Init.Datatypes.cons n s) m))
  | trans_var : forall (c:(list instr)) (p:Z) (x:id), (codeseq_at c p
      (ivar x)) -> forall (s:(list Z)) (m:(map id Z)), (transition c (VMS p s
      m) (VMS (p + 1%Z)%Z (Init.Datatypes.cons (get m x) s) m))
  | trans_set_var : forall (c:(list instr)) (p:Z) (x:id), (codeseq_at c p
      (isetvar x)) -> forall (n:Z) (s:(list Z)) (m:(map id Z)), (transition c
      (VMS p (Init.Datatypes.cons n s) m) (VMS (p + 1%Z)%Z s (set m x n)))
  | trans_add : forall (c:(list instr)) (p:Z), (codeseq_at c p
      (Init.Datatypes.cons Iadd Init.Datatypes.nil)) -> forall (n1:Z) (n2:Z)
      (s:(list Z)) (m:(map id Z)), (transition c (VMS p
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m) (VMS (p + 1%Z)%Z
      (Init.Datatypes.cons (n1 + n2)%Z s) m))
  | trans_sub : forall (c:(list instr)) (p:Z), (codeseq_at c p
      (Init.Datatypes.cons Isub Init.Datatypes.nil)) -> forall (n1:Z) (n2:Z)
      (s:(list Z)) (m:(map id Z)), (transition c (VMS p
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m) (VMS (p + 1%Z)%Z
      (Init.Datatypes.cons (n1 - n2)%Z s) m))
  | trans_mul : forall (c:(list instr)) (p:Z), (codeseq_at c p
      (Init.Datatypes.cons Imul Init.Datatypes.nil)) -> forall (n1:Z) (n2:Z)
      (s:(list Z)) (m:(map id Z)), (transition c (VMS p
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m) (VMS (p + 1%Z)%Z
      (Init.Datatypes.cons (n1 * n2)%Z s) m))
  | trans_beq : forall (c:(list instr)) (p1:Z) (ofs1:Z), (codeseq_at c p1
      (ibeq ofs1)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (n1 = n2) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS ((p1 + 1%Z)%Z + ofs1)%Z s m))
  | trans_beq1 : forall (c:(list instr)) (p1:Z) (ofs1:Z), (codeseq_at c p1
      (ibeq ofs1)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (~ (n1 = n2)) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS (p1 + 1%Z)%Z s m))
  | trans_bne : forall (c:(list instr)) (p1:Z) (ofs1:Z), (codeseq_at c p1
      (ibne ofs1)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (n1 = n2) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS (p1 + 1%Z)%Z s m))
  | trans_bne1 : forall (c:(list instr)) (p1:Z) (ofs1:Z), (codeseq_at c p1
      (ibne ofs1)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (~ (n1 = n2)) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS ((p1 + 1%Z)%Z + ofs1)%Z s m))
  | trans_ble : forall (c:(list instr)) (p1:Z) (ofs1:Z), (codeseq_at c p1
      (ible ofs1)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (n1 <= n2)%Z -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS ((p1 + 1%Z)%Z + ofs1)%Z s m))
  | trans_ble1 : forall (c:(list instr)) (p1:Z) (ofs1:Z), (codeseq_at c p1
      (ible ofs1)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (~ (n1 <= n2)%Z) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS (p1 + 1%Z)%Z s m))
  | trans_bgt : forall (c:(list instr)) (p1:Z) (ofs1:Z), (codeseq_at c p1
      (ibgt ofs1)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (n1 <= n2)%Z -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS (p1 + 1%Z)%Z s m))
  | trans_bgt1 : forall (c:(list instr)) (p1:Z) (ofs1:Z), (codeseq_at c p1
      (ibgt ofs1)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (~ (n1 <= n2)%Z) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS ((p1 + 1%Z)%Z + ofs1)%Z s m))
  | trans_branch : forall (c:(list instr)) (p:Z) (ofs1:Z), (codeseq_at c p
      (ibranch ofs1)) -> forall (s:(list Z)) (m:(map id Z)), (transition c
      (VMS p s m) (VMS ((p + 1%Z)%Z + ofs1)%Z s m)).

(* Why3 assumption *)
Inductive transition_star: (list instr) -> machine_state -> machine_state ->
  Prop :=
  | Refl : forall (p:(list instr)) (x:machine_state), (transition_star p x x)
  | Step : forall (p:(list instr)) (x:machine_state) (y:machine_state)
      (z:machine_state), (transition p x y) -> ((transition_star p y z) ->
      (transition_star p x z)).

Axiom transition_star_one : forall (p:(list instr)) (s1:machine_state)
  (s2:machine_state), (transition p s1 s2) -> (transition_star p s1 s2).

Axiom transition_star_transitive : forall (p:(list instr)) (s1:machine_state)
  (s2:machine_state) (s3:machine_state), (transition_star p s1 s2) ->
  ((transition_star p s2 s3) -> (transition_star p s1 s3)).

(* Why3 assumption *)
Definition vm_terminates (c:(list instr)) (mi:(map id Z)) (mf:(map id
  Z)): Prop := exists p:Z, (codeseq_at c p
  (Init.Datatypes.cons Ihalt Init.Datatypes.nil)) /\ (transition_star c
  (VMS 0%Z Init.Datatypes.nil mi) (VMS p Init.Datatypes.nil mf)).

(* Why3 assumption *)
Definition fst {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b} (p:(a*
  b)%type): a := match p with
  | (x, _) => x
  end.

(* Why3 assumption *)
Definition snd {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b} (p:(a*
  b)%type): b := match p with
  | (_, y) => y
  end.

(* Why3 assumption *)
Definition pred := (machine_state -> bool).

(* Why3 assumption *)
Definition rel := (machine_state -> (machine_state -> bool)).

(* Why3 assumption *)
Definition pre (a:Type) := (a -> (Z -> (machine_state -> bool))).

(* Why3 assumption *)
Definition post (a:Type) := (a -> (Z -> (machine_state -> (machine_state ->
  bool)))).

(* Why3 assumption *)
Inductive hl
  (a:Type) :=
  | mk_hl : (list instr) -> (a -> (Z -> (machine_state -> bool))) -> (a ->
      (Z -> (machine_state -> (machine_state -> bool)))) -> hl a.
Axiom hl_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (hl a).
Existing Instance hl_WhyType.
Implicit Arguments mk_hl [[a]].

(* Why3 assumption *)
Definition post1 {a:Type} {a_WT:WhyType a} (v:(hl a)): (a -> (Z ->
  (machine_state -> (machine_state -> bool)))) :=
  match v with
  | (mk_hl x x1 x2) => x2
  end.

(* Why3 assumption *)
Definition pre1 {a:Type} {a_WT:WhyType a} (v:(hl a)): (a -> (Z ->
  (machine_state -> bool))) := match v with
  | (mk_hl x x1 x2) => x1
  end.

(* Why3 assumption *)
Definition code1 {a:Type} {a_WT:WhyType a} (v:(hl a)): (list instr) :=
  match v with
  | (mk_hl x x1 x2) => x
  end.

(* Why3 assumption *)
Definition wp_trans (a:Type) := (a -> (Z -> ((machine_state -> bool) ->
  (machine_state -> bool)))).

(* Why3 assumption *)
Inductive wp
  (a:Type) :=
  | mk_wp : (list instr) -> (a -> (Z -> ((machine_state -> bool) ->
      (machine_state -> bool)))) -> wp a.
Axiom wp_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (wp a).
Existing Instance wp_WhyType.
Implicit Arguments mk_wp [[a]].

(* Why3 assumption *)
Definition wp1 {a:Type} {a_WT:WhyType a} (v:(wp a)): (a -> (Z ->
  ((machine_state -> bool) -> (machine_state -> bool)))) :=
  match v with
  | (mk_wp x x1) => x1
  end.

(* Why3 assumption *)
Definition wcode {a:Type} {a_WT:WhyType a} (v:(wp a)): (list instr) :=
  match v with
  | (mk_wp x x1) => x
  end.

(* Why3 assumption *)
Definition contextual_irrelevance (c:(list instr)) (p:Z) (ms1:machine_state)
  (ms2:machine_state): Prop := forall (c_glob:(list instr)), (codeseq_at
  c_glob p c) -> (transition_star c_glob ms1 ms2).

(* Why3 assumption *)
Definition hl_correctness {a:Type} {a_WT:WhyType a} (cs:(hl a)): Prop :=
  forall (x:a) (p:Z) (ms:machine_state), (((((pre1 cs) x) p) ms) = true) ->
  exists ms':machine_state, ((((((post1 cs) x) p) ms) ms') = true) /\
  (contextual_irrelevance (code1 cs) p ms ms').

(* Why3 assumption *)
Definition wp_correctness {a:Type} {a_WT:WhyType a} (code2:(wp a)): Prop :=
  forall (x:a) (p:Z) (post2:(machine_state -> bool)) (ms:machine_state),
  ((((((wp1 code2) x) p) post2) ms) = true) -> exists ms':machine_state,
  ((post2 ms') = true) /\ (contextual_irrelevance (wcode code2) p ms ms').

(* Why3 assumption *)
Definition seq_wp {a:Type} {a_WT:WhyType a} (l1:Z) (w1:(a -> (Z ->
  ((machine_state -> bool) -> (machine_state -> bool))))) (w2:((a*
  machine_state)%type -> (Z -> ((machine_state -> bool) -> (machine_state ->
  bool))))): (a -> (Z -> ((machine_state -> bool) -> (machine_state ->
  bool)))) := fun (x:a) (p:Z) (q:(machine_state -> bool))
  (ms:machine_state) => ((((w1 x) p) (((w2 (x, ms)) (p + l1)%Z) q)) ms).

Axiom seq_wp_lemma : forall {a:Type} {a_WT:WhyType a}, forall (l1:Z)
  (w1:(a -> (Z -> ((machine_state -> bool) -> (machine_state -> bool)))))
  (w2:((a* machine_state)%type -> (Z -> ((machine_state -> bool) ->
  (machine_state -> bool))))) (x:a) (p:Z) (q:(machine_state -> bool))
  (ms:machine_state), ((((((seq_wp l1 w1 w2) x) p) q) ms) = ((((w1 x) p)
  (((w2 (x, ms)) (p + l1)%Z) q)) ms)).

Parameter fork_wp: forall {a:Type} {a_WT:WhyType a}, (a -> (Z ->
  ((machine_state -> bool) -> (machine_state -> bool)))) -> (a -> (Z ->
  (machine_state -> bool))) -> (a -> (Z -> ((machine_state -> bool) ->
  (machine_state -> bool)))).

Axiom fork_wp_def : forall {a:Type} {a_WT:WhyType a}, forall (w:(a -> (Z ->
  ((machine_state -> bool) -> (machine_state -> bool))))) (cond:(a -> (Z ->
  (machine_state -> bool)))) (x:a) (p:Z) (q:(machine_state -> bool))
  (ms:machine_state), ((((((fork_wp w cond) x) p) q) ms) = true) <->
  (((~ ((((cond x) p) ms) = true)) -> ((q ms) = true)) /\ (((((cond x) p)
  ms) = true) -> (((((w x) p) q) ms) = true))).

Axiom fork_wp_lemma : forall {a:Type} {a_WT:WhyType a}, forall (w:(a -> (Z ->
  ((machine_state -> bool) -> (machine_state -> bool))))) (cond:(a -> (Z ->
  (machine_state -> bool)))) (x:a) (p:Z) (q:(machine_state -> bool))
  (ms:machine_state), ((((((fork_wp w cond) x) p) q) ms) = true) <->
  (((~ ((((cond x) p) ms) = true)) -> ((q ms) = true)) /\ (((((cond x) p)
  ms) = true) -> (((((w x) p) q) ms) = true))).

Parameter towp_wp: forall {a:Type} {a_WT:WhyType a}, (a -> (Z ->
  (machine_state -> bool))) -> (a -> (Z -> (machine_state ->
  (machine_state -> bool)))) -> (a -> (Z -> ((machine_state -> bool) ->
  (machine_state -> bool)))).

Axiom towp_wp_def : forall {a:Type} {a_WT:WhyType a}, forall (pr:(a -> (Z ->
  (machine_state -> bool)))) (ps:(a -> (Z -> (machine_state ->
  (machine_state -> bool))))) (x:a) (p:Z) (q:(machine_state -> bool))
  (ms:machine_state), ((((((towp_wp pr ps) x) p) q) ms) = true) <-> (((((pr
  x) p) ms) = true) /\ forall (ms':machine_state), (((((ps x) p) ms)
  ms') = true) -> ((q ms') = true)).

Axiom towp_wp_lemma : forall {a:Type} {a_WT:WhyType a}, forall (pr:(a ->
  (Z -> (machine_state -> bool)))) (ps:(a -> (Z -> (machine_state ->
  (machine_state -> bool))))) (x:a) (p:Z) (q:(machine_state -> bool))
  (ms:machine_state), ((((((towp_wp pr ps) x) p) q) ms) = true) <-> (((((pr
  x) p) ms) = true) /\ forall (ms':machine_state), (((((ps x) p) ms)
  ms') = true) -> ((q ms') = true)).

Parameter trivial_pre: forall {a:Type} {a_WT:WhyType a}, (a -> (Z ->
  (machine_state -> bool))).

Axiom trivial_pre_def : forall {a:Type} {a_WT:WhyType a}, forall (us:a) (p:Z)
  (ms:machine_state), (((((trivial_pre : (a -> (Z -> (machine_state ->
  bool)))) us) p) ms) = true) <-> match ms with
  | (VMS p' _ _) => (p = p')
  end.

(* Why3 assumption *)
Inductive acc {a:Type} {a_WT:WhyType a}: (a -> (a -> bool)) -> a -> Prop :=
  | Acc : forall (r:(a -> (a -> bool))) (x:a), (forall (y:a), (((r y)
      x) = true) -> (acc r y)) -> (acc r x).

Parameter loop_progress: forall {a:Type} {a_WT:WhyType a}, (a -> (Z ->
  (machine_state -> bool))) -> (a -> (Z -> (machine_state -> bool))) -> (a ->
  (Z -> (machine_state -> (machine_state -> bool)))) -> (a -> (Z ->
  (machine_state -> (machine_state -> bool)))).

Axiom loop_progress_def : forall {a:Type} {a_WT:WhyType a}, forall (inv:(a ->
  (Z -> (machine_state -> bool)))) (post2:(a -> (Z -> (machine_state ->
  bool)))) (var:(a -> (Z -> (machine_state -> (machine_state -> bool)))))
  (x:a) (p:Z) (ms:machine_state) (ms':machine_state), ((((((loop_progress inv
  post2 var) x) p) ms) ms') = true) <-> ((((((inv x) p) ms') = true) /\
  (((((var x) p) ms') ms) = true)) \/ ((((post2 x) p) ms') = true)).

(* Why3 assumption *)
Definition forget_old {a:Type} {a_WT:WhyType a} (post2:(a -> (Z ->
  (machine_state -> bool)))): (a -> (Z -> (machine_state -> (machine_state ->
  bool)))) := fun (x:a) (p:Z) (us:machine_state) => ((post2 x) p).

Parameter ifun_post: forall {a:Type} {a_WT:WhyType a}, (machine_state ->
  machine_state) -> (a -> (Z -> (machine_state -> (machine_state -> bool)))).

Axiom ifun_post_def : forall {a:Type} {a_WT:WhyType a},
  forall (f:(machine_state -> machine_state)) (us:a) (us1:Z)
  (ms:machine_state) (ms':machine_state), ((((((ifun_post f: (a -> (Z ->
  (machine_state -> (machine_state -> bool))))) us) us1) ms) ms') = true) <->
  (ms' = (f ms)).

Parameter iconst_post: forall {a:Type} {a_WT:WhyType a}, Z -> (a -> (Z ->
  (machine_state -> (machine_state -> bool)))).

Axiom iconst_post_def : forall {a:Type} {a_WT:WhyType a}, forall (n:Z) (us:a)
  (p:Z) (ms:machine_state) (ms':machine_state), ((((((iconst_post n: (a ->
  (Z -> (machine_state -> (machine_state -> bool))))) us) p) ms)
  ms') = true) <-> forall (s:(list Z)) (m:(map id Z)), (ms = (VMS p s m)) ->
  (ms' = (VMS (p + 1%Z)%Z (Init.Datatypes.cons n s) m)).

(* Why3 assumption *)
Definition iconst_fun (n:Z): (machine_state -> machine_state) :=
  fun (ms:machine_state) =>
  match ms with
  | (VMS p s m) => (VMS (p + 1%Z)%Z (Init.Datatypes.cons n s) m)
  end.

Parameter ivar_post: forall {a:Type} {a_WT:WhyType a}, id -> (a -> (Z ->
  (machine_state -> (machine_state -> bool)))).

Axiom ivar_post_def : forall {a:Type} {a_WT:WhyType a}, forall (x:id) (us:a)
  (p:Z) (ms:machine_state) (ms':machine_state), ((((((ivar_post x: (a ->
  (Z -> (machine_state -> (machine_state -> bool))))) us) p) ms)
  ms') = true) <-> forall (s:(list Z)) (m:(map id Z)), (ms = (VMS p s m)) ->
  (ms' = (VMS (p + 1%Z)%Z (Init.Datatypes.cons (get m x) s) m)).

(* Why3 assumption *)
Definition ivar_fun (x:id): (machine_state -> machine_state) :=
  fun (ms:machine_state) =>
  match ms with
  | (VMS p s m) => (VMS (p + 1%Z)%Z (Init.Datatypes.cons (get m x) s) m)
  end.

(* Why3 assumption *)
Definition binop := (Z -> (Z -> Z)).

Parameter ibinop_pre: forall {a:Type} {a_WT:WhyType a}, (a -> (Z ->
  (machine_state -> bool))).

Axiom ibinop_pre_def : forall {a:Type} {a_WT:WhyType a}, forall (us:a) (p:Z)
  (ms:machine_state), (((((ibinop_pre : (a -> (Z -> (machine_state ->
  bool)))) us) p) ms) = true) <-> exists n1:Z, exists n2:Z,
  exists s:(list Z), exists m:(map id Z), (ms = (VMS p
  (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)).

Parameter ibinop_post: forall {a:Type} {a_WT:WhyType a}, (Z -> (Z -> Z)) ->
  (a -> (Z -> (machine_state -> (machine_state -> bool)))).

Axiom ibinop_post_def : forall {a:Type} {a_WT:WhyType a}, forall (op:(Z ->
  (Z -> Z))) (us:a) (p:Z) (ms:machine_state) (ms':machine_state),
  ((((((ibinop_post op: (a -> (Z -> (machine_state -> (machine_state ->
  bool))))) us) p) ms) ms') = true) <-> forall (n1:Z) (n2:Z) (s:(list Z))
  (m:(map id Z)), (ms = (VMS p
  (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)) ->
  (ms' = (VMS (p + 1%Z)%Z (Init.Datatypes.cons ((op n1) n2) s) m)).

(* Why3 assumption *)
Definition ibinop_fun (op:(Z -> (Z -> Z))): (machine_state ->
  machine_state) := fun (ms:machine_state) =>
  match ms with
  | (VMS p (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m) =>
      (VMS (p + 1%Z)%Z (Init.Datatypes.cons ((op n1) n2) s) m)
  | _ => ms
  end.

(* Why3 assumption *)
Definition plus: (Z -> (Z -> Z)) := fun (x:Z) (y:Z) => (x + y)%Z.

(* Why3 assumption *)
Definition sub: (Z -> (Z -> Z)) := fun (x:Z) (y:Z) => (x - y)%Z.

(* Why3 assumption *)
Definition mul: (Z -> (Z -> Z)) := fun (x:Z) (y:Z) => (x * y)%Z.

Parameter inil_post: forall {a:Type} {a_WT:WhyType a}, (a -> (Z ->
  (machine_state -> (machine_state -> bool)))).

Axiom inil_post_def : forall {a:Type} {a_WT:WhyType a}, forall (us:a) (us1:Z)
  (ms:machine_state) (ms':machine_state), ((((((inil_post : (a -> (Z ->
  (machine_state -> (machine_state -> bool))))) us) us1) ms) ms') = true) <->
  (ms = ms').

Parameter ibranch_post: forall {a:Type} {a_WT:WhyType a}, Z -> (a -> (Z ->
  (machine_state -> (machine_state -> bool)))).

Axiom ibranch_post_def : forall {a:Type} {a_WT:WhyType a}, forall (ofs1:Z)
  (us:a) (p:Z) (ms:machine_state) (ms':machine_state),
  ((((((ibranch_post ofs1: (a -> (Z -> (machine_state -> (machine_state ->
  bool))))) us) p) ms) ms') = true) <-> forall (s:(list Z)) (m:(map id Z)),
  (ms = (VMS p s m)) -> (ms' = (VMS ((p + 1%Z)%Z + ofs1)%Z s m)).

(* Why3 assumption *)
Definition ibranch_fun (ofs1:Z): (machine_state -> machine_state) :=
  fun (ms:machine_state) =>
  match ms with
  | (VMS p s m) => (VMS ((p + 1%Z)%Z + ofs1)%Z s m)
  end.

(* Why3 assumption *)
Definition cond := (Z -> (Z -> bool)).

Parameter icjump_post: forall {a:Type} {a_WT:WhyType a}, (Z -> (Z ->
  bool)) -> Z -> (a -> (Z -> (machine_state -> (machine_state -> bool)))).

Axiom icjump_post_def : forall {a:Type} {a_WT:WhyType a}, forall (cond1:(Z ->
  (Z -> bool))) (ofs1:Z) (us:a) (p:Z) (ms:machine_state) (ms':machine_state),
  ((((((icjump_post cond1 ofs1: (a -> (Z -> (machine_state ->
  (machine_state -> bool))))) us) p) ms) ms') = true) <-> forall (n1:Z)
  (n2:Z) (s:(list Z)) (m:(map id Z)), (ms = (VMS p
  (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)) -> (((((cond1 n1)
  n2) = true) -> (ms' = (VMS ((p + ofs1)%Z + 1%Z)%Z s m))) /\ ((~ (((cond1
  n1) n2) = true)) -> (ms' = (VMS (p + 1%Z)%Z s m)))).

Parameter icjump_fun: (Z -> (Z -> bool)) -> Z -> (machine_state ->
  machine_state).

Axiom icjump_fun_def : forall (cond1:(Z -> (Z -> bool))) (ofs1:Z)
  (ms:machine_state),
  match ms with
  | (VMS p (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m) =>
      ((((cond1 n1) n2) = true) -> (((icjump_fun cond1 ofs1)
      ms) = (VMS ((p + ofs1)%Z + 1%Z)%Z s m))) /\ ((~ (((cond1 n1)
      n2) = true)) -> (((icjump_fun cond1 ofs1) ms) = (VMS (p + 1%Z)%Z s m)))
  | _ => (((icjump_fun cond1 ofs1) ms) = ms)
  end.

Parameter beq: (Z -> (Z -> bool)).

Axiom beq_def : forall (x:Z) (y:Z), (((beq x) y) = true) <-> (x = y).

Parameter bne: (Z -> (Z -> bool)).

Axiom bne_def : forall (x:Z) (y:Z), (((bne x) y) = true) <-> ~ (x = y).

Parameter ble: (Z -> (Z -> bool)).

Axiom ble_def : forall (x:Z) (y:Z), (((ble x) y) = true) <-> (x <= y)%Z.

Parameter bgt: (Z -> (Z -> bool)).

Axiom bgt_def : forall (x:Z) (y:Z), (((bgt x) y) = true) <-> (y < x)%Z.

Parameter isetvar_pre: forall {a:Type} {a_WT:WhyType a}, (a -> (Z ->
  (machine_state -> bool))).

Axiom isetvar_pre_def : forall {a:Type} {a_WT:WhyType a}, forall (us:a) (p:Z)
  (ms:machine_state), (((((isetvar_pre : (a -> (Z -> (machine_state ->
  bool)))) us) p) ms) = true) <-> exists n:Z, exists s:(list Z),
  exists m:(map id Z), (ms = (VMS p (Init.Datatypes.cons n s) m)).

Parameter isetvar_post: forall {a:Type} {a_WT:WhyType a}, id -> (a -> (Z ->
  (machine_state -> (machine_state -> bool)))).

Axiom isetvar_post_def : forall {a:Type} {a_WT:WhyType a}, forall (x:id)
  (us:a) (p:Z) (ms:machine_state) (ms':machine_state),
  ((((((isetvar_post x: (a -> (Z -> (machine_state -> (machine_state ->
  bool))))) us) p) ms) ms') = true) <-> forall (s:(list Z)) (n:Z) (m:(map id
  Z)), (ms = (VMS p (Init.Datatypes.cons n s) m)) -> (ms' = (VMS (p + 1%Z)%Z
  s (set m x n))).

(* Why3 assumption *)
Definition isetvar_fun (x:id): (machine_state -> machine_state) :=
  fun (ms:machine_state) =>
  match ms with
  | (VMS p (Init.Datatypes.cons n s) m) => (VMS (p + 1%Z)%Z s (set m x n))
  | _ => ms
  end.

Parameter aexpr_post: forall {a:Type} {a_WT:WhyType a}, aexpr -> Z -> (a ->
  (Z -> (machine_state -> (machine_state -> bool)))).

Axiom aexpr_post_def : forall {a:Type} {a_WT:WhyType a}, forall (a1:aexpr)
  (len:Z) (us:a) (p:Z) (ms:machine_state) (ms':machine_state),
  ((((((aexpr_post a1 len: (a -> (Z -> (machine_state -> (machine_state ->
  bool))))) us) p) ms) ms') = true) <->
  match ms with
  | (VMS _ s m) => (ms' = (VMS (p + len)%Z (Init.Datatypes.cons (aeval m
      a1) s) m))
  end.

Parameter bexpr_post: forall {a:Type} {a_WT:WhyType a}, bexpr -> bool -> Z ->
  Z -> (a -> (Z -> (machine_state -> (machine_state -> bool)))).

Axiom bexpr_post_def : forall {a:Type} {a_WT:WhyType a}, forall (b:bexpr)
  (cond1:bool) (out_t:Z) (out_f:Z) (us:a) (p:Z) (ms:machine_state)
  (ms':machine_state), (((((((bexpr_post b cond1 out_t out_f: (a -> (Z ->
  (machine_state -> (machine_state -> bool))))) us) p) ms) ms') = true) ->
  match ms with
  | (VMS _ s m) => (((beval m b) = cond1) -> (ms' = (VMS (p + out_t)%Z s
      m))) /\ ((~ ((beval m b) = cond1)) -> (ms' = (VMS (p + out_f)%Z s m)))
  end) /\
  (match ms with
  | (VMS _ s m) => (((beval m b) = cond1) /\ (ms' = (VMS (p + out_t)%Z s
      m))) \/ ((~ ((beval m b) = cond1)) /\ (ms' = (VMS (p + out_f)%Z s m)))
  end -> ((((((bexpr_post b cond1 out_t out_f: (a -> (Z -> (machine_state ->
  (machine_state -> bool))))) us) p) ms) ms') = true)).

Parameter exec_cond: forall {a:Type} {a_WT:WhyType a}, bexpr -> bool -> (a ->
  (Z -> (machine_state -> bool))).

Axiom exec_cond_def : forall {a:Type} {a_WT:WhyType a}, forall (b1:bexpr)
  (cond1:bool) (us:a) (us1:Z) (ms:machine_state), (((((exec_cond b1
  cond1: (a -> (Z -> (machine_state -> bool)))) us) us1) ms) = true) <->
  match ms with
  | (VMS _ _ m) => ((beval m b1) = cond1)
  end.

Parameter com_pre: forall {a:Type} {a_WT:WhyType a}, com -> (a -> (Z ->
  (machine_state -> bool))).

Axiom com_pre_def : forall {a:Type} {a_WT:WhyType a}, forall (cmd:com) (us:a)
  (p:Z) (ms:machine_state), (((((com_pre cmd: (a -> (Z -> (machine_state ->
  bool)))) us) p) ms) = true) <->
  match ms with
  | (VMS p' _ m) => (p = p') /\ exists m':(map id Z), (ceval m cmd m')
  end.

Parameter com_post: forall {a:Type} {a_WT:WhyType a}, com -> Z -> (a -> (Z ->
  (machine_state -> (machine_state -> bool)))).

Axiom com_post_def : forall {a:Type} {a_WT:WhyType a}, forall (cmd:com)
  (len:Z) (us:a) (us1:Z) (ms:machine_state) (ms':machine_state),
  ((((((com_post cmd len: (a -> (Z -> (machine_state -> (machine_state ->
  bool))))) us) us1) ms) ms') = true) <->
  match ms with
  | (VMS p s m) =>
      match ms' with
      | (VMS p' s' m') => (p' = (p + len)%Z) /\ ((s' = s) /\ (ceval m cmd
          m'))
      end
  end.

Parameter exec_cond_old: forall {a:Type} {a_WT:WhyType a}, bexpr -> bool ->
  ((a* machine_state)%type -> (Z -> (machine_state -> bool))).

Axiom exec_cond_old_def : forall {a:Type} {a_WT:WhyType a}, forall (b1:bexpr)
  (cond1:bool) (x:(a* machine_state)%type) (us:Z) (us1:machine_state),
  (((((exec_cond_old b1 cond1: ((a* machine_state)%type -> (Z ->
  (machine_state -> bool)))) x) us) us1) = true) <->
  match (snd x) with
  | (VMS _ _ m) => ((beval m b1) = cond1)
  end.

Parameter loop_invariant: forall {a:Type} {a_WT:WhyType a}, com -> ((a*
  machine_state)%type -> (Z -> (machine_state -> bool))).

Axiom loop_invariant_def : forall {a:Type} {a_WT:WhyType a}, forall (c:com)
  (x:(a* machine_state)%type) (p:Z) (msi:machine_state),
  (((((loop_invariant c: ((a* machine_state)%type -> (Z -> (machine_state ->
  bool)))) x) p) msi) = true) <->
  match (snd x) with
  | (VMS _ s0 m0) =>
      match msi with
      | (VMS pi si mi) => (pi = p) /\ ((s0 = si) /\ exists mf:(map id Z),
          (ceval m0 c mf) /\ (ceval mi c mf))
      end
  end.

Parameter loop_post: forall {a:Type} {a_WT:WhyType a}, com -> Z -> ((a*
  machine_state)%type -> (Z -> (machine_state -> bool))).

Axiom loop_post_def : forall {a:Type} {a_WT:WhyType a}, forall (c:com)
  (len:Z) (x:(a* machine_state)%type) (p:Z) (msf:machine_state),
  (((((loop_post c len: ((a* machine_state)%type -> (Z -> (machine_state ->
  bool)))) x) p) msf) = true) <->
  match (snd x) with
  | (VMS _ s0 m0) =>
      match msf with
      | (VMS pf sf mf) => (pf = (p + len)%Z) /\ ((s0 = sf) /\ (ceval m0 c
          mf))
      end
  end.

Parameter loop_variant: forall {a:Type} {a_WT:WhyType a}, com -> bexpr ->
  (a -> (Z -> (machine_state -> (machine_state -> bool)))).

Axiom loop_variant_def : forall {a:Type} {a_WT:WhyType a}, forall (c:com)
  (test:bexpr) (us:a) (us1:Z) (msj:machine_state) (msi:machine_state),
  ((((((loop_variant c test: (a -> (Z -> (machine_state -> (machine_state ->
  bool))))) us) us1) msj) msi) = true) <->
  match msj with
  | (VMS pj sj mj) =>
      match msi with
      | (VMS pi si mi) => (pj = pi) /\ ((sj = si) /\ ((ceval mi c mj) /\
          ((beval mi test) = true)))
      end
  end.

Require Import Why3.
Ltac ae := why3 "Alt-Ergo,0.99.1," timelimit 5; admit.
Ltac cvc := why3 "CVC4,1.4," timelimit 5; admit.

(* Why3 goal *)
Theorem WP_parameter_compile_com : forall {a:Type} {a_WT:WhyType a},
  forall (cmd:com), forall (x:bexpr) (x1:com), (cmd = (Cwhile x x1)) ->
  forall (code_body:(list instr)) (code_body1:(((a* machine_state)%type*
  machine_state)%type -> (Z -> (machine_state -> bool)))) (code_body2:(((a*
  machine_state)%type* machine_state)%type -> (Z -> (machine_state ->
  (machine_state -> bool))))), let code_body3 := (mk_hl code_body code_body1
  code_body2) in ((((code_body1 = (com_pre x1: (((a* machine_state)%type*
  machine_state)%type -> (Z -> (machine_state -> bool))))) /\ (hl_correctness
  code_body3)) /\ (code_body2 = (com_post x1
  (list.Length.length code_body): (((a* machine_state)%type*
  machine_state)%type -> (Z -> (machine_state -> (machine_state ->
  bool))))))) -> let body_length :=
  ((list.Length.length code_body) + 1%Z)%Z in forall (code_test:(list instr))
  (code_test1:((a* machine_state)%type -> (Z -> (machine_state -> bool))))
  (code_test2:((a* machine_state)%type -> (Z -> (machine_state ->
  (machine_state -> bool))))), let code_test3 := (mk_hl code_test code_test1
  code_test2) in ((((code_test1 = (trivial_pre : ((a* machine_state)%type ->
  (Z -> (machine_state -> bool))))) /\ (hl_correctness code_test3)) /\
  (code_test2 = (bexpr_post x false
  ((list.Length.length code_test) + body_length)%Z
  (list.Length.length code_test): ((a* machine_state)%type -> (Z ->
  (machine_state -> (machine_state -> bool))))))) -> let ofs1 :=
  ((list.Length.length code_test) + body_length)%Z in forall (o:(list instr))
  (o1:((((a* machine_state)%type* machine_state)%type* machine_state)%type ->
  (Z -> (machine_state -> bool)))) (o2:((((a* machine_state)%type*
  machine_state)%type* machine_state)%type -> (Z -> (machine_state ->
  (machine_state -> bool))))), let o3 := (mk_hl o o1 o2) in
  ((((o1 = (trivial_pre : ((((a* machine_state)%type* machine_state)%type*
  machine_state)%type -> (Z -> (machine_state -> bool))))) /\
  (o2 = (ibranch_post (-ofs1)%Z: ((((a* machine_state)%type*
  machine_state)%type* machine_state)%type -> (Z -> (machine_state ->
  (machine_state -> bool))))))) /\ (((list.Length.length o) = 1%Z) /\
  (hl_correctness o3))) -> ((hl_correctness o3) -> forall (o4:(list instr))
  (o5:((((a* machine_state)%type* machine_state)%type* machine_state)%type ->
  (Z -> ((machine_state -> bool) -> (machine_state -> bool))))), let o6 :=
  (mk_wp o4 o5) in ((((list.Length.length o4) = (list.Length.length o)) /\
  ((o5 = (towp_wp o1 o2)) /\ (wp_correctness o6))) -> ((hl_correctness
  code_body3) -> forall (o7:(list instr)) (o8:(((a* machine_state)%type*
  machine_state)%type -> (Z -> ((machine_state -> bool) -> (machine_state ->
  bool))))), let o9 := (mk_wp o7 o8) in
  ((((list.Length.length o7) = (list.Length.length code_body)) /\
  ((o8 = (towp_wp code_body1 code_body2)) /\ (wp_correctness o9))) ->
  (((wp_correctness o9) /\ (wp_correctness o6)) -> forall (o10:(list instr))
  (o11:(((a* machine_state)%type* machine_state)%type -> (Z ->
  ((machine_state -> bool) -> (machine_state -> bool))))), let o12 :=
  (mk_wp o10 o11) in
  ((((list.Length.length o10) = ((list.Length.length o7) + (list.Length.length o4))%Z) /\
  ((o11 = (seq_wp (list.Length.length o7) o8 o5)) /\ (wp_correctness
  o12))) -> ((wp_correctness o12) -> forall (o13:(list instr)) (o14:(((a*
  machine_state)%type* machine_state)%type -> (Z -> ((machine_state ->
  bool) -> (machine_state -> bool))))), let o15 := (mk_wp o13 o14) in
  (((o14 = (fork_wp o11 (exec_cond x true: (((a* machine_state)%type*
  machine_state)%type -> (Z -> (machine_state -> bool)))))) /\
  (((list.Length.length o13) = (list.Length.length o10)) /\ (wp_correctness
  o15))) -> ((hl_correctness code_test3) -> forall (o16:(list instr))
  (o17:((a* machine_state)%type -> (Z -> ((machine_state -> bool) ->
  (machine_state -> bool))))), let o18 := (mk_wp o16 o17) in
  ((((list.Length.length o16) = (list.Length.length code_test)) /\
  ((o17 = (towp_wp code_test1 code_test2)) /\ (wp_correctness o18))) ->
  (((wp_correctness o18) /\ (wp_correctness o15)) ->
  forall (wp_while:(list instr)) (wp_while1:((a* machine_state)%type -> (Z ->
  ((machine_state -> bool) -> (machine_state -> bool))))), let wp_while2 :=
  (mk_wp wp_while wp_while1) in
  ((((list.Length.length wp_while) = ((list.Length.length o16) + (list.Length.length o13))%Z) /\
  ((wp_while1 = (seq_wp (list.Length.length o16) o17 o14)) /\ (wp_correctness
  wp_while2))) -> let inv := (loop_invariant cmd: ((a* machine_state)%type ->
  (Z -> (machine_state -> bool)))) in let var := (loop_variant x1 x: ((a*
  machine_state)%type -> (Z -> (machine_state -> (machine_state ->
  bool))))) in let o19 := (loop_progress inv (loop_post cmd ofs1: ((a*
  machine_state)%type -> (Z -> (machine_state -> bool)))) var) in
  (((wp_correctness wp_while2) /\ forall (x2:(a* machine_state)%type) (p:Z)
  (ms:machine_state), ((((inv x2) p) ms) = true) -> (((((wp_while1 x2) p)
  (((o19 x2) p) ms)) ms) = true)) -> forall (hl_while:(list instr))
  (hl_while1:((a* machine_state)%type -> (Z -> (machine_state -> bool))))
  (hl_while2:((a* machine_state)%type -> (Z -> (machine_state ->
  (machine_state -> bool))))), (((hl_while1 = inv) /\ (hl_while2 = o19)) /\
  (((list.Length.length hl_while) = (list.Length.length wp_while)) /\
  (hl_correctness (mk_hl hl_while hl_while1 hl_while2)))) -> forall (x2:(a*
  machine_state)%type) (p:Z) (ms:machine_state), ((((inv x2) p)
  ms) = true) -> (acc ((var x2) p) ms))))))))))))))))).
(* Why3 intros a a_WT cmd x x1 h1 code_body code_body1 code_body2 code_body3
        ((h2,h3),h4) body_length code_test code_test1 code_test2 code_test3
        ((h5,h6),h7) ofs1 o o1 o2 o3 ((h8,h9),(h10,h11)) h12 o4 o5 o6
        (h13,(h14,h15)) h16 o7 o8 o9 (h17,(h18,h19)) (h20,h21) o10 o11 o12
        (h22,(h23,h24)) h25 o13 o14 o15 (h26,(h27,h28)) h29 o16 o17 o18
        (h30,(h31,h32)) (h33,h34) wp_while wp_while1 wp_while2
        (h35,(h36,h37)) inv var o19 (h38,h39) hl_while hl_while1 hl_while2
        ((h40,h41),(h42,h43)) x2 p ms h44. *)
intros a a_WT cmd x x1 h1 code_body code_body1 code_body2 code_body3
((h2,h3),h4) body_length code_test code_test1 code_test2 code_test3
((h5,h6),h7) ofs o o1 o2 o3 ((h8,h9),(h10,h11)) h12 o4 o5 o6 (h13,(h14,h15))
h16 o7 o8 o9 (h17,(h18,h19)) (h20,h21) o10 o11 o12 (h22,(h23,h24)) h25 o13
o14 o15 (h26,(h27,h28)) h29 o16 o17 o18 (h30,(h31,h32)) (h33,h34) wp_while
wp_while1 wp_while2 (h35,(h36,h37)) inv var o19 (h38,h39) hl_while hl_while1
hl_while2 ((h40,h41),(h42,h43)) x2 p ms h44.
apply loop_invariant_def in h44.
destruct x2.
destruct m.
destruct ms.
simpl in *.
destruct h44.
destruct H0.
destruct H1 as [mf [ P T]].
induction T; try discriminate.
apply Acc.
intros.
apply loop_variant_def in H2.
exfalso. cvc.
apply Acc.
intros.
replace y with (VMS z0 l0 mj).
apply IHT2; trivial. 
apply loop_variant_def in H2.
destruct y.
assert (body = x1) by ae.
assert (m0 = mj).
eapply ceval_deterministic.
2: exact T1.
ae. ae.
Admitted.

