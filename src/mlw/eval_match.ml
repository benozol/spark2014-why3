(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2016   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Stdlib
open Ident
open Ty
open Term
open Decl
open Ity
open Expr
open Pdecl

(* helper functions for algebraic types *)

let ls_of_rs s = match s.rs_logic with
  RLls ls -> ls | _ -> assert false

let is_projection ls = ls.ls_constr = 0 &&
  try (restore_rs ls).rs_field <> None
  with Not_found -> false

let find_constructors_ts kn ({ts_name = id} as ts) =
  let rec find = function
    | {d_news = s}::dl when not (Mid.mem id s) -> find dl
    | {d_node = Ddata dl}::_ -> List.assq ts dl
    | _ -> [] in
  find (Mid.find id kn).pd_pure

let find_constructors kn ty = match ty.ty_node with
  | Tyapp (ts, _) -> find_constructors_ts kn ts
  | _ -> []

let find_logic_definition kn ({ls_name = id} as ls) =
  let rec find = function
    | {d_news = s}::dl when not (Mid.mem id s) -> find dl
    | {d_node = Dlogic dl}::_ -> Some (List.assq ls dl)
    | _ -> None in
  find (Mid.find id kn).pd_pure

let find_constructor_fields kn cs =
  let ty = Opt.get cs.ls_value in
  try List.assq cs (find_constructors kn ty)
  with Not_found -> assert false

let find_projection_field pj tl pjl =
  let rec find tl pjl = match tl, pjl with
    | t::_, Some ls::_ when ls_equal pj ls -> t
    | _::tl, _::pjl -> find tl pjl
    | _ -> assert false in
  find tl pjl

let apply_projection kn pj cs tl =
  find_projection_field pj tl (find_constructor_fields kn cs)

(* Part I - Invariant handling *)

let ls_valid =
  let v = create_tvsymbol (id_fresh "a") in
  create_psymbol (id_fresh "valid") [ty_var v]

let its_solid s =
  not s.its_fragile && (* no need to go any further *)
  List.for_all (fun f -> f.its_frozen) s.its_arg_flg &&
  List.for_all (fun f -> f.its_frozen) s.its_reg_flg

let is_fragile_constructor ls =
  ls.ls_constr > 0 &&
  match (Opt.get ls.ls_value).ty_node with
  | Tyapp (s,_) -> not (its_solid (restore_its s))
  | _ -> assert false

let is_fragile_projection ls =
  ls.ls_constr = 0 &&
  try let rs = restore_rs ls in
      if rs.rs_field = None then false else
      match (List.hd rs.rs_cty.cty_args).pv_ity.ity_node with
      | Ityreg {reg_its = s} | Ityapp (s,_,_) -> not (its_solid s)
      | _ -> assert false
  with Not_found -> false

(* Integer "points" represent individual values whose invariant
   may be broken. The special point 0 represents any value with
   verified invariant. Fresh points are assigned to values from
   top to bottom, so that a lesser point can never be reached
   from a greater point. Each point is associated to a list of
   fresh variables that correspond to the "temporary fields" of
   the point. Committing a point means that we prove that the
   temporary fields satisfy the invariant and then assume that
   the temporary fields are equal to the respective projections.

   Recursive "caps" represent deconstructible values from which
   points can be reached. Each variable is associated to a cap.
   A cap is either a committed value, a point (a non-committed
   record with a breakable invariant), a constructible value
   (characterized by the set of possible constructors), or
   a record with an unbreakable invariant. *)

type cap =
  | V                   (* valid *)
  | P of int            (* point *)
  | C of cap list Mls.t (* algebraic type *)
  | R of cap Mls.t      (* record with an unbreakable invariant *)

type point = {
  p_leaf   : term;                  (* term we can be reached from *)
  p_stem   : (term * pattern) list; (* deconstruction from a root *)
  p_fields : (vsymbol * cap) Mls.t; (* temporary fields *)
}

type binding =
  | W           (* valid point *)
  | B of point  (* broken point *)
  | L of int    (* link *)

type state = {
  s_roots  : cap Mvs.t;       (* non-broken roots may be unbound *)
  s_points : binding Mint.t;  (* non-broken points may be unbound *)
}

let new_point =
  let c = ref 0 in
  fun () -> incr c; !c

let rec get_point st n =
  match Mint.find_def W n st.s_points with
  | L n -> get_point st n
  | b -> n, b

let mkC css =
  let chk _ l = List.for_all (function V -> true | _ -> false) l in
  if Mls.for_all chk css then V else C css

let mkR pjs =
  let chk _ c = match c with V -> true | _ -> false in
  if Mls.for_all chk pjs then V else R pjs

(* TODO:
  - do not collapse on Eif and Ecase in k_expr when the type is fragile

  - projection application may require committing when the argument is
    a C with unjoinable caps. We can and should detect such applications
    inside specifications and produce appropriate commits. We should also
    avoid creating such applications outside stop_split: Vc.name_regions
    should NEVER use a projection with a fragile instantiated value type.
*)

let add_var kn st v =
  let rp = ref st.s_points in
  let rec down stem leaf ty = match ty.ty_node with
    | Tyvar _ -> V
    | Tyapp (s,tl) ->
        let s = restore_its s in
        if its_solid s then V else
        let d = find_its_defn kn s in
        let sbs = ts_match_args s.its_ts tl in
        if s.its_nonfree then if s.its_fragile then (* breakable record *)
          let rec name t = match t.t_node with
            | Tapp (pj,[t]) -> name t ^ "_" ^ pj.ls_name.id_string
            | Tvar v -> v.vs_name.id_string
            | _ -> assert false (* never *) in
          let bn = name leaf in
          let add_field m f =
            let ty = Ty.ty_inst sbs (Opt.get f.rs_field).pv_vs.vs_ty in
            let nm = bn ^ "_" ^ f.rs_name.id_string in
            let v = create_vsymbol (id_fresh nm) ty in
            Mls.add (ls_of_rs f) (v, down [] (t_var v) ty) m in
          let pjs = List.fold_left add_field Mls.empty d.itd_fields in
          let bd = B {p_leaf = leaf; p_stem = stem; p_fields = pjs} in
          let np = new_point () in
          rp := Mint.add np bd !rp;
          P np
        else (* unbreakable record *)
          let add_field m f =
            let pj = ls_of_rs f in
            let ty = Ty.ty_inst sbs (Opt.get f.rs_field).pv_vs.vs_ty in
            Mls.add pj (down stem (fs_app pj [leaf] ty) ty) m in
          mkR (List.fold_left add_field Mls.empty d.itd_fields)
        else (* constructible type *)
          let add_field m f = Mpv.add (Opt.get f.rs_field) (ls_of_rs f) m in
          let pjm = List.fold_left add_field Mpv.empty d.itd_fields in
          let add_constr m c =
            let cs = ls_of_rs c in
            let inst f = Ty.ty_inst sbs f.pv_vs.vs_ty in
            let tyl = List.map inst c.rs_cty.cty_args in
            let conv_field f ty_f =
              match Mpv.find_opt f pjm with
              | Some pj -> down stem (fs_app pj [leaf] ty_f) ty_f
              | None ->
                  let pat_arg ({pv_vs = v} as a) ty = if pv_equal a f
                    then pat_var (create_vsymbol (id_clone v.vs_name) ty)
                    else pat_wild ty in
                  let pl = List.map2 pat_arg c.rs_cty.cty_args tyl in
                  let pat = pat_app cs pl ty in
                  let v = Svs.choose pat.pat_vars in
                  down ((leaf, pat)::stem) (t_var v) ty_f in
            Mls.add cs (List.map2 conv_field c.rs_cty.cty_args tyl) m in
          mkC (List.fold_left add_constr Mls.empty d.itd_constructors)
  in
  match down [] (t_var v) v.vs_ty with
  | V -> st (* not broken *)
  | c -> { s_roots = Mvs.add v c st.s_roots; s_points = !rp }

let cap_valid st c =
  let rec down = function
    | V -> ()
    | P n -> if snd (get_point st n) <> W then raise Exit
    | C css -> Mls.iter (fun _ fl -> List.iter down fl) css
    | R pjs -> Mls.iter (fun _ c -> down c) pjs in
  try down c; true with Exit -> false

let add_pat st c p =
  let rec down rt c p =
    if c = V then rt else
    match p.pat_node with
    | Pwild -> rt
    | Pvar v -> Mvs.add v c rt
    | Papp (cs,pl) -> begin match c with
        | C css -> begin match Mls.find_opt cs css with
            | Some cl -> List.fold_left2 down rt cl pl
            | None -> rt (* all fields are committed *) end
        | _ -> assert false (* should never happen *) end
    | Por _ -> assert (cap_valid st c); rt
    | Pas (p,v) -> Mvs.add v c (down rt c p) in
  { st with s_roots = down st.s_roots c p }

let rec cap_join st c1 c2 = match c1, c2 with
  | V, c | c, V ->
      assert (cap_valid st c); V
  | P n1, P n2 ->
      let n1, b1 = get_point st n1 in
      let n2, b2 = get_point st n2 in
      if b1 = W then begin assert (b2 = W); V end
      else begin assert (n1 = n2); P n1 end
  | C s1, C s2 ->
      let join _ l1 l2 = Some (List.map2 (cap_join st) l1 l2) in
      mkC (Mls.union join s1 s2)
  | R s1, R s2 ->
      let join _ c1 c2 = Some (cap_join st c1 c2) in
      mkR (Mls.union join s1 s2)
  | _ -> assert false

let cap_of_term kn st t =
  let rec unroll t = function
    | (pj,t0)::pjl ->
        let t = t_app pj [t] t0.t_ty in
        unroll (t_label_copy t0 t) pjl
    | []  -> t in
  let rec unwind t c pjl0 = match c, pjl0 with
    | _, [] -> t, c
    | V, _ -> unroll t pjl0, V
    | P n, (pj,t0)::pjl ->
        begin match get_point st n with
        | _, L _ -> assert false (* never *)
        | _, W -> unroll t pjl0, V
        | _, B p ->
            let v, c = Mls.find pj p.p_fields in
            unwind (t_label_copy t0 (t_var v)) c pjl end
    | C css, (pj,t0)::pjl when Mls.cardinal css = 1 ->
        let cs, fl = Mls.choose css in
        let c = apply_projection kn pj cs fl in
        let t = t_app pj [t] t0.t_ty in
        unwind (t_label_copy t0 t) c pjl
    | C css, (pj,t0)::pjl ->
        let ty = Opt.get t.t_ty in
        let v0 = create_vsymbol (id_fresh "q") (Opt.get t0.t_ty) in
        let t0 = t_label_copy t0 (t_var v0) and p0 = pat_var v0 in
        let bb = match Mls.choose css with
          | {ls_constr = len}, _ when len > Mls.cardinal css ->
              let v = create_vsymbol (id_fresh "q") ty in
              [t_close_branch (pat_var v) (unroll (t_var v) pjl0)]
          | _ -> [] in
        let csl, sbs = match ty.ty_node with
          | Tyapp (ts,tl) ->
              find_constructors_ts kn ts, ts_match_args ts tl
          | _ -> assert false in
        let mk_branch cs fl =
          let fdl = List.assq cs csl in
          let mk_pat fd_ty fd = match fd with
            | Some ls when ls_equal pj ls -> p0
            | _ -> pat_wild (ty_inst sbs fd_ty) in
          let pl = List.map2 mk_pat cs.ls_args fdl in
          let c = find_projection_field pj fl fdl in
          let t0, c = unwind t0 c pjl in
          t_close_branch (pat_app cs pl ty) t0, c in
        let add_branch cs fl (bl, cj) =
          let b, c = mk_branch cs fl in
          b::bl, Some (match cj with
            | Some cj -> cap_join st c cj | None -> c) in
        let bl, c = Mls.fold add_branch css (bb, None) in
        t_case t bl, Opt.get c
    | R pjs, (pj,t0)::pjl ->
        let c = Mls.find pj pjs in
        let t = t_app pj [t] t0.t_ty in
        unwind (t_label_copy t0 t) c pjl
  in
  let rec down sr pjl t = match t.t_node with
    | Tvar v -> (* projection propagation *)
        unwind t (Mvs.find_def V v sr) pjl
    | Tconst _ -> (* constants are valid *)
        unroll t pjl, V
    | Tapp (ls,[t1;t2]) when ls_equal ls ps_equ ->
        let t1, c1 = down sr pjl t1 in
        let t2, c2 = down sr pjl t2 in
        ignore (cap_join st c1 c2);
        t_label_copy t (t_equ t1 t2), V
    | Tapp (ls,[t1]) when is_fragile_projection ls ->
        down sr ((ls,t)::pjl) t1
    | Tapp (ls,tl) when is_fragile_constructor ls ->
        begin match pjl with
        | (pj,t0)::pjl ->
            let t = apply_projection kn pj ls tl in
            down sr pjl (t_label_copy t0 t)
        | [] ->
            let tl, cl = List.split (List.map (down sr []) tl) in
            let t = t_label_copy t (t_app ls tl t.t_ty) in
            t, mkC (Mls.singleton ls cl) end
    | Tapp (ls,tl) ->
        let tl = List.map (fun t ->
          let t, c = down sr [] t in
          assert (cap_valid st c); t) tl in
        unroll (t_label_copy t (t_app ls tl t.t_ty)) pjl, V
    | Tif (t0,t1,t2) ->
        let t0, _  = down sr [] t0 in
        let t1, c1 = down sr pjl t1 in
        let t2, c2 = down sr pjl t2 in
        t_label_copy t (t_if t0 t1 t2), cap_join st c1 c2
    | Tlet (t0,tb) ->
        let t0, c0 = down sr [] t0 in
        let v, t1 = t_open_bound tb in
        let sr = Mvs.add v c0 sr in
        let t1, c1 = down sr pjl t1 in
        t_label_copy t (t_let_close v t0 t1), c1
    | Tcase (t0,bl) ->
        let t0, c0 = down sr [] t0 in
        let mk_branch b =
          let p, t1 = t_open_branch b in
          let st = add_pat {st with s_roots = sr} c0 p in
          let t1, c1 = down st.s_roots pjl t1 in
          t_close_branch p t1, c1 in
        let add_branch b (bl, cj) =
          let b, c = mk_branch b in
          b::bl, Some (match cj with
            | Some cj -> cap_join st c cj | None -> c) in
        let bl, c = List.fold_right add_branch bl ([], None) in
        t_label_copy t (t_case t0 bl), Opt.get c
    | Teps tb ->
        let v, f = t_open_bound tb in
        let f, _ = down sr [] f in
        unroll (t_label_copy t (t_eps_close v f)) pjl, V
    | Tquant (q,tq) ->
        let vl, tt, t0 = t_open_quant tq in
        let down t = fst (down sr [] t) in
        let tt = List.map (List.map down) tt in
        let tq = t_close_quant vl tt (down t0) in
        t_label_copy t (t_quant q tq), V
    | Tbinop (op,f1,f2) ->
        let f1, _ = down sr [] f1 in
        let f2, _ = down sr [] f2 in
        t_label_copy t (t_binary op f1 f2), V
    | Tnot f ->
        let f, _ = down sr [] f in
        t_label_copy t (t_not f), V
    | Ttrue | Tfalse ->
        t, V
  in
  down st.s_roots [] t

(* Part II - EvalMatch simplification *)

(* we destruct tuples, units, and singleton records *)
let destructible kn ty =
  match find_constructors kn ty with
  | [ls,_] when is_fs_tuple ls -> Some ls
  | [{ls_args = [_]} as ls, _] -> Some ls
  | [{ls_args = []}  as ls, _] -> Some ls
  | _ -> None

(* we inline projections of destructed types *)
let find_inlineable kn ls = match ls.ls_args with
  | [arg] when destructible kn arg <> None ->
      begin match find_logic_definition kn ls with
      | Some def ->
          let res = open_ls_defn def in
          begin match (snd res).t_node with
          | Tvar _ | Tconst _
          | Tapp (_, [{t_node = Tvar _}]) -> Some res
          | Tcase ({t_node = Tvar _}, [bt]) ->
              begin match t_open_branch bt with
              | _, {t_node = Tvar _} -> Some res
              | _ -> None end
          | _ -> None end
      | _ -> None end
  | _ -> None

let unfold_inlineable kn ls tl ty = match find_inlineable kn ls with
  | Some (vl, e) ->
      let mt = List.fold_left2 (fun mt x y ->
        ty_match mt x.vs_ty (t_type y)) Mtv.empty vl tl in
      let mv = List.fold_right2 Mvs.add vl tl Mvs.empty in
      Some (t_ty_subst (oty_match mt e.t_ty ty) mv e)
  | None -> None

let rec add_quant kn (vl,tl,f) ({vs_ty = ty} as v) =
  match destructible kn ty with
  | Some ls ->
      let s = ty_match Mtv.empty (Opt.get ls.ls_value) ty in
      let mk_v ty = create_vsymbol (id_clone v.vs_name) (ty_inst s ty) in
      let nvl = List.map mk_v ls.ls_args in
      let t = fs_app ls (List.map t_var nvl) ty in
      let f = t_let_close_simp v t f in
      let tl = tr_map (t_subst_single v t) tl in
      List.fold_left (add_quant kn) (vl,tl,f) nvl
  | None ->
      (v::vl, tl, f)

let let_map fn env t1 tb =
  let x,t2,close = t_open_bound_cb tb in
  let t2 = fn (Mvs.add x t1 env) t2 in
  (* TODO/FIXME? convert (let x = if f then True else False in h)
     into (forall x. (x = True <-> f) -> h) ? *)
  t_let_simp t1 (close x t2)

let branch_map fn env t1 bl =
  let mk_b b =
    let p,t2,close = t_open_branch_cb b in close p (fn env t2) in
  t_case t1 (List.map mk_b bl)

let rec dive_to_constructor fn env t =
  let dive env t = dive_to_constructor fn env t in
  t_label_copy t (match t.t_node with
    | Tvar x -> dive env (Mvs.find_exn Exit x env)
    | Tlet (t1,tb) -> let_map dive env t1 tb
    | Tcase (t1,bl) -> branch_map dive env t1 bl
    | Tif (f,t1,t2) -> t_if_simp f (dive env t1) (dive env t2)
    | Tapp (ls,tl) when ls.ls_constr > 0 -> fn env t ls tl
    | _ -> raise Exit)

let rec cs_equ env t1 t2 =
  if t_equal t1 t2 then t_true else
  let right cs1 tl1 env _t2 cs2 tl2 =
    if not (ls_equal cs1 cs2) then t_false else
    t_and_simp_l (List.map2 (cs_equ env) tl1 tl2) in
  let left t2 env _t1 cs1 tl1 =
    dive_to_constructor (right cs1 tl1) env t2 in
  try dive_to_constructor (left t2) env t1
  with Exit -> t_equ t1 t2

let flat_case t bl =
  let mk_b b = let p,t = t_open_branch b in [p],t in
  let mk_case = t_case_close and mk_let = t_let_close_simp in
  Pattern.compile_bare ~mk_case ~mk_let [t] (List.map mk_b bl)

let rec eval_match kn stop env t =
  let stop = stop || Slab.mem Term.stop_split t.t_label in
  let eval env t = eval_match kn stop env t in
  t_label_copy t (match t.t_node with
    | Tapp (ls, [t1;t2]) when ls_equal ls ps_equ ->
        cs_equ env (eval env t1) (eval env t2)
    | Tapp (ls, [t1]) when is_projection ls ->
        let t1 = eval env t1 in
        let fn _env _t2 cs tl = apply_projection kn ls cs tl in
        begin try dive_to_constructor fn env t1
        with Exit -> t_app ls [t1] t.t_ty end
    | Tapp (ls, tl) ->
        begin match unfold_inlineable kn ls tl t.t_ty with
        | Some t -> eval env t
        | None -> t_map (eval env) t
        end
    | Tlet (t1, tb2) ->
        let t1 = eval env t1 in
        let_map eval env t1 tb2
    | Tcase (t1, bl1) ->
        let t1 = eval env t1 in
        let fn env t2 _cs _tl =
          eval env (Loc.try2 ?loc:t.t_loc flat_case t2 bl1) in
        begin try dive_to_constructor fn env t1
        with Exit -> branch_map eval env t1 bl1 end
    | Tquant (q, qf) ->
        let vl,tl,f,close = t_open_quant_cb qf in
        let vl,tl,f = if stop then List.rev vl,tl,f else
          List.fold_left (add_quant kn) ([],tl,f) vl in
        t_quant_simp q (close (List.rev vl) tl (eval env f))
    | _ ->
        t_map (eval env) t)

let eval_match kn t = eval_match kn false Mvs.empty t
