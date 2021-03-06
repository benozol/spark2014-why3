open Term
open Decl

let apply_append fn acc l =
  List.fold_left (fun l e -> fn e :: l) acc (List.rev l)

let rec split acc f =
  match f.t_node with
  | Ttrue ->
      f :: acc
  | Tfalse | Tapp _ | Tnot _ | Tquant (Texists, _) | Tbinop (Tor, _, _) ->
      f :: acc
  | Tbinop (Tand, f1, f2) ->
      split (split acc (t_attr_copy f f2)) (t_attr_copy f f1)
  | Tbinop (Timplies, f1, f2) ->
      let fn f2 = t_attr_copy f (t_implies f1 f2) in
      apply_append fn acc (split [] f2)
  | Tbinop (Tiff,f1,f2) ->
      let f12 = t_attr_copy f (t_implies f1 (t_attr_copy f f2)) in
      let f21 = t_attr_copy f (t_implies f2 (t_attr_copy f f1)) in
      split (split acc f21) f12
  | Tif (fif,fthen,felse) ->
      let fit = t_attr_copy f (t_implies fif fthen) in
      let fie = t_attr_copy f (t_implies (t_not fif) felse) in
      split (split acc fie) fit
  | Tlet (t,fb) ->
      let vs,f1,close = t_open_bound_cb fb in
      let fn f1 = t_attr_copy f (t_let t (close vs f1)) in
      apply_append fn acc (split [] f1)
  | Tcase (tl,bl) ->
      split_case f t_true acc tl bl
  | Tquant (Tforall,fq) ->
      let vsl,trl,f1,close = t_open_quant_cb fq in
      let fn f1 = t_attr_copy f (t_forall (close vsl trl f1)) in
      apply_append fn acc (split [] f1)
  | Tvar _ | Tconst _ | Teps _ -> raise (FmlaExpected f)

and split_case forig c acc tl bl =
  let bl = List.rev_map t_open_branch_cb bl in
  let bll,_ = List.fold_left (fun (bll,el) (pl,f,close) ->
    let spf = split [] f in
    let brc = close pl c in
    let bll = List.map (fun rl -> brc::rl) bll in
    let bll = apply_append (fun f -> close pl f :: el) bll spf in
    bll, brc::el) ([],[]) bl
  in
  let fn bl = t_attr_copy forig (t_case tl bl) in
  apply_append fn acc bll

let split_goal pr f =
  let make_prop f = [create_prop_decl Pgoal pr f] in
  let l = split [] f in
  List.map make_prop l

let split_conj = Trans.goal_l split_goal

let split_conj_name = "split_conj"
let () =
   Trans.register_transform_l split_conj_name split_conj
   ~desc:"Split conjunctions, equivalences, if-then-else and case in the goal,\
   on the right hand side, and only there."


let post_def_axiom_regexp = Re.Str.regexp ".*__\\(post\\|def\\)_axiom"

let match_axiom s = Re.Str.string_match post_def_axiom_regexp s 0

let rec merge_univ_quant t acc =
  match t.t_node with
  | Tquant (Tforall, tq) -> let vs, tr, t = t_open_quant tq in
                            if tr <> [] then
                              let t = t_quant Tforall (t_close_quant vs tr t)
                              in
                              t_quant Tforall (t_close_quant acc [] t)
                            else
                              merge_univ_quant t (acc @ vs)
  | _ -> t_quant Tforall (t_close_quant acc [] t)

let split_axioms d =
  match d.d_node with
  | Dprop (Paxiom, ({ pr_name = { Ident.id_string = s } } as prsym), term) ->
     if match_axiom s then
       let splitted_terms = split [] term in
       List.rev_map (fun t -> let t = merge_univ_quant t [] in
                              let name = create_prsymbol (Ident.id_fresh s) in
                              create_prop_decl Paxiom name t) splitted_terms
     else
       [ create_prop_decl Paxiom prsym (merge_univ_quant term []) ]
  | _ -> [ d ]

let split_conj_axioms = Trans.decl split_axioms None

let split_conj_axioms_name = "split_conj_axioms"


let () =
  Trans.register_transform split_conj_axioms_name split_conj_axioms
  ~desc:"Split def and post axioms generated by SPARK tools"

let () =
  let trans =
    Trans.compose_l Split_goal.split_goal_right split_conj in
  Trans.register_transform_l "split_goal_wp_conj" trans
    ~desc:"split goal followed by conjunction split"
