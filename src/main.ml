(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-                                                   *)
(*    Francois Bobot                                                      *)
(*    Jean-Christophe Filliatre                                           *)
(*    Johannes Kanig                                                      *)
(*    Andrei Paskevich                                                    *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format
open Theory
open Task
open Driver
open Trans

let files = Queue.create ()
let parse_only = ref false
let type_only = ref false
let debug = ref false
let loadpath = ref []
let driver = ref None
let set_all_goals = ref false
let which_theories = Hashtbl.create 8
let add_which_theories s = Hashtbl.replace which_theories s None 
let add_which_goals s = 
  let l = Str.split (Str.regexp "\\.") s in
  let tname, l = 
    match l with
      | [] | [_] -> 
          eprintf "--goal : Must be a qualified name (%s)@." s;
          exit 1
      | a::l -> a,l in
  try
    match Hashtbl.find which_theories tname with
      | None -> ()
      | Some goals -> Hashtbl.replace goals s l
  with Not_found ->   
    let goals = Hashtbl.create 4 in
    Hashtbl.replace goals s l;
    Hashtbl.replace which_theories tname (Some goals)

let timeout = ref 10
let call = ref false
let output_dir = ref None
let output_file = ref None
let list_printers = ref false
let list_transforms = ref false
(*let list_goals = ref false*)
let print_debug = ref false
let print_namespace = ref false

let () = 
  Arg.parse 
    ["--parse-only", Arg.Set parse_only, "stops after parsing";
     "--type-only", Arg.Set type_only, "stops after type-checking";
     "--debug", Arg.Set debug, "sets the debug flag";
     "-I", Arg.String (fun s -> loadpath := s :: !loadpath), 
     "<dir>  adds dir to the loadpath";
     "--all-goals", Arg.Set set_all_goals,
     "apply on all the goals of the file";
     "--goals-of", Arg.String (fun s -> add_which_theories s), 
     "apply on all the goals of the theory given (ex. --goal T)";
     "--goal", Arg.String (fun s -> add_which_goals s), 
     "apply only on the goal given (ex. --goal T.g)";
     "--output-dir", Arg.String (fun s -> output_dir := Some s), 
     "choose to output each goals in the given directory.";
     "--output-file", Arg.String (fun s -> output_file := Some s), 
     "choose to output the goals in the given file.\
(- for stdout) can't be used with --call";
     "--call", Arg.Set call, 
     "choose to call the prover on each goals.\
can't be used with --output-file";
     "--driver", Arg.String (fun s -> driver := Some s),
     "<file>  set the driver file";
     "--timeout", Arg.Set_int timeout, "set the timeout used when calling \
provers (0 unlimited, default 10)";
     "--list-printers", Arg.Set list_printers, "list the printers registered";
     "--list-transforms", Arg.Set list_transforms, "list the transformation \
registered";
(*     "--list-goals", Arg.Set list_goals, "list the goals of the files";*)
     "--print-debug", Arg.Set print_debug, "print on stderr the theories of \
the files given on the commandline"; 
     "--print-namespace", Arg.Set print_namespace, "print on stderr the \
namespaces for the files given on the command line";
     "-", Arg.Unit (fun () -> Queue.push "-" files), 
     "parse the file from stdin"
    ]
    (fun f -> Queue.push f files)
    "usage: why [options] files..."

let timeout = if !timeout <= 0 then None else Some !timeout

let () = 
  match !output_dir,!output_file,!call with
    | None,None,false -> type_only := true
(*    | _,Some _,true -> 
        eprintf "--output-file and --call can't be use at the same time.@.";
        exit 1*)
    | _ -> ()


(*
let transformation l = 
  let t1 = Simplify_recursive_definition.t in
  let t2 = Inlining.all in
  List.map (fun (t,c) ->
              let c = if !simplify_recursive 
              then Trans.apply t1 c
              else c in
              let c = if !inlining then Trans.apply t2 c
              else c in
              (t,c)) l
*)
let rec report fmt = function
  | Lexer.Error e ->
      fprintf fmt "lexical error: %a" Lexer.report e;
  | Loc.Located (loc, e) ->
      fprintf fmt "%a%a" Loc.report_position loc report e
  | Parsing.Parse_error ->
      fprintf fmt "syntax error"
  | Denv.Error e ->
      Denv.report fmt e
  | Typing.Error e ->
      Typing.report fmt e
  | Decl.UnknownIdent i ->
      fprintf fmt "anomaly: unknown ident %s" i.Ident.id_short
  | Driver.Error e ->
      Driver.report fmt e
  | Dynlink_compat.Dynlink.Error e -> 
      fprintf fmt "Dynlink : %s" (Dynlink_compat.Dynlink.error_message e)
  | e -> fprintf fmt "anomaly: %s" (Printexc.to_string e)

(*
let transform env l =
  let l = 
    List.map 
      (fun t -> t, use_export (init_task env) t) 
      l
  in
  (*let l = transformation l in*)
  if !print_stdout then 
    List.iter 
      (fun (t,task) -> Pretty.print_named_task
        std_formatter t.th_name.Ident.id_long task) l
  else match !driver with
    | None ->
	()
    | Some file ->

	begin match l with
	  | (_,task) :: _ -> begin match extract_goals task with
	      | g :: _ -> 
		  Driver.print_task drv std_formatter g
	      | [] -> 
		  eprintf "no goal@."
	    end
	  | [] -> ()
	end
*)


let extract_goals ?filter = 
  fun env drv acc th ->
    let l = split_theory th filter in
    let l = List.rev_map (fun task -> 
      let cl = Util.option_apply Ident.Mid.empty (fun t -> t.task_clone) task in
      let us = Util.option_apply Ident.Mid.empty (fun t -> t.task_used) task in
      let us = Ident.Mid.add th.th_name th us in
      let drv = Driver.cook_driver env cl us drv in (th,task,drv)) l in
    List.rev_append l acc

let file_sanitizer = None (* We should remove which character? *)
(*  Ident.sanitizer Ident.char_to_alnumus Ident.char_to_alnumus*)

let print_theory_namespace fmt th =
  Pretty.print_namespace fmt th.th_name.Ident.id_short th.th_export




let do_goals env drv src_filename_printer dest_filename_printer file goals = 
  (* Apply transformations *)
  let goals = List.fold_left 
    (fun acc (th,task,drv) -> 
       List.rev_append 
         (List.map (fun e -> (th,e,drv)) 
            (Driver.apply_transforms drv task)
         ) acc) [] goals 
  in
  (* Pretty-print the goals or call the prover *)
  begin
    match !output_dir with
      | None -> ()
      | Some dir (* we are in the output dir mode *) -> 
          let file = 
            let file = Filename.basename file in
            let file = 
              try
                Filename.chop_extension file 
              with Invalid_argument _ -> file in
            Ident.string_unique src_filename_printer file in
          List.iter 
            (fun (th,task,drv) ->
               let dest = 
                 Driver.filename_of_goal drv 
                   file th.th_name.Ident.id_short task in
               (* Uniquify the filename before the extension if it exists*)
               let i = 
                 try String.rindex dest '.' 
                 with Not_found -> String.length dest in
               let name,ext = String.sub dest 0 i, 
                 String.sub dest i (String.length dest- i) in
               let name = Ident.string_unique dest_filename_printer name in
               let filename = name^ext in
               let filename = Filename.concat dir filename in
               let cout = open_out filename in
               let fmt = formatter_of_out_channel cout in
               fprintf fmt "%a@?" (Driver.print_task drv) task;
               close_out cout) goals
  end;
  begin
    match !output_file with
      | None -> ()
      | Some file (* we are in the output file mode *) -> 
          let fmt = if file = "-" then std_formatter
          else formatter_of_out_channel (open_out file) 
          in
          let print_task fmt (th,task,drv) =
            fprintf fmt "@[%a@]" (Driver.print_task drv) task
          in
          let print_zero fmt () = fprintf fmt "\000@?" in
          fprintf fmt "%a@?" (Pp.print_list print_zero print_task) goals
  end;
  if !call then
    (* we are in the call mode *)
    let call (th,task,drv) = 
      let res = 
        Driver.call_prover ~debug:!debug ?timeout drv task in
      printf "%s %s %s : %a@." 
        file th.th_name.Ident.id_short 
        ((task_goal task).Decl.pr_name).Ident.id_long
        Call_provers.print_prover_result res in
    List.iter call goals



let do_no_file env drv src_filename_printer dest_filename_printer =
  let drv =  
    match drv with
      | None -> eprintf "a driver is needed@."; exit 1
      | Some drv -> drv in
  (* Extract the goal(s) *)
  Hashtbl.iter
    (fun tname goals ->
       let dir,file,th = match List.rev (Str.split (Str.regexp "\\.") tname) with
         | t::p -> List.rev p, List.hd p, t
         | _ -> assert false
       in
       let th = try Env.find_theory env dir th with Not_found -> 
         eprintf "--goal/--goals_of : Unknown theory %s@." 
           tname; exit 1 
       in                
       let filter = match goals with
         | None -> None
         | Some s -> Some 
             (Hashtbl.fold 
                (fun s l acc ->
                   let pr = try ns_find_pr th.th_export l 
                   with Not_found ->
                     eprintf "--goal : Unknown goal %s@." s ; exit 1 in
                   Decl.Spr.add pr acc
                ) s Decl.Spr.empty) in
       let goals = extract_goals ?filter env drv [] th in
       do_goals env drv src_filename_printer dest_filename_printer file goals) 
    which_theories 
  

let do_file env drv src_filename_printer dest_filename_printer file =
  let file,cin = 
    if file = "-" 
    then "stdin",stdin
    else file, open_in file 
  in
  if !parse_only then begin
    let lb = Lexing.from_channel cin in
    Loc.set_file file lb;
    let _ = Lexer.parse_logic_file lb in 
    close_in cin
  end else begin
    let m = Typing.read_channel env file cin in
    close_in cin;
    if !print_debug then
      eprintf "%a@."
        (Pp.print_iter2 Mnm.iter Pp.newline Pp.nothing Pp.nothing 
           Pretty.print_theory)
        m;
    if !print_namespace then
      eprintf "%a@."
        (Pp.print_iter2 Mnm.iter Pp.newline Pp.nothing Pp.nothing 
           print_theory_namespace)
        m;
    if not !type_only then
      let drv =  
        match drv with
          | None -> eprintf "a driver is needed@."; exit 1
          | Some drv -> drv in
      (* Extract the goal(s) *)
      let goals = 
        if !set_all_goals 
        then Mnm.fold (fun _ th acc -> extract_goals env drv acc th) m []
        else
          Hashtbl.fold
            (fun tname goals acc ->
               let th = try Mnm.find tname m with Not_found -> 
                 eprintf "File %s : --goal/--goals_of : Unknown theory %s@." 
                   file tname; exit 1 in                
               let filter = match goals with
                 | None -> None
                 | Some s -> Some 
                     (Hashtbl.fold 
                        (fun s l acc ->
                           let pr = try ns_find_pr th.th_export l 
                           with Not_found ->
                             eprintf "File %s : --goal : Unknown goal %s@." 
                               file s ; exit 1 in
                           Decl.Spr.add pr acc
                        ) s Decl.Spr.empty) in
               extract_goals ?filter env drv acc th
            ) which_theories [] in
      do_goals env drv src_filename_printer dest_filename_printer file goals
  end


let () =
  try
    let env = Env.create_env (Typing.retrieve !loadpath) in
    let drv = match !driver with
      | None -> None
      | Some file -> Some (load_driver file env) in
    if !list_printers then 
      begin
        printf "@[<hov 2>printers :@\n%a@]@."
          (Pp.print_list Pp.newline Pp.string) (Driver.list_printers ());
        exit 0
      end;
    if !list_transforms then 
      begin
        printf "@[<hov 2>transforms :@\n%a@]@."
          (Pp.print_list Pp.newline Pp.string) (Driver.list_transforms ());
        exit 0          
      end;
    let src_filename_printer = Ident.create_ident_printer 
      ?sanitizer:file_sanitizer [] in
    let dest_filename_printer = Ident.create_ident_printer 
      ?sanitizer:file_sanitizer [] in
    if Queue.is_empty files then
      do_no_file env drv src_filename_printer dest_filename_printer
    else
      Queue.iter (do_file env drv src_filename_printer dest_filename_printer)
      files
  with e when not !debug ->
    eprintf "%a@." report e;
    exit 1

(*
Local Variables: 
compile-command: "unset LANG; make -C .. byte"
End: 
*)

(****

#load "hashcons.cmo";;
#load "name.cmo";;
#load "term.cmo";;
#load "pp.cmo";;
#load "pretty.cmo";;
#install_printer Name.print;;
#install_printer Pretty.print_ty;;
#install_printer Pretty.print_term;;

open Term

let alpha = Name.from_string "alpha"
let var_alpha = Ty.ty_var alpha

let list = Ty.create_tysymbol (Name.from_string "list") [alpha] None

let list_alpha = Ty.ty_app list [var_alpha]
let list_list_alpha = Ty.ty_app list [list_alpha]

let nil = create_fsymbol (Name.from_string "nil") ([], list_alpha)
let t_nil = t_app nil [] list_alpha
let tt_nil = t_app nil [] list_list_alpha

let cons = create_fsymbol (Name.from_string "cons") 
  ([var_alpha; list_alpha], list_alpha)

let int_ = Ty.create_tysymbol (Name.from_string "int") [] None

let _ = t_app cons [t_nil; tt_nil] list_list_alpha

****)
