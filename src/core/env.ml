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

open Ident
open Theory

(** Environment *)

type env = {
  env_retrieve : retrieve_theory;
  env_memo     : (string list, theory Mnm.t) Hashtbl.t;
  env_tag      : int;
}

and retrieve_theory = env -> string list -> theory Mnm.t

let create_env =
  let r = ref 0 in
  fun retrieve ->
    incr r;
    let env = {
      env_retrieve = retrieve;
      env_memo     = Hashtbl.create 17;
      env_tag      = !r }
    in
    let th = builtin_theory in
    let m = Mnm.add th.th_name.id_string th Mnm.empty in
    Hashtbl.add env.env_memo [] m;
    env

exception TheoryNotFound of string list * string

let find_theory env sl s =
  try
    let m =
      try
	Hashtbl.find env.env_memo sl
      with Not_found ->
	Hashtbl.add env.env_memo sl Mnm.empty;
	let m = env.env_retrieve env sl in
	Hashtbl.replace env.env_memo sl m;
	m
    in
    Mnm.find s m
  with Not_found ->
    raise (TheoryNotFound (sl, s))

let env_tag env = env.env_tag


(** Parsers *)

type parse_only   = env -> string -> in_channel -> unit
type read_channel = env -> string -> in_channel -> theory Mnm.t
type error_report = Format.formatter -> exn -> unit

let parse_only_table   = Hashtbl.create 17 (* parser name -> parse_only *)
let read_channel_table = Hashtbl.create 17 (* parser name -> read_channel *)
let error_report_table = Hashtbl.create 17 (* parser name -> error_report *)
let suffixes_table     = Hashtbl.create 17 (* suffix -> parser name *)

let register_parser name suffixes po rc er =
  Hashtbl.add parse_only_table   name po;
  Hashtbl.add read_channel_table name rc;
  Hashtbl.add error_report_table name er;
  List.iter (fun s -> Hashtbl.add suffixes_table ("." ^ s) name) suffixes

exception UnknownParser of string (* parser name *)

let parser_for_file ?name file = match name with
  | None -> 
      begin try 
	let ext = 
	  let s = Filename.chop_extension file in
	  let n = String.length s in
	  String.sub file n (String.length file - n)
	in
	Hashtbl.find suffixes_table ext
      with Invalid_argument _ | Not_found -> 
	"why"
      end
  | Some n -> 
      n

let find_parser table n =
  try Hashtbl.find table n
  with Not_found -> raise (UnknownParser n)

let parse_only ?name env file ic =
  let n = parser_for_file ?name file in 
  let po = find_parser parse_only_table n in
  po env file ic

let read_channel ?name env file ic =
  let n = parser_for_file ?name file in 
  let rc = find_parser read_channel_table n in
  rc env file ic

let report ?name file fmt e =
  let n = parser_for_file ?name file in 
  let er = find_parser error_report_table n in
  er fmt e

let list_formats () =
  let h = Hashtbl.create 17 in
  let add s p = 
    let l = try Hashtbl.find h p with Not_found -> [] in
    Hashtbl.replace h p (s :: l)
  in
  Hashtbl.iter add suffixes_table;
  Hashtbl.fold (fun p l acc -> (p, l) :: acc) h []

