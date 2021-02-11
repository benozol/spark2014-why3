(* This contains a function that helps translate counterexamples back into data
   as seen in the Ada language (see function register_apply_to_records of
   Collect_data_model). *)
open Why3
open Ident
open Model_parser

let only_first_field1 str =
  let rec aux = function
    | [] -> true
    | _ :: "" :: "content" :: rest | _ :: "content" :: rest
    | "split_fields" :: rest | "split_discrs" :: rest -> aux rest
    | _ -> false in
  not (String.equal str "") && aux Re.Str.(split (regexp "__") str)

(** Decide for a list of field names of a record to replace the record with the
    value of the first field. *)
let only_first_field2 = function
  | [s] -> only_first_field1 s
  | [s1; s2] ->
      (* This recognize records corresponding to unconstrained array. We only
         want the first part of the record (the array). *)
      Strings.has_prefix "elts" s1 && Strings.has_prefix "rt" s2
  | _ -> false

let get_field_attr attr =
  match Strings.bounded_split ':' attr.attr_string 3 with
  | ["field"; _; s] -> Some s
  | _ -> None

let for_field clean (f, v) =
  let prefixes =
    ["us_split_fields"; "us_split_discrs"; "__split_discrs"; "__split_fields"] in
  if List.exists (fun s -> Strings.has_prefix s f) prefixes then
    match v with
    | Record fs ->
        let for_field (f, v) =
          match clean#value v with
          | Some v -> Some (f, v)
          | None -> None in
        Lists.map_filter for_field fs
    | _ -> (
        match clean#value v with
        | None -> []
        | Some v -> [f, v] )
  else if Strings.has_prefix "rec__ext" f then
    []
  else match clean#value v with
    | None -> []
    | Some v -> [f, v]

(* The returned [fields] are the strings S in attributes [field:N:S], which have
   to be removed from the model element name *)
let collect_attrs a (men_attrs, fields) = match get_field_attr a with
  | Some f when only_first_field1 f -> men_attrs, f :: fields
  | _ -> Sattr.add a men_attrs, fields

(* Correct a model element name by a field string *)
let correct_name f = Re.Str.global_replace (Re.Str.regexp_string f) ""

let clean_name men =
  let men_attrs, fields =
    Sattr.fold collect_attrs men.men_attrs (Sattr.empty, []) in
  let men_name = List.fold_right correct_name fields men.men_name in
  {men with men_name; men_attrs}

(* Filtering the model to remove elements that are not understood by gnat2why
   (sole purpose is to reduce the size of the output). *)
let in_spark (e: model_element) =
  let name = e.me_name.men_name in
  (* Names that do not begin with either '.' or a number are not recognized by
     gnat2why *)
  String.length name > 0 && (name.[0] = '.' || (name.[0] <= '9' && name.[0] >= '0'))

(** Clean values by a) replacing records according to [only_first_field2] and
   simplifying discriminant records b) removing unparsed values, in which the
   function returns [None]. *)
let clean = object (self)
  inherit Model_parser.clean as super

  method! record fs =
    let field_names, field_values = List.split fs in
    if only_first_field2 field_names then
      self#value (List.hd field_values)
    else match List.concat (List.map (for_field self) fs) with
      | [] -> None
      | fs -> Some (Record fs)

  method! element me =
    match super#element me with
    | Some me ->
        let {Model_parser.men_name= name; men_attrs= attrs} = me.me_name in
        let men_name = Ident.get_model_trace_string ~name ~attrs in
        let men_name = List.hd (Strings.bounded_split '@' men_name 2) in
        let me_name = clean_name {me.me_name with men_name} in
        let me = {me with me_name} in
        if in_spark me then Some me else None
    | _ -> None
end

let model_to_model (m, summary) =
  let open Counterexample in
  match summary with
  | NC log | SW log | NCSW log ->
      Some (model_of_exec_log ~original_model:m log, summary)
  | _ -> None
