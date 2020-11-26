(* This contains a function that helps translate counterexamples back into data
   as seen in the Ada language (see function register_apply_to_records of
   Collect_data_model). *)
open Why3
open Wstdlib

let get_only_first l =
  match l with
  | [x; y] when Strings.has_prefix "elts" x &&
                Strings.has_prefix "rt" y ->
    (* This recognize records corresponding to unconstrained array. We only
       want the first part of the record (the array). *)
      true
  | [x] when Strings.has_prefix "map__content" x ->
    (* Corresponds to map *)
      true
  | [x] when Strings.has_prefix "t__content" x ->
    (* Corresponds to bv *)
      true
  | [x] when Strings.ends_with x "__content" ->
    (* Records for int__content, bool__content, real__content or anything
       content: we are only interested in the value (not in the record). *)
      true
  | [x] when Strings.has_prefix "__split_fields" x ->
      true
  | [x] when Strings.has_prefix "__split_discrs" x ->
      true
  | _ -> false

let remove_fields_attrs attrs =
  Ident.Sattr.fold (fun attr acc ->
      match Strings.bounded_split ':' attr.Ident.attr_string 3 with
      | "field" :: _ :: s when get_only_first s ->
          acc
      | _ -> Ident.Sattr.add attr acc) attrs Ident.Sattr.empty

let rec remove_fields model_value =
  Model_parser.(match model_value with
  | Integer _ | Decimal _ | Fraction _ | Float _ | Boolean _ | Bitvector _
    | Unparsed _ | String _ -> model_value
  | Array a ->
      Array (remove_fields_array a)
  | Record l when get_only_first (List.map fst l) ->
      remove_fields (snd (List.hd l))
  | Record r ->
      let r =
        List.map (fun (field_name, value) ->
            (field_name, remove_fields value)
          )
          r
      in
      Record r
  | Proj p ->
      let proj_name, value = p in
      Proj (proj_name, remove_fields value)
  | Apply (s, l) ->
      Apply (s, (List.map (fun v -> remove_fields v) l)))

and remove_fields_array a =
  Model_parser.(
  let {arr_others = others; arr_indices = arr_index_list} = a in
  let others = remove_fields others in
  let arr_index_list =
    List.map (fun ind ->
        let {arr_index_key = key; arr_index_value = value} = ind in
        let value = remove_fields value in
        { arr_index_key = key; arr_index_value = value}
      )
      arr_index_list
  in
  {arr_others = others; arr_indices = arr_index_list}
  )

let () = Model_parser.register_remove_field
    (fun (attrs, v) -> remove_fields_attrs attrs, remove_fields v)

