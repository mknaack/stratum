open Compile

type patch_kind = Set | Add | Replace | Remove | Incr

let patch_kind_of_string = function
  | "set" -> Some Set
  | "add" -> Some Add
  | "replace" -> Some Replace
  | "remove" -> Some Remove
  | "incr" -> Some Incr
  | _ -> None

let string_of_patch_kind = function
  | Set -> "set"
  | Add -> "add"
  | Replace -> "replace"
  | Remove -> "remove"
  | Incr -> "incr"

type patch_op = { op : patch_kind; path : string; value : Yojson.Safe.t option }

let path_segs p =
  match String.split_on_char '/' p with "" :: segs -> segs | segs -> segs

(* doc := <expr> where expr is a jsonb_set/#- chain over `doc` *)
let patch_set ops =
  List.fold_left
    (fun inner (o : patch_op) ->
      let segs = path_segs o.path in
      let p = jsonb_path_lit segs in
      match (o.op, o.value) with
      | (Set | Add | Replace), Some v ->
          "jsonb_set(" ^ inner ^ ", " ^ p ^ ", " ^ jsonb_lit v ^ ", true)"
      | Remove, _ -> "(" ^ inner ^ " #- " ^ p ^ ")"
      | Incr, Some ((`Int _ | `Float _ | `Intlit _) as v) ->
          "jsonb_set(" ^ inner ^ ", " ^ p ^ ", to_jsonb(COALESCE((" ^ inner
          ^ " #>> " ^ p ^ ")::numeric, 0) + " ^ Yojson.Safe.to_string v
          ^ "::numeric))"
      | Incr, _ -> error "incr requires a numeric value"
      | (Set | Add | Replace), None ->
          error "%s requires a value" (string_of_patch_kind o.op))
    "doc" ops

(* overwrite a top-level field — used to bump _etag/_ts after a patch *)
let set_field inner name v =
  "jsonb_set(" ^ inner ^ ", " ^ jsonb_path_lit [ name ] ^ ", " ^ jsonb_lit v
  ^ ", true)"

(* PATCH `condition`: "from c where <pred>" — parse as a whole query and
   keep only the WHERE fragment *)
let compile_where s =
  let q = parse ("SELECT * " ^ s) in
  match q.where with
  | None -> error "condition has no WHERE clause"
  | Some e -> where_expr ~alias:q.alias ~params:[] e
