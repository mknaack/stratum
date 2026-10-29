open Ast

exception Error of string

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

(* --- SQL literal helpers --- *)

(* escape ' as '' inside a '...' SQL literal *)
let sql_str s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '\'';
  String.iter
    (fun c ->
      if c = '\'' then Buffer.add_string b "''" else Buffer.add_char b c)
    s;
  Buffer.add_char b '\'';
  Buffer.contents b

(* '{a,b}' text[] literal for jsonb path extraction *)
let jsonb_path_lit segs =
  let elem s =
    let escaped =
      let b = Buffer.create (String.length s) in
      String.iter
        (fun c ->
          match c with
          | '"' -> Buffer.add_string b "\\\""
          | '\\' -> Buffer.add_string b "\\\\"
          | c -> Buffer.add_char b c)
        s;
      Buffer.contents b
    in
    "\"" ^ escaped ^ "\""
  in
  sql_str ("{" ^ String.concat "," (List.map elem segs) ^ "}")

(* '$.a."b c"' jsonpath literal *)
let is_jsonpath_ident s =
  let ok c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '_'
  in
  String.length s > 0 && (s.[0] < '0' || s.[0] > '9') && String.for_all ok s

let jsonpath_lit segs =
  let elem s =
    if is_jsonpath_ident s then "." ^ s
    else
      let b = Buffer.create (String.length s + 3) in
      Buffer.add_string b ".\"";
      String.iter
        (fun c ->
          match c with
          | '"' -> Buffer.add_string b "\\\""
          | '\\' -> Buffer.add_string b "\\\\"
          | c -> Buffer.add_char b c)
        s;
      Buffer.add_char b '"';
      Buffer.contents b
  in
  sql_str ("$" ^ String.concat "" (List.map elem segs))

(* JSON value as '<json>'::jsonb *)
let jsonb_lit (v : Yojson.Safe.t) =
  sql_str (Yojson.Safe.to_string v) ^ "::jsonb"

(* --- path expression --- *)

let jpath segs = "doc #> " ^ jsonb_path_lit segs
let jpath_text segs = "doc #>> " ^ jsonb_path_lit segs
let jpath_exists segs = "jsonb_exists(doc, " ^ jsonpath_lit segs ^ ")"

(* resolve @name from query parameters; the OCaml client sends values as
   strings, so parse each as JSON and fall back to a plain string *)
let param_value params name =
  match List.assoc_opt name params with
  | None -> error "unbound parameter @%s" name
  | Some s -> (
      match Yojson.Safe.from_string s with v -> v | exception _ -> `String s)

(* strip the root alias from a path *)
let drop_alias alias = function
  | a :: rest when String.equal a alias -> rest
  | segs -> segs

let rec value_expr ~alias ~params = function
  | Path segs -> (
      match drop_alias alias segs with [] -> "doc" | segs -> jpath segs)
  | Lit v -> jsonb_lit v
  | Param name -> jsonb_lit (param_value params name)
  | Call (f, args) -> call_expr ~alias f args
  | _ -> error "expression not supported in SELECT/projection"

and call_expr ~alias f args =
  let path_arg = function
    | [ Path segs ] -> drop_alias alias segs
    | _ -> error "%s expects a field path" f
  in
  match (f, args) with
  | "LOWER", [ _ ] -> "to_jsonb(lower(" ^ jpath_text (path_arg args) ^ "))"
  | "UPPER", [ _ ] -> "to_jsonb(upper(" ^ jpath_text (path_arg args) ^ "))"
  | "LENGTH", [ _ ] ->
      "to_jsonb(char_length(" ^ jpath_text (path_arg args) ^ "))"
  | "ABS", [ _ ] ->
      "to_jsonb(abs((" ^ jpath_text (path_arg args) ^ ")::numeric))"
  | _ -> error "unsupported function %s" f

(* truth of a bare path predicate: `WHERE c.flag` *)
let truth segs = "(" ^ jpath segs ^ ") = 'true'::jsonb"

let rec where_expr ~alias ~params = function
  | Path [] -> truth []
  | Path segs -> truth (drop_alias alias segs)
  | Bin (op, l, r) ->
      bin_sql op (value_expr ~alias ~params l) (value_expr ~alias ~params r)
  | And (l, r) ->
      "("
      ^ where_expr ~alias ~params l
      ^ " AND "
      ^ where_expr ~alias ~params r
      ^ ")"
  | Or (l, r) ->
      "("
      ^ where_expr ~alias ~params l
      ^ " OR "
      ^ where_expr ~alias ~params r
      ^ ")"
  | Not e -> "(NOT " ^ where_expr ~alias ~params e ^ ")"
  | In (e, items) ->
      let elems =
        String.concat ", "
          (List.map (fun i -> value_expr ~alias ~params i) items)
      in
      "(" ^ value_expr ~alias ~params e ^ " = ANY (ARRAY[" ^ elems ^ "]))"
  | Between (e, lo, hi) ->
      let v = value_expr ~alias ~params e in
      "(" ^ v ^ " >= "
      ^ value_expr ~alias ~params lo
      ^ " AND " ^ v ^ " <= "
      ^ value_expr ~alias ~params hi
      ^ ")"
  | Call (f, args) -> pred_call ~alias ~params f args
  | _ -> error "unsupported predicate"

and bin_sql op l r =
  let o =
    match op with
    | Eq -> " = "
    | Ne -> " <> "
    | Lt -> " < "
    | Le -> " <= "
    | Gt -> " > "
    | Ge -> " >= "
  in
  "(" ^ l ^ o ^ r ^ ")"

and pred_call ~alias ~params f args =
  let path_arg = function
    | Path segs :: _ -> drop_alias alias segs
    | _ -> error "%s expects a field path" f
  in
  let lit_arg = function
    | [ _; Lit v ] -> v
    | [ _; Param n ] -> param_value params n
    | _ -> error "%s expects a literal argument" f
  in
  let string_guard p = "(jsonb_typeof(" ^ jpath p ^ ") = 'string' AND " in
  match f with
  | "IS_DEFINED" -> jpath_exists (path_arg args)
  | "IS_NULL" -> "(" ^ jpath (path_arg args) ^ " = 'null'::jsonb)"
  | "IS_NUMBER" -> "(jsonb_typeof(" ^ jpath (path_arg args) ^ ") = 'number')"
  | "IS_STRING" -> "(jsonb_typeof(" ^ jpath (path_arg args) ^ ") = 'string')"
  | "IS_BOOL" -> "(jsonb_typeof(" ^ jpath (path_arg args) ^ ") = 'boolean')"
  | "IS_ARRAY" -> "(jsonb_typeof(" ^ jpath (path_arg args) ^ ") = 'array')"
  | "IS_OBJECT" -> "(jsonb_typeof(" ^ jpath (path_arg args) ^ ") = 'object')"
  | "IS_PRIMITIVE" ->
      "(jsonb_typeof("
      ^ jpath (path_arg args)
      ^ ") IN ('number','string','boolean','null'))"
  | "CONTAINS" -> (
      let p = path_arg args in
      match lit_arg args with
      | `String s ->
          string_guard p ^ "strpos(" ^ jpath_text p ^ ", " ^ sql_str s
          ^ ") > 0)"
      | _ -> error "CONTAINS expects a string literal")
  | "STARTSWITH" -> (
      let p = path_arg args in
      match lit_arg args with
      | `String s ->
          string_guard p ^ "left(" ^ jpath_text p ^ ", "
          ^ string_of_int (String.length s)
          ^ ") = " ^ sql_str s ^ ")"
      | _ -> error "STARTSWITH expects a string literal")
  | "ENDSWITH" -> (
      let p = path_arg args in
      match lit_arg args with
      | `String s ->
          string_guard p ^ "right(" ^ jpath_text p ^ ", "
          ^ string_of_int (String.length s)
          ^ ") = " ^ sql_str s ^ ")"
      | _ -> error "ENDSWITH expects a string literal")
  | "ARRAY_CONTAINS" ->
      let v = lit_arg args in
      "(" ^ jpath (path_arg args) ^ " @> " ^ jsonb_lit (`List [ v ]) ^ ")"
  | _ -> error "unsupported function %s" f

let key_of_item { item; alias = a } =
  match (a, item) with
  | Some k, _ -> k
  | None, Path segs -> (
      match List.rev segs with
      | k :: _ -> k
      | [] -> error "SELECT * not allowed here")
  | None, _ -> error "SELECT item requires AS alias"

let select_sql ~alias ~params = function
  | All -> "doc"
  | Value e -> value_expr ~alias ~params e
  | Items items ->
      let pairs =
        items
        |> List.map (fun it ->
            sql_str (key_of_item it) ^ ", " ^ value_expr ~alias ~params it.item)
      in
      "jsonb_build_object(" ^ String.concat ", " pairs ^ ")"

let compile_select ~params ~fetch ~offset ~wh (q : t) : string =
  let sel = select_sql ~alias:q.alias ~params q.select in
  let ord =
    match q.order_by with
    | [] -> [ "ORDER BY lsn" ]
    | items ->
        [
          "ORDER BY "
          ^ String.concat ", "
              (List.map
                 (fun (e, dir) ->
                   value_expr ~alias:q.alias ~params e
                   ^ match dir with Asc -> " ASC" | Desc -> " DESC")
                 items)
          ^ ", lsn";
        ]
  in
  let lim =
    match (q.top, q.limit) with
    | Some t, _ -> min t fetch
    | None, Some l -> min l fetch
    | None, None -> fetch
  in
  String.concat " "
    ([ "SELECT"; sel; "FROM docs WHERE db = ? AND coll = ?" ]
    @ wh @ ord
    @ [
        "LIMIT " ^ string_of_int lim;
        "OFFSET " ^ string_of_int (offset + q.offset);
      ])

let compile ~params ~fetch ~offset (q : t) : string =
  let wh =
    match q.where with
    | None -> []
    | Some e -> [ "AND " ^ where_expr ~alias:q.alias ~params e ]
  in
  (* aggregates return a single scalar row; ordering/paging don't apply *)
  match q.select with
  | Value (Call ("COUNT", _)) ->
      String.concat " "
        ([ "SELECT COUNT(*)::text FROM docs WHERE db = ? AND coll = ?" ] @ wh)
  | _ -> compile_select ~params ~fetch ~offset ~wh q

let parse s =
  let lexbuf = Lexing.from_string s in
  try Parser.query Lexer.token lexbuf with
  | Lexer.Lex_error e -> error "lex error: %s" e
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      error "parse error at character %d near '%s'" pos.pos_cnum
        (Lexing.lexeme lexbuf)
