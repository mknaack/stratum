open Caqti.Templater

type t = ((module Caqti_eio.CONNECTION), Caqti.Error.connect) Caqti_eio.Pool.t
type 'a res = ('a, Caqti.Error.t) result

let connect ~sw ~stdenv uri =
  Caqti_eio.connect_pool ~sw ~stdenv (Uri.of_string uri)

let with_conn pool f =
  Caqti_eio.Pool.use
    (fun (module Db : Caqti_eio.CONNECTION) ->
      f (module Db : Caqti_eio.CONNECTION))
    pool

(* run f on one connection inside a transaction; rollback on Error or
   exception (re-raised), commit on Ok *)
let with_txn pool f =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  match Db.start () with
  | Error _ as e -> e
  | Ok () -> (
      try
        match f (module Db : Caqti_eio.CONNECTION) with
        | Ok v -> ( match Db.commit () with Ok () -> Ok v | Error _ as e -> e)
        | Error _ as e -> ( match Db.rollback () with _ -> e)
      with e ->
        ignore (Db.rollback ());
        raise e)

let migrate pool =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let exec s = Db.exec (direct T.(unit -->. unit) s) () in
  let ( let* ) x f = match x with Error _ as e -> e | Ok v -> f v in
  let* () =
    exec
      "CREATE TABLE IF NOT EXISTS databases (db text PRIMARY KEY, resource \
       jsonb NOT NULL)"
  in
  let* () =
    exec
      "CREATE TABLE IF NOT EXISTS collections (db text NOT NULL, coll text NOT \
       NULL, pk_path text, resource jsonb NOT NULL, PRIMARY KEY (db, coll))"
  in
  let* () =
    exec
      "CREATE TABLE IF NOT EXISTS docs (db text NOT NULL, coll text NOT NULL, \
       id text NOT NULL, pk text, doc jsonb NOT NULL, lsn bigserial, PRIMARY \
       KEY (db, coll, id), UNIQUE (lsn))"
  in
  exec "CREATE INDEX IF NOT EXISTS docs_pk ON docs (db, coll, pk)"

let json_of_text = Yojson.Safe.from_string

(* --- databases --- *)

let db_create pool ~id ~resource =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(t2 string string -->? string)
      "INSERT INTO databases (db, resource) VALUES (?, ?::jsonb) ON CONFLICT \
       (db) DO NOTHING RETURNING db"
  in
  Db.find_opt req (id, Yojson.Safe.to_string resource)
  |> Result.map (function Some _ -> `Created | None -> `Conflict)

let db_get pool ~id =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(string -->? string)
      "SELECT resource::text FROM databases WHERE db = ?"
  in
  Db.find_opt req id |> Result.map (Option.map json_of_text)

let db_rid pool ~id =
  match db_get pool ~id with
  | Ok (Some j) -> Ok Yojson.Safe.Util.(j |> member "_rid" |> to_string_option)
  | Ok None -> Ok None
  | Error _ as e -> e

let db_list pool =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(unit -->* string)
      "SELECT resource::text FROM databases ORDER BY db"
  in
  Db.collect_list req () |> Result.map (List.map json_of_text)

let db_delete pool ~id =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(string -->? string)
      "DELETE FROM databases WHERE db = ? RETURNING db"
  in
  Db.find_opt req id |> Result.map Option.is_some

(* --- collections --- *)

let coll_create pool ~db ~coll ~pk_path ~resource =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(t4 string string (option string) string -->? string)
      "INSERT INTO collections (db, coll, pk_path, resource) VALUES (?, ?, ?, \
       ?::jsonb) ON CONFLICT (db, coll) DO NOTHING RETURNING coll"
  in
  Db.find_opt req (db, coll, pk_path, Yojson.Safe.to_string resource)
  |> Result.map (function Some _ -> `Created | None -> `Conflict)

let coll_get pool ~db ~coll =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(t2 string string -->? t2 (option string) string)
      "SELECT pk_path, resource::text FROM collections WHERE db = ? AND coll = \
       ?"
  in
  Db.find_opt req (db, coll)
  |> Result.map (Option.map (fun (pk_path, r) -> (pk_path, json_of_text r)))

let coll_list pool ~db =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(string -->* string)
      "SELECT resource::text FROM collections WHERE db = ? ORDER BY coll"
  in
  Db.collect_list req db |> Result.map (List.map json_of_text)

let coll_delete pool ~db ~coll =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(t2 string string -->? string)
      "DELETE FROM collections WHERE db = ? AND coll = ? RETURNING coll"
  in
  Db.find_opt req (db, coll) |> Result.map Option.is_some

(* --- documents --- *)

let doc_create_conn (module Db : Caqti_eio.CONNECTION) ~db ~coll ~id ~pk ~doc
    ~upsert =
  let sql =
    "INSERT INTO docs (db, coll, id, pk, doc) VALUES (?, ?, ?, ?, ?::jsonb) "
    ^ (if upsert then
         "ON CONFLICT (db, coll, id) DO UPDATE SET doc = EXCLUDED.doc, pk = \
          EXCLUDED.pk "
       else "ON CONFLICT (db, coll, id) DO NOTHING ")
    ^ "RETURNING doc::text"
  in
  let req =
    direct
      T.(t2 (t3 string string string) (t2 (option string) string) -->? string)
      sql
  in
  Db.find_opt req ((db, coll, id), (pk, Yojson.Safe.to_string doc))
  |> Result.map (function
    | Some doc -> `Ok (json_of_text doc)
    | None -> `Conflict)

let doc_create pool ~db ~coll ~id ~pk ~doc ~upsert =
  with_conn pool @@ fun conn ->
  doc_create_conn conn ~db ~coll ~id ~pk ~doc ~upsert

let doc_get_conn (module Db : Caqti_eio.CONNECTION) ~db ~coll ~id =
  let req =
    direct
      T.(t3 string string string -->? string)
      "SELECT doc::text FROM docs WHERE db = ? AND coll = ? AND id = ?"
  in
  Db.find_opt req (db, coll, id) |> Result.map (Option.map json_of_text)

let doc_get pool ~db ~coll ~id =
  with_conn pool @@ fun conn -> doc_get_conn conn ~db ~coll ~id

let doc_replace_conn (module Db : Caqti_eio.CONNECTION) ~db ~coll ~id ~pk ~doc =
  let req =
    direct
      T.(t2 (t2 string (option string)) (t3 string string string) -->? string)
      "UPDATE docs SET doc = ?::jsonb, pk = ? WHERE db = ? AND coll = ? AND id \
       = ? RETURNING doc::text"
  in
  Db.find_opt req ((Yojson.Safe.to_string doc, pk), (db, coll, id))
  |> Result.map (Option.map json_of_text)

let doc_replace pool ~db ~coll ~id ~pk ~doc =
  with_conn pool @@ fun conn -> doc_replace_conn conn ~db ~coll ~id ~pk ~doc

(* pk: (header pk option, header pk option) — second slot drives
   "(? IS NULL OR pk = ?)" so an absent header skips the pk check *)
let doc_delete_conn (module Db : Caqti_eio.CONNECTION) ~db ~coll ~id ~pk_header
    =
  let req =
    direct
      T.(
        t2 (t3 string string string) (t2 (option string) (option string))
        -->? string)
      "DELETE FROM docs WHERE db = ? AND coll = ? AND id = ? AND (? IS NULL OR \
       pk = ?) RETURNING id"
  in
  Db.find_opt req ((db, coll, id), (pk_header, pk_header))
  |> Result.map Option.is_some

let doc_delete pool ~db ~coll ~id ~pk_header =
  with_conn pool @@ fun conn -> doc_delete_conn conn ~db ~coll ~id ~pk_header

(* set_sql is a composed jsonb expression (jsonb_set/#- over `doc`);
   cond_sql is an optional WHERE fragment for the PATCH `condition` *)
let doc_patch_conn (module Db : Caqti_eio.CONNECTION) ~db ~coll ~id ~pk_header
    ~set_sql ~cond_sql =
  let sql =
    "UPDATE docs SET doc = " ^ set_sql
    ^ " WHERE db = ? AND coll = ? AND id = ? AND (? IS NULL OR pk = ?)"
    ^ (match cond_sql with None -> "" | Some c -> " AND (" ^ c ^ ")")
    ^ " RETURNING doc::text"
  in
  let req =
    direct
      T.(
        t2 (t3 string string string) (t2 (option string) (option string))
        -->? string)
      sql
  in
  Db.find_opt req ((db, coll, id), (pk_header, pk_header))
  |> Result.map (Option.map json_of_text)

let doc_patch pool ~db ~coll ~id ~pk_header ~set_sql ~cond_sql =
  with_conn pool @@ fun conn ->
  doc_patch_conn conn ~db ~coll ~id ~pk_header ~set_sql ~cond_sql

(* rows of (lsn, doc) after a cursor, capped to fetch; `since` bounds
   doc->_ts for If-Modified-Since *)
let doc_list pool ~db ~coll ~after ~fetch ?(since = None) () =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req =
    direct
      T.(
        t3 (t2 string string) (t3 int64 (option int64) (option int64)) int
        -->* t2 int64 string)
      "SELECT lsn, doc::text FROM docs WHERE db = ? AND coll = ? AND lsn > ? \
       AND (? IS NULL OR (doc->>'_ts')::bigint >= ?) ORDER BY lsn LIMIT ?"
  in
  Db.collect_list req ((db, coll), (after, since, since), fetch)
  |> Result.map (List.map (fun (lsn, d) -> (lsn, json_of_text d)))

(* ad-hoc query: sql built by Query.Compile; one jsonb-text column out *)
let doc_query pool ~db ~coll ~sql =
  with_conn pool @@ fun (module Db : Caqti_eio.CONNECTION) ->
  let req = direct T.(t2 string string -->* string) sql in
  Db.collect_list req (db, coll) |> Result.map (List.map json_of_text)
