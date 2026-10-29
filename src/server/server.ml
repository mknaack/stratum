open Yojson.Safe.Util
module H = Http.Header

let json_headers extra =
  let base =
    [
      ("content-type", "application/json");
      ("x-ms-request-charge", "1");
      ("x-ms-activity-id", Protocol.rid ());
      ("x-ms-session-token", "0:-1#1");
    ]
  in
  H.of_list (base @ extra)

let respond ?(headers = H.init ()) ~status body =
  Cohttp_eio.Server.respond ~headers ~status
    ~body:(Cohttp_eio.Body.of_string body)
    ()

let respond_json ?(extra = []) ~status json =
  respond ~headers:(json_headers extra) ~status (Yojson.Safe.to_string json)

let error ~status ~code ~message =
  respond_json ~status (Protocol.error_json ~code ~message)

let not_found msg = error ~status:`Not_found ~code:"NotFound" ~message:msg
let bad_request msg = error ~status:`Bad_request ~code:"BadRequest" ~message:msg
let conflict msg = error ~status:`Conflict ~code:"Conflict" ~message:msg

let precondition_failed msg =
  error ~status:`Precondition_failed ~code:"PreconditionFailed" ~message:msg

let db_error e =
  error ~status:`Internal_server_error ~code:"InternalServerError"
    ~message:(Caqti.Error.show e)

let header req name = H.get (Http.Request.headers req) name

let bool_header req name =
  match header req name with Some "true" | Some "True" -> true | _ -> false

let int_header req name = Option.bind (header req name) int_of_string_opt

(* opaque base64 cursor: an lsn for feed lists, an offset for queries *)
let continuation req =
  match header req "x-ms-continuation" with
  | None -> Ok 0L
  | Some c -> (
      match Protocol.lsn_of_continuation c with
      | Some l -> Ok l
      | None -> Error (bad_request "invalid continuation token"))

let pk_of_req req =
  match header req "x-ms-documentdb-partitionkey" with
  | None -> None
  | Some s -> Protocol.parse_pk_header s

(* --- storage-backed handlers --- *)

(* the batch endpoint posts a bare array, so any top-level JSON is accepted *)
let parse_body s =
  match Yojson.Safe.from_string s with
  | j -> Ok j
  | exception _ -> Error (bad_request "request body is not valid JSON")

let require_coll pool db coll f =
  match Storage.coll_get pool ~db ~coll with
  | Error e -> db_error e
  | Ok None -> not_found (Printf.sprintf "Collection %s not found" coll)
  | Ok (Some (pk_path, resource)) -> f ~pk_path ~resource

let list_dbs pool _req _body =
  match Storage.db_list pool with
  | Error e -> db_error e
  | Ok items ->
      respond_json ~status:`OK (Protocol.feed ~rid:"" ~member:"Databases" items)

let create_db pool _req body =
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      match member "id" json |> to_string_option with
      | None -> bad_request "missing \"id\" in request body"
      | Some id -> (
          let resource =
            Protocol.database_resource ~id ~rid:(Protocol.rid ())
              ~etag:(Protocol.etag ()) ~ts:(Protocol.now ())
          in
          match Storage.db_create pool ~id ~resource with
          | Error e -> db_error e
          | Ok `Conflict ->
              conflict (Printf.sprintf "Database %s already exists" id)
          | Ok `Created -> respond_json ~status:`Created resource))

let get_db pool db =
  match Storage.db_get pool ~id:db with
  | Error e -> db_error e
  | Ok None -> not_found (Printf.sprintf "Database %s not found" db)
  | Ok (Some r) ->
      let et = Yojson.Safe.Util.(r |> member "_etag" |> to_string) in
      respond_json ~extra:[ ("etag", et) ] ~status:`OK r

let delete_db pool db =
  match Storage.db_delete pool ~id:db with
  | Error e -> db_error e
  | Ok false -> not_found (Printf.sprintf "Database %s not found" db)
  | Ok true -> respond ~headers:(json_headers []) ~status:`No_content ""

let list_colls pool db _req _body =
  match Storage.coll_list pool ~db with
  | Error e -> db_error e
  | Ok items -> (
      match Storage.db_rid pool ~id:db with
      | Error e -> db_error e
      | Ok db_rid ->
          let rid = Option.value db_rid ~default:"" in
          respond_json ~status:`OK
            (Protocol.feed ~rid ~member:"DocumentCollections" items))

let pk_paths_of_json json =
  match member "partitionKey" json with
  | `Assoc _ as pk ->
      member "paths" pk |> to_list |> List.filter_map to_string_option
  | _ -> []

(* non-doc resources live in the docs table under reserved coll ids *)
let users_coll = "__users"
let permissions_coll user = "__permissions:" ^ user
let offers_db = "__offers"
let offers_coll = "__offers"
let coll_rid_of resource = resource |> member "_rid" |> to_string

(* one offer per collection, minted at create time; x-ms-offer-throughput
   defaults to 400 *)
let mint_offer pool ~resource ~offer_resource_id ~throughput =
  let o_rid = Protocol.rid () in
  let doc =
    Protocol.offer ~rid:o_rid ~resource ~offer_resource_id ~throughput
      ~etag:(Protocol.quote_etag (Protocol.etag ()))
      ~ts:(Protocol.now ())
  in
  Storage.doc_create pool ~db:offers_db ~coll:offers_coll ~id:o_rid ~pk:None
    ~doc ~upsert:false

let create_coll pool db req body =
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      match member "id" json |> to_string_option with
      | None -> bad_request "missing \"id\" in request body"
      | Some coll -> (
          let pk_paths = pk_paths_of_json json in
          let pk_path = List.nth_opt pk_paths 0 in
          let partition_key = member "partitionKey" json in
          let indexing_policy = Protocol.member_opt "indexingPolicy" json in
          match Storage.db_rid pool ~id:db with
          | Error e -> db_error e
          | Ok None -> not_found (Printf.sprintf "Database %s not found" db)
          | Ok (Some db_rid) -> (
              let resource =
                Protocol.collection_resource ~id:coll ~db_rid
                  ~rid:(Protocol.rid ()) ~etag:(Protocol.etag ())
                  ~ts:(Protocol.now ()) ~indexing_policy
                  ~partition_key:(Some partition_key)
              in
              match Storage.coll_create pool ~db ~coll ~pk_path ~resource with
              | Error e -> db_error e
              | Ok `Conflict ->
                  conflict (Printf.sprintf "Collection %s already exists" coll)
              | Ok `Created -> (
                  let throughput =
                    int_header req "x-ms-offer-throughput"
                    |> Option.value ~default:400
                  in
                  match
                    mint_offer pool
                      ~resource:(resource |> member "_self" |> to_string)
                      ~offer_resource_id:(coll_rid_of resource) ~throughput
                  with
                  | Error e -> db_error e
                  | Ok _ -> respond_json ~status:`Created resource))))

let get_coll pool db coll =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource ->
  respond_json ~status:`OK resource

let delete_coll pool db coll =
  match Storage.coll_delete pool ~db ~coll with
  | Error e -> db_error e
  | Ok false -> not_found (Printf.sprintf "Collection %s not found" coll)
  | Ok true -> respond ~headers:(json_headers []) ~status:`No_content ""

(* --- documents --- *)

let doc_pk pool db coll doc =
  match Storage.coll_get pool ~db ~coll with
  | Ok (Some (Some pk_path, _)) -> Protocol.pk_of_doc pk_path doc
  | _ -> None

(* Cosmos parameter names include the leading @; the AST drops it *)
let param_name n =
  if String.starts_with ~prefix:"@" n then String.sub n 1 (String.length n - 1)
  else n

let params_of_json json =
  member "parameters" json |> to_list
  |> List.filter_map (fun p ->
      match (member "name" p |> to_string_option, member "value" p) with
      | Some n, `String v -> Some (param_name n, v)
      | Some n, v -> Some (param_name n, Yojson.Safe.to_string v)
      | _ -> None)

let create_doc pool db coll ~upsert req body =
  require_coll pool db coll @@ fun ~pk_path ~resource ->
  match parse_body body with
  | Error r -> r
  | Ok doc -> (
      match Storage.db_rid pool ~id:db with
      | Error e -> db_error e
      | Ok None -> not_found (Printf.sprintf "Database %s not found" db)
      | Ok (Some db_rid) -> (
          let id =
            match Protocol.member_opt "id" doc with
            | Some (`String s) -> s
            | _ -> Protocol.rid ()
          in
          let doc =
            `Assoc (("id", `String id) :: List.remove_assoc "id" (to_assoc doc))
          in
          let pk =
            match pk_path with
            | Some p -> Protocol.pk_of_doc p doc
            | None -> pk_of_req req
          in
          let doc =
            Protocol.inject_doc_meta ~db_rid ~coll_rid:(coll_rid_of resource)
              ~doc_rid:(Protocol.rid ()) ~etag:(Protocol.etag ())
              ~ts:(Protocol.now ()) doc
          in
          match Storage.doc_create pool ~db ~coll ~id ~pk ~doc ~upsert with
          | Error e -> db_error e
          | Ok `Conflict ->
              conflict (Printf.sprintf "Document %s already exists" id)
          | Ok (`Ok (doc, inserted)) ->
              let et = Yojson.Safe.Util.(doc |> member "_etag" |> to_string) in
              (* upsert on an existing doc replaces it: Cosmos answers 200 *)
              respond_json
                ~extra:[ ("etag", et) ]
                ~status:(if inserted then `Created else `OK)
                doc))

let get_doc pool db coll id req =
  require_coll pool db coll @@ fun ~pk_path ~resource:_ ->
  match Storage.doc_get pool ~db ~coll ~id with
  | Error e -> db_error e
  | Ok None -> not_found (Printf.sprintf "Document %s not found" id)
  | Ok (Some doc) -> (
      match (pk_of_req req, pk_path) with
      | Some hdr, Some path when Protocol.pk_of_doc path doc <> Some hdr ->
          not_found (Printf.sprintf "Document %s not found" id)
      | _ ->
          let et = doc |> member "_etag" |> to_string in
          respond_json ~extra:[ ("etag", et) ] ~status:`OK doc)

let replace_doc pool db coll id req body =
  require_coll pool db coll @@ fun ~pk_path ~resource ->
  match parse_body body with
  | Error r -> r
  | Ok doc -> (
      let pk =
        match pk_path with
        | Some p -> Protocol.pk_of_doc p doc
        | None -> pk_of_req req
      in
      match Storage.db_rid pool ~id:db with
      | Error e -> db_error e
      | Ok None -> not_found (Printf.sprintf "Database %s not found" db)
      | Ok (Some db_rid) -> (
          let doc =
            `Assoc (("id", `String id) :: List.remove_assoc "id" (to_assoc doc))
          in
          let doc =
            Protocol.inject_doc_meta ~db_rid ~coll_rid:(coll_rid_of resource)
              ~doc_rid:(Protocol.rid ()) ~etag:(Protocol.etag ())
              ~ts:(Protocol.now ()) doc
          in
          match Storage.doc_get pool ~db ~coll ~id with
          | Error e -> db_error e
          | Ok None -> not_found (Printf.sprintf "Document %s not found" id)
          | Ok (Some existing) -> (
              let cur =
                existing |> member "_etag" |> to_string |> Protocol.unquote_etag
              in
              match header req "if-match" with
              | Some m when Protocol.unquote_etag m <> cur ->
                  error ~status:`Precondition_failed ~code:"PreconditionFailed"
                    ~message:"etag does not match"
              | _ -> (
                  match Storage.doc_replace pool ~db ~coll ~id ~pk ~doc with
                  | Error e -> db_error e
                  | Ok None ->
                      not_found (Printf.sprintf "Document %s not found" id)
                  | Ok (Some doc) ->
                      let et = doc |> member "_etag" |> to_string in
                      respond_json ~extra:[ ("etag", et) ] ~status:`OK doc))))

let list_docs pool db coll req _body =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource ->
  match continuation req with
  | Error r -> r
  | Ok after -> (
      let limit =
        Option.value (int_header req "x-ms-max-item-count") ~default:100
      in
      match Storage.doc_list pool ~db ~coll ~after ~fetch:(limit + 1) () with
      | Error e -> db_error e
      | Ok rows ->
          let page, next =
            if List.length rows > limit then
              ( List.filteri (fun i _ -> i < limit) rows,
                Some (fst (List.nth rows (limit - 1))) )
            else (rows, None)
          in
          let cont =
            match next with
            | None -> []
            | Some lsn ->
                [ ("x-ms-continuation", Protocol.continuation_of_lsn lsn) ]
          in
          respond_json ~extra:cont ~status:`OK
            (Protocol.feed ~rid:(coll_rid_of resource) ~member:"Documents"
               (List.map snd page)))

(* A-IM: Incremental feed — If-None-Match carries the continuation etag,
   304 when caught up *)
let change_feed pool db coll req =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource ->
  let limit =
    Option.value (int_header req "x-ms-max-item-count") ~default:100
  in
  let since =
    Option.bind (header req "if-modified-since") Protocol.http_date_to_epoch
  in
  let pk = pk_of_req req in
  match
    match header req "if-none-match" with
    | Some "*" -> (
        (* Start_from.Now: the checkpoint is the current feed tail *)
        match Storage.doc_max_lsn pool ~db ~coll with
        | Error e -> Error (db_error e)
        | Ok l -> Ok l)
    | inm -> (
        let cursor =
          match inm with
          | Some s -> Some (Protocol.unquote_etag s)
          | None -> header req "x-ms-continuation"
        in
        match cursor with
        | None -> Ok 0L
        | Some s -> (
            match Protocol.lsn_of_continuation s with
            | Some l -> Ok l
            | None -> Error (bad_request "invalid continuation token")))
  with
  | Error r -> r
  | Ok after -> (
      match
        Storage.doc_list pool ~db ~coll ~after ~fetch:(limit + 1) ~since ~pk ()
      with
      | Error e -> db_error e
      | Ok [] -> (
          (* clients read the etag off a 304 as the next checkpoint *)
          match Storage.doc_max_lsn pool ~db ~coll with
          | Error e -> db_error e
          | Ok max_lsn ->
              let cont = Protocol.continuation_of_lsn max_lsn in
              respond
                ~headers:(json_headers [ ("etag", cont) ])
                ~status:`Not_modified "")
      | Ok rows ->
          let page, more =
            if List.length rows > limit then
              (List.filteri (fun i _ -> i < limit) rows, true)
            else (rows, false)
          in
          let last_lsn = fst (List.nth page (List.length page - 1)) in
          let cont = Protocol.continuation_of_lsn last_lsn in
          let extra =
            ("etag", cont)
            :: (if more then [ ("x-ms-continuation", cont) ] else [])
          in
          respond_json ~extra ~status:`OK
            (Protocol.feed ~rid:(coll_rid_of resource) ~member:"Documents"
               (List.map snd page)))

let patch_op_of_json o =
  let s = member "op" o |> to_string in
  let op =
    match Query.Patch.patch_kind_of_string s with
    | Some k -> k
    | None -> Query.Compile.error "unsupported patch op %s" s
  in
  {
    Query.Patch.op;
    path = member "path" o |> to_string;
    value = Protocol.member_opt "value" o;
  }

let patch_doc pool db coll id req body =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource:_ ->
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      let ops =
        member "operations" json |> to_list |> List.map patch_op_of_json
      in
      match ops with
      | [] -> bad_request "PATCH requires at least one operation"
      | _ -> (
          try
            let set_sql =
              let e = Query.Patch.patch_set ops in
              Query.Patch.set_field e "_etag"
                (`String (Protocol.quote_etag (Protocol.etag ())))
              |> fun e -> Query.Patch.set_field e "_ts" (`Int (Protocol.now ()))
            in
            let cond_sql =
              Protocol.member_opt "condition" json
              |> Option.map (fun c -> Query.Patch.compile_where (to_string c))
            in
            match Storage.doc_get pool ~db ~coll ~id with
            | Error e -> db_error e
            | Ok None -> not_found (Printf.sprintf "Document %s not found" id)
            | Ok (Some _) -> (
                match
                  Storage.doc_patch pool ~db ~coll ~id
                    ~pk_header:(pk_of_req req) ~set_sql ~cond_sql
                with
                | Error e -> db_error e
                | Ok None ->
                    precondition_failed "PATCH condition evaluated to false"
                | Ok (Some doc) ->
                    let et = doc |> member "_etag" |> to_string in
                    respond_json ~extra:[ ("etag", et) ] ~status:`OK doc)
          with Query.Compile.Error e -> bad_request e))

exception Batch_fail of int * string

(* carries the failing op's index out of with_txn so the txn rolls back *)
exception Batch_escape of int * int * string

let fail code msg = raise (Batch_fail (code, msg))
let fail_db e = fail 500 (Caqti.Error.show e)

(* x-ms-cosmos-batch-atomic: every op in one Postgres transaction; the whole
   batch rolls back on the first failure *)
type batch_ctx = {
  db : string;
  coll : string;
  db_rid : string;
  coll_rid : string;
  pk_of_body : Yojson.Safe.t -> string option;
  pk0 : string option;
}

let batch_result ~code ?body e_tag =
  Protocol.yojson_of_batch_result
    {
      status_code = code;
      request_charge = 1.0;
      e_tag;
      resource_body = Option.value body ~default:`Null;
    }

let batch_error_body code msg =
  let name =
    match code with
    | 400 -> "BadRequest"
    | 404 -> "NotFound"
    | 409 -> "Conflict"
    | 412 -> "PreconditionFailed"
    | 424 -> "FailedDependency"
    | _ -> "InternalError"
  in
  `Assoc [ ("code", `String name); ("message", `String msg) ]

(* ifMatch: doc etag must equal; ifNoneMatch: must not ("*" = must not exist) *)
let check_precondition conn (ctx : batch_ctx) (o : Protocol.batch_operation) id
    =
  match Storage.doc_get_conn conn ~db:ctx.db ~coll:ctx.coll ~id with
  | Error e -> fail_db e
  | Ok doc ->
      let cur =
        Option.map
          (fun d -> d |> member "_etag" |> to_string |> Protocol.unquote_etag)
          doc
      in
      let match_ok =
        match o.if_match with
        | None -> true
        | Some m -> cur = Some (Protocol.unquote_etag m)
      in
      let none_match_ok =
        match o.if_none_match with
        | None -> true
        | Some "*" -> cur = None
        | Some m -> cur <> Some (Protocol.unquote_etag m)
      in
      match_ok && none_match_ok

let batch_create conn ctx ~upsert (o : Protocol.batch_operation) =
  let id = member "id" o.resource_body |> to_string in
  if not (check_precondition conn ctx o id) then
    fail 412 "precondition did not match"
  else
    let doc =
      Protocol.inject_doc_meta ~db_rid:ctx.db_rid ~coll_rid:ctx.coll_rid
        ~doc_rid:(Protocol.rid ()) ~etag:(Protocol.etag ())
        ~ts:(Protocol.now ()) o.resource_body
    in
    match
      Storage.doc_create_conn conn ~db:ctx.db ~coll:ctx.coll ~id
        ~pk:(ctx.pk_of_body o.resource_body)
        ~doc ~upsert
    with
    | Ok (`Ok (doc, inserted)) ->
        batch_result
          ~code:(if inserted then 201 else 200)
          ~body:doc
          (Some (doc |> member "_etag" |> to_string))
    | Ok `Conflict -> fail 409 (Printf.sprintf "Document %s already exists" id)
    | Error e -> fail_db e

let batch_replace conn ctx (o : Protocol.batch_operation) =
  let id =
    match o.resource_id with
    | Some id -> id
    | None -> member "id" o.resource_body |> to_string
  in
  if not (check_precondition conn ctx o id) then
    fail 412 "precondition did not match"
  else
    let doc =
      Protocol.inject_doc_meta ~db_rid:ctx.db_rid ~coll_rid:ctx.coll_rid
        ~doc_rid:(Protocol.rid ()) ~etag:(Protocol.etag ())
        ~ts:(Protocol.now ()) o.resource_body
    in
    match
      Storage.doc_replace_conn conn ~db:ctx.db ~coll:ctx.coll ~id
        ~pk:(ctx.pk_of_body o.resource_body)
        ~doc
    with
    | Ok (Some doc) ->
        batch_result ~code:200 ~body:doc
          (Some (doc |> member "_etag" |> to_string))
    | Ok None -> fail 404 (Printf.sprintf "Document %s not found" id)
    | Error e -> fail_db e

let batch_delete conn ctx (o : Protocol.batch_operation) =
  let id =
    match o.resource_id with
    | Some id -> id
    | None -> fail 400 "Delete needs resourceId"
  in
  if not (check_precondition conn ctx o id) then
    fail 412 "precondition did not match"
  else
    match
      Storage.doc_delete_conn conn ~db:ctx.db ~coll:ctx.coll ~id
        ~pk_header:ctx.pk0
    with
    | Ok true -> batch_result ~code:204 None
    | Ok false -> fail 404 (Printf.sprintf "Document %s not found" id)
    | Error e -> fail_db e

let batch_read conn ctx (o : Protocol.batch_operation) =
  let id =
    match o.resource_id with
    | Some id -> id
    | None -> fail 400 "Read needs resourceId"
  in
  match Storage.doc_get_conn conn ~db:ctx.db ~coll:ctx.coll ~id with
  | Ok (Some doc) ->
      batch_result ~code:200 ~body:doc
        (Some (doc |> member "_etag" |> to_string))
  | Ok None -> fail 404 (Printf.sprintf "Document %s not found" id)
  | Error e -> fail_db e

let batch_patch conn ctx (o : Protocol.batch_operation) =
  let id =
    match o.resource_id with
    | Some id -> id
    | None -> member "id" o.resource_body |> to_string
  in
  if not (check_precondition conn ctx o id) then
    fail 412 "precondition did not match"
  else
    (* the SDK shape puts the op(s) in `value` and the condition in `from`;
       `resourceBody.operations`/`condition` covers the raw shape *)
    let pops =
      match o.value with
      | Some (`List ops) -> List.map patch_op_of_json ops
      | Some op -> [ patch_op_of_json op ]
      | None -> (
          match o.resource_body with
          | `Assoc _ ->
              member "operations" o.resource_body
              |> to_list |> List.map patch_op_of_json
          | _ -> [])
    in
    let set_sql = Query.Patch.patch_set pops in
    let cond_sql =
      match o.from_ with
      | Some c when c <> "" -> Some (Query.Patch.compile_where c)
      | _ -> (
          match o.resource_body with
          | `Assoc _ ->
              Protocol.member_opt "condition" o.resource_body
              |> Option.map (fun c -> Query.Patch.compile_where (to_string c))
          | _ -> None)
    in
    match
      Storage.doc_patch_conn conn ~db:ctx.db ~coll:ctx.coll ~id
        ~pk_header:ctx.pk0 ~set_sql ~cond_sql
    with
    | Ok (Some doc) ->
        batch_result ~code:200 ~body:doc
          (Some (doc |> member "_etag" |> to_string))
    | Ok None -> fail 412 "PATCH condition evaluated to false"
    | Error e -> fail_db e

let batch_run_op conn ctx (o : Protocol.batch_operation) =
  try
    match o.operation_type with
    | Protocol.Batch_op.Create -> batch_create conn ctx ~upsert:false o
    | Protocol.Batch_op.Upsert -> batch_create conn ctx ~upsert:true o
    | Protocol.Batch_op.Replace -> batch_replace conn ctx o
    | Protocol.Batch_op.Delete -> batch_delete conn ctx o
    | Protocol.Batch_op.Read -> batch_read conn ctx o
    | Protocol.Batch_op.Patch -> batch_patch conn ctx o
  with Query.Compile.Error e -> fail 400 e

(* one result per op; a failure at index i aborts the txn and the caller
   reports that op's status and 424 for every other *)
let batch_run_atomic conn ctx ops =
  let rec go i acc = function
    | [] -> `List (List.rev acc)
    | o :: rest ->
        let r =
          try batch_run_op conn ctx o
          with Batch_fail (code, msg) -> raise (Batch_escape (i, code, msg))
        in
        go (i + 1) (r :: acc) rest
  in
  go 0 [] ops

let batch_run_one pool ctx (o : Protocol.batch_operation) =
  match
    try Storage.with_txn pool @@ fun conn -> Ok (batch_run_op conn ctx o)
    with Batch_fail (code, msg) ->
      Ok (batch_result ~code ~body:(batch_error_body code msg) None)
  with
  | Ok r -> r
  | Error e ->
      batch_result ~code:500
        ~body:(batch_error_body 500 (Caqti.Error.show e))
        None

let batch_docs ~atomic pool db coll _req body =
  require_coll pool db coll @@ fun ~pk_path ~resource ->
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      let ops =
        (* the client posts a bare array; the REST shape wraps it in
           "operations" *)
        (match json with
          | `List ops -> ops
          | _ -> member "operations" json |> to_list)
        |> List.map Protocol.batch_operation_of_yojson
      in
      match ops with
      | [] -> bad_request "batch requires at least one operation"
      | _ -> (
          let pk_of_body b =
            match pk_path with Some p -> Protocol.pk_of_doc p b | None -> None
          in
          (* every operation must target the same partition key *)
          let pks =
            List.filter_map
              (fun (o : Protocol.batch_operation) -> pk_of_body o.resource_body)
              ops
            |> List.sort_uniq String.compare
          in
          match pks with
          | _ :: _ :: _ ->
              bad_request "batch operations must share a partition key"
          | _ -> (
              match Storage.db_rid pool ~id:db with
              | Error e -> db_error e
              | Ok None -> not_found (Printf.sprintf "Database %s not found" db)
              | Ok (Some db_rid) ->
                  let ctx =
                    {
                      db;
                      coll;
                      db_rid;
                      coll_rid = coll_rid_of resource;
                      pk_of_body;
                      pk0 = List.nth_opt pks 0;
                    }
                  in
                  if atomic then
                    try
                      match
                        Storage.with_txn pool @@ fun conn ->
                        Ok (batch_run_atomic conn ctx ops)
                      with
                      | Ok (`List results) ->
                          respond_json ~status:`OK (`List results)
                      | Error e -> db_error e
                    with Batch_escape (i, code, msg) ->
                      let results =
                        List.mapi
                          (fun j _ ->
                            if j = i then
                              batch_result ~code
                                ~body:(batch_error_body code msg)
                                None
                            else batch_result ~code:424 None)
                          ops
                      in
                      respond_json ~status:`OK (`List results)
                  else
                    respond_json ~status:`OK
                      (`List (List.map (batch_run_one pool ctx) ops)))))

let delete_doc pool db coll id req =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource:_ ->
  match Storage.doc_delete pool ~db ~coll ~id ~pk_header:(pk_of_req req) with
  | Error e -> db_error e
  | Ok false -> not_found (Printf.sprintf "Document %s not found" id)
  | Ok true -> respond ~headers:(json_headers []) ~status:`No_content ""

let query_docs pool db coll req body =
  require_coll pool db coll @@ fun ~pk_path ~resource ->
  if
    pk_path <> None
    && header req "x-ms-documentdb-partitionkey" = None
    && header req "x-ms-documentdb-query-enablecrosspartition" <> Some "True"
  then
    bad_request
      "cross partition query is required but disabled on partitioned \
       collections"
  else
    match parse_body body with
    | Error r -> r
    | Ok json -> (
        match continuation req with
        | Error r -> r
        | Ok after -> (
            let sql_text = member "query" json |> to_string in
            let params = params_of_json json in
            let limit =
              int_header req "x-ms-max-item-count" |> Option.value ~default:100
            in
            match
              try
                let q = Query.Compile.parse sql_text in
                Ok
                  (Query.Compile.compile ~params ~fetch:(limit + 1)
                     ~offset:(Int64.to_int after) q)
              with Query.Compile.Error e -> Error (bad_request e)
            with
            | Error r -> r
            | Ok sql -> (
                match Storage.doc_query pool ~db ~coll ~sql with
                | Error e -> db_error e
                | Ok rows ->
                    let page, more =
                      if List.length rows > limit then
                        (List.filteri (fun i _ -> i < limit) rows, true)
                      else (rows, false)
                    in
                    let cont =
                      if more then
                        [
                          ( "x-ms-continuation",
                            Protocol.continuation_of_lsn
                              (Int64.add after (Int64.of_int limit)) );
                        ]
                      else []
                    in
                    respond_json ~extra:cont ~status:`OK
                      (Protocol.feed ~rid:(coll_rid_of resource)
                         ~member:"Documents" page))))

(* --- users, permissions, offers --- *)

let require_db_rid pool db f =
  match Storage.db_rid pool ~id:db with
  | Error e -> db_error e
  | Ok None -> not_found (Printf.sprintf "Database %s not found" db)
  | Ok (Some db_rid) -> f ~db_rid

let list_resource pool ~db ~coll ~member ~rid =
  match Storage.doc_list pool ~db ~coll ~after:0L ~fetch:1000 () with
  | Error e -> db_error e
  | Ok rows ->
      respond_json ~status:`OK (Protocol.feed ~rid ~member (List.map snd rows))

let get_resource pool ~db ~coll ~id =
  match Storage.doc_get pool ~db ~coll ~id with
  | Error e -> db_error e
  | Ok None -> not_found (Printf.sprintf "Resource %s not found" id)
  | Ok (Some doc) -> respond_json ~status:`OK doc

let delete_resource pool ~db ~coll ~id =
  match Storage.doc_delete pool ~db ~coll ~id ~pk_header:None with
  | Error e -> db_error e
  | Ok false -> not_found (Printf.sprintf "Resource %s not found" id)
  | Ok true -> respond ~headers:(json_headers []) ~status:`No_content ""

let replace_member k v (`Assoc fs) =
  `Assoc (List.map (fun (n, j) -> (n, if n = k then v else j)) fs)

let create_user pool db body =
  require_db_rid pool db @@ fun ~db_rid ->
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      let id = member "id" json |> to_string in
      let u_rid = Protocol.rid () in
      let doc =
        Protocol.yojson_of_user_resource
          {
            id;
            rid = u_rid;
            ts = Protocol.now ();
            self = "dbs/" ^ db_rid ^ "/users/" ^ u_rid ^ "/";
            etag = Protocol.quote_etag (Protocol.etag ());
            permissions = "permissions/";
          }
      in
      match
        Storage.doc_create pool ~db ~coll:users_coll ~id ~pk:None ~doc
          ~upsert:false
      with
      | Error e -> db_error e
      | Ok `Conflict -> conflict (Printf.sprintf "User %s already exists" id)
      | Ok (`Ok (doc, _)) -> respond_json ~status:`Created doc)

let replace_user pool db user body =
  require_db_rid pool db @@ fun ~db_rid ->
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      let id = member "id" json |> to_string in
      match Storage.doc_get pool ~db ~coll:users_coll ~id:user with
      | Error e -> db_error e
      | Ok None -> not_found (Printf.sprintf "User %s not found" user)
      | Ok (Some old) -> (
          (* a PUT renames the user: the stored key is the id *)
          let u_rid = old |> member "_rid" |> to_string in
          let doc =
            Protocol.yojson_of_user_resource
              {
                id;
                rid = u_rid;
                ts = Protocol.now ();
                self = "dbs/" ^ db_rid ^ "/users/" ^ u_rid ^ "/";
                etag = Protocol.quote_etag (Protocol.etag ());
                permissions = "permissions/";
              }
          in
          match
            Storage.with_txn pool @@ fun conn ->
            match
              ( Storage.doc_delete_conn conn ~db ~coll:users_coll ~id:user
                  ~pk_header:None,
                Storage.doc_create_conn conn ~db ~coll:users_coll ~id ~pk:None
                  ~doc ~upsert:false )
            with
            | Ok _, Ok r -> Ok r
            | Error e, _ -> Error e
            | _, Error e -> Error e
          with
          | Error e -> db_error e
          | Ok `Conflict ->
              conflict (Printf.sprintf "User %s already exists" id)
          | Ok (`Ok (doc, _)) -> respond_json ~status:`OK doc))

(* permissions hang off a user; each gets its own synthetic coll *)
let require_user pool db user f =
  require_db_rid pool db @@ fun ~db_rid ->
  match Storage.doc_get pool ~db ~coll:users_coll ~id:user with
  | Error e -> db_error e
  | Ok None -> not_found (Printf.sprintf "User %s not found" user)
  | Ok (Some doc) -> f ~db_rid ~user_rid:(doc |> member "_rid" |> to_string)

let create_permission pool db user body =
  require_user pool db user @@ fun ~db_rid ~user_rid ->
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      let id = member "id" json |> to_string in
      let mode = member "permissionMode" json |> to_string in
      let resource = member "resource" json |> to_string in
      let p_rid = Protocol.rid () in
      let doc =
        Protocol.permission ~id ~mode ~resource ~db_rid ~user_rid
          ~perm_rid:p_rid
          ~etag:(Protocol.quote_etag (Protocol.etag ()))
          ~ts:(Protocol.now ())
      in
      match
        Storage.doc_create pool ~db ~coll:(permissions_coll user) ~id ~pk:None
          ~doc ~upsert:false
      with
      | Error e -> db_error e
      | Ok `Conflict ->
          conflict (Printf.sprintf "Permission %s already exists" id)
      | Ok (`Ok (doc, _)) -> respond_json ~status:`Created doc)

let replace_permission pool db user perm body =
  require_user pool db user @@ fun ~db_rid ~user_rid ->
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      let coll = permissions_coll user in
      match Storage.doc_get pool ~db ~coll ~id:perm with
      | Error e -> db_error e
      | Ok None -> not_found (Printf.sprintf "Permission %s not found" perm)
      | Ok (Some _) -> (
          let id = member "id" json |> to_string in
          let mode = member "permissionMode" json |> to_string in
          let resource = member "resource" json |> to_string in
          let p_rid = Protocol.rid () in
          let doc =
            Protocol.permission ~id ~mode ~resource ~db_rid ~user_rid
              ~perm_rid:p_rid
              ~etag:(Protocol.quote_etag (Protocol.etag ()))
              ~ts:(Protocol.now ())
          in
          (* same rename-by-replace as users *)
          match
            Storage.with_txn pool @@ fun conn ->
            match
              ( Storage.doc_delete_conn conn ~db ~coll ~id:perm ~pk_header:None,
                Storage.doc_create_conn conn ~db ~coll ~id ~pk:None ~doc
                  ~upsert:false )
            with
            | Ok _, Ok r -> Ok r
            | Error e, _ -> Error e
            | _, Error e -> Error e
          with
          | Error e -> db_error e
          | Ok `Conflict ->
              conflict (Printf.sprintf "Permission %s already exists" id)
          | Ok (`Ok (doc, _)) -> respond_json ~status:`OK doc))

let replace_offer pool rid body =
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      match
        Storage.doc_replace pool ~db:offers_db ~coll:offers_coll ~id:rid
          ~pk:None ~doc:json
      with
      | Error e -> db_error e
      | Ok None -> not_found (Printf.sprintf "Offer %s not found" rid)
      | Ok (Some doc) -> respond_json ~status:`OK doc)

let query_offers pool req body =
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      let limit =
        int_header req "x-ms-max-item-count" |> Option.value ~default:100
      in
      match
        try
          let q = Query.Compile.parse (member "query" json |> to_string) in
          Ok
            (Query.Compile.compile ~params:(params_of_json json)
               ~fetch:(limit + 1) ~offset:0 q)
        with Query.Compile.Error e -> Error (bad_request e)
      with
      | Error r -> r
      | Ok sql -> (
          match Storage.doc_query pool ~db:offers_db ~coll:offers_coll ~sql with
          | Error e -> db_error e
          | Ok rows ->
              respond_json ~status:`OK
                (Protocol.feed ~rid:"" ~member:"Offers" rows)))

let get_pkranges pool db coll =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource ->
  respond_json ~status:`OK (Protocol.pkranges ~coll_rid:(coll_rid_of resource))

(* --- router --- *)

(* resource tokens are self-contained: sig = base64(mode|resource|nonce).
   A request bearing one must stay under the granted resource and mode *)
let check_resource_token meth path req =
  match header req "authorization" with
  | None -> `Allow
  | Some a -> (
      let tok = Uri.pct_decode a in
      match String.starts_with ~prefix:"type=resource" tok with
      | false -> `Allow
      | true -> (
          let grant =
            match
              tok |> String.split_on_char '&'
              |> List.find_opt (fun s -> String.starts_with ~prefix:"sig=" s)
            with
            | None -> None
            | Some s -> (
                let s = String.sub s 4 (String.length s - 4) in
                try Some (Base64.decode_exn s) with _ -> None)
          in
          match grant with
          | None -> `Deny
          | Some grant -> (
              match String.split_on_char '|' grant with
              | mode :: resource :: _ ->
                  let scoped = "/" ^ resource in
                  let read_verb =
                    match meth with
                    | `GET | `HEAD -> true
                    | `POST -> bool_header req "x-ms-documentdb-isquery"
                    | _ -> false
                  in
                  if
                    (mode = "All" || read_verb)
                    && (path = scoped
                       || String.starts_with ~prefix:(scoped ^ "/") path)
                  then `Allow
                  else `Deny
              | _ -> `Deny)))

let handle pool req body_reader =
  let uri = Http.Request.resource req |> Uri.of_string in
  let segs =
    Uri.path uri |> String.split_on_char '/' |> List.filter (fun s -> s <> "")
  in
  let meth = Http.Request.meth req in
  (* only read declared bodies; cohttp-eio reports an `Unknown` body when
     Content-Length/Transfer-Encoding are absent and a lazy read can
     wedge the connection *)
  let body =
    match meth with
    | `POST | `PUT | `PATCH -> (
        match (header req "content-length", header req "transfer-encoding") with
        | None, None -> ""
        | _ -> Eio.Buf_read.(of_flow ~max_size:max_int body_reader |> take_all))
    | _ -> ""
  in
  match check_resource_token meth (Uri.path uri) req with
  | `Deny ->
      error ~status:`Forbidden ~code:"Forbidden"
        ~message:"the resource token does not grant access to this resource"
  | `Allow -> (
      try
        match (meth, segs) with
        | `GET, [ "dbs" ] -> list_dbs pool req body
        | `POST, [ "dbs" ] -> create_db pool req body
        | `GET, [ "dbs"; db ] -> get_db pool db
        | `DELETE, [ "dbs"; db ] -> delete_db pool db
        | `GET, [ "dbs"; db; "colls" ] -> list_colls pool db req body
        | `POST, [ "dbs"; db; "colls" ] -> create_coll pool db req body
        | `GET, [ "dbs"; db; "colls"; coll ] -> get_coll pool db coll
        | `DELETE, [ "dbs"; db; "colls"; coll ] -> delete_coll pool db coll
        | `GET, [ "dbs"; db; "colls"; coll; "docs" ] ->
            if Option.is_some (header req "a-im") then
              change_feed pool db coll req
            else list_docs pool db coll req body
        | `POST, [ "dbs"; db; "colls"; coll; "docs" ] ->
            if bool_header req "x-ms-documentdb-isquery" then
              query_docs pool db coll req body
            else if
              bool_header req "x-ms-cosmos-is-batch-request"
              || bool_header req "x-ms-cosmos-batch-atomic"
            then
              batch_docs
                ~atomic:(bool_header req "x-ms-cosmos-batch-atomic")
                pool db coll req body
            else
              create_doc pool db coll
                ~upsert:(bool_header req "x-ms-documentdb-is-upsert")
                req body
        | `PATCH, [ "dbs"; db; "colls"; coll; "docs"; id ] ->
            patch_doc pool db coll id req body
        | `GET, [ "dbs"; db; "colls"; coll; "docs"; id ] ->
            get_doc pool db coll id req
        | `PUT, [ "dbs"; db; "colls"; coll; "docs"; id ] ->
            replace_doc pool db coll id req body
        | `DELETE, [ "dbs"; db; "colls"; coll; "docs"; id ] ->
            delete_doc pool db coll id req
        | `GET, [ "dbs"; db; "colls"; coll; "pkranges" ] ->
            get_pkranges pool db coll
        | `POST, [ "dbs"; db; "users" ] -> create_user pool db body
        | `GET, [ "dbs"; db; "users" ] ->
            list_resource pool ~db ~coll:users_coll ~member:"Users" ~rid:""
        | `GET, [ "dbs"; db; "users"; user ] ->
            get_resource pool ~db ~coll:users_coll ~id:user
        | `PUT, [ "dbs"; db; "users"; user ] -> replace_user pool db user body
        | `DELETE, [ "dbs"; db; "users"; user ] ->
            delete_resource pool ~db ~coll:users_coll ~id:user
        | `POST, [ "dbs"; db; "users"; user; "permissions" ] ->
            create_permission pool db user body
        | `GET, [ "dbs"; db; "users"; user; "permissions" ] ->
            list_resource pool ~db ~coll:(permissions_coll user)
              ~member:"Permissions" ~rid:""
        | `GET, [ "dbs"; db; "users"; user; "permissions"; perm ] ->
            get_resource pool ~db ~coll:(permissions_coll user) ~id:perm
        | `PUT, [ "dbs"; db; "users"; user; "permissions"; perm ] ->
            replace_permission pool db user perm body
        | `DELETE, [ "dbs"; db; "users"; user; "permissions"; perm ] ->
            delete_resource pool ~db ~coll:(permissions_coll user) ~id:perm
        | `POST, [ "offers" ] -> query_offers pool req body
        | `GET, [ "offers" ] ->
            list_resource pool ~db:offers_db ~coll:offers_coll ~member:"Offers"
              ~rid:""
        | `GET, [ "offers"; rid ] ->
            get_resource pool ~db:offers_db ~coll:offers_coll ~id:rid
        | `PUT, [ "offers"; rid ] -> replace_offer pool rid body
        | _ -> not_found "resource not found"
      with
      | Yojson.Json_error e -> bad_request ("invalid JSON: " ^ e)
      | Yojson.Safe.Util.Type_error (e, _) -> bad_request ("invalid JSON: " ^ e)
      | Query.Compile.Error e -> bad_request e
      | Caqti.Error.Exn e -> db_error e
      | e ->
          error ~status:`Internal_server_error ~code:"InternalError"
            ~message:(Printexc.to_string e))
