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

let parse_body s =
  match Yojson.Safe.from_string s with
  | `Assoc _ as j -> Ok j
  | _ -> Error (bad_request "request body must be a JSON object")
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

let create_coll pool db _req body =
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
              | Ok `Created -> respond_json ~status:`Created resource)))

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

let coll_rid_of resource = resource |> member "_rid" |> to_string

(* Cosmos parameter names include the leading @; the AST drops it *)
let param_name n =
  if String.starts_with ~prefix:"@" n then String.sub n 1 (String.length n - 1)
  else n

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
          | Ok (`Ok doc) ->
              let et = Yojson.Safe.Util.(doc |> member "_etag" |> to_string) in
              respond_json ~extra:[ ("etag", et) ] ~status:`Created doc))

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
  let cursor =
    match header req "if-none-match" with
    | Some s -> Some (Protocol.unquote_etag s)
    | None -> header req "x-ms-continuation"
  in
  let after =
    match cursor with
    | None -> Ok 0L
    | Some s -> (
        match Protocol.lsn_of_continuation s with
        | Some l -> Ok l
        | None -> Error (bad_request "invalid continuation token"))
  in
  match after with
  | Error r -> r
  | Ok after -> (
      let since =
        match header req "if-modified-since" with
        | Some s -> Protocol.http_date_to_epoch s
        | None -> None
      in
      let limit =
        Option.value (int_header req "x-ms-max-item-count") ~default:100
      in
      match
        Storage.doc_list pool ~db ~coll ~after ~fetch:(limit + 1) ~since ()
      with
      | Error e -> db_error e
      | Ok [] -> respond ~headers:(json_headers []) ~status:`Not_modified ""
      | Ok rows ->
          let page =
            if List.length rows > limit then
              List.filteri (fun i _ -> i < limit) rows
            else rows
          in
          let last_lsn = fst (List.nth page (List.length page - 1)) in
          let cont = Protocol.continuation_of_lsn last_lsn in
          respond_json
            ~extra:[ ("etag", cont); ("x-ms-continuation", cont) ]
            ~status:`OK
            (Protocol.feed ~rid:(coll_rid_of resource) ~member:"Documents"
               (List.map snd page)))

let patch_op_of_json o =
  let s = member "op" o |> to_string in
  let op =
    match Query.Compile.patch_kind_of_string s with
    | Some k -> k
    | None -> Query.Compile.error "unsupported patch op %s" s
  in
  {
    Query.Compile.op;
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
              let e = Query.Compile.patch_set ops in
              Query.Compile.set_field e "_etag"
                (`String (Protocol.quote_etag (Protocol.etag ())))
              |> fun e ->
              Query.Compile.set_field e "_ts" (`Int (Protocol.now ()))
            in
            let cond_sql =
              Protocol.member_opt "condition" json
              |> Option.map (fun c -> Query.Compile.compile_where (to_string c))
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

exception Batch_abort of (Cohttp_eio.Server.writer -> unit)

type batch_op = Create | Upsert | Replace | Delete | Read | Patch

let batch_op_of_string = function
  | "Create" -> Some Create
  | "Upsert" -> Some Upsert
  | "Replace" -> Some Replace
  | "Delete" -> Some Delete
  | "Read" -> Some Read
  | "Patch" -> Some Patch
  | _ -> None

(* x-ms-cosmos-batch-atomic: every op in one Postgres transaction; the whole
   batch rolls back on the first failure *)
let batch_docs pool db coll _req body =
  require_coll pool db coll @@ fun ~pk_path ~resource ->
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      let ops = member "operations" json |> to_list in
      match ops with
      | [] -> bad_request "batch requires at least one operation"
      | _ -> (
          let coll_rid = coll_rid_of resource in
          let pk_of_body b =
            match pk_path with Some p -> Protocol.pk_of_doc p b | None -> None
          in
          (* every operation must target the same partition key *)
          let pks =
            List.filter_map
              (fun o ->
                Option.bind (Protocol.member_opt "resourceBody" o) pk_of_body)
              ops
            |> List.sort_uniq String.compare
          in
          match pks with
          | _ :: _ :: _ ->
              bad_request "batch operations must share a partition key"
          | _ -> (
              let pk0 = List.nth_opt pks 0 in
              match Storage.db_rid pool ~id:db with
              | Error e -> db_error e
              | Ok None -> not_found (Printf.sprintf "Database %s not found" db)
              | Ok (Some db_rid) -> (
                  let abort r = raise (Batch_abort r) in
                  let result ~code ?body e_tag =
                    Protocol.yojson_of_batch_result
                      {
                        status_code = code;
                        request_charge = 1.0;
                        e_tag;
                        resource_body = Option.value body ~default:`Null;
                      }
                  in
                  let check_if_match conn o id =
                    match
                      Protocol.member_opt "ifMatch" o |> Option.map to_string
                    with
                    | None -> true
                    | Some m -> (
                        match Storage.doc_get_conn conn ~db ~coll ~id with
                        | Ok (Some doc) ->
                            let cur =
                              doc |> member "_etag" |> to_string
                              |> Protocol.unquote_etag
                            in
                            cur = Protocol.unquote_etag m
                        | _ -> false)
                  in
                  let run_op conn o =
                    let rbody = member "resourceBody" o in
                    let rid_opt = member "resourceId" o |> to_string_option in
                    match
                      batch_op_of_string (member "operationType" o |> to_string)
                    with
                    | Some ((Create | Upsert) as ty) -> (
                        let id = member "id" rbody |> to_string in
                        let doc =
                          Protocol.inject_doc_meta ~db_rid ~coll_rid
                            ~doc_rid:(Protocol.rid ()) ~etag:(Protocol.etag ())
                            ~ts:(Protocol.now ()) rbody
                        in
                        match
                          Storage.doc_create_conn conn ~db ~coll ~id
                            ~pk:(pk_of_body rbody) ~doc ~upsert:(ty = Upsert)
                        with
                        | Ok (`Ok doc) ->
                            let et = doc |> member "_etag" |> to_string in
                            result ~code:201 ~body:doc (Some et)
                        | Ok `Conflict ->
                            abort
                              (conflict
                                 (Printf.sprintf "Document %s already exists" id))
                        | Error e -> abort (db_error e))
                    | Some Replace -> (
                        let id =
                          match rid_opt with
                          | Some id -> id
                          | None -> member "id" rbody |> to_string
                        in
                        if not (check_if_match conn o id) then
                          abort (precondition_failed "If-Match did not match")
                        else
                          let doc =
                            Protocol.inject_doc_meta ~db_rid ~coll_rid
                              ~doc_rid:(Protocol.rid ())
                              ~etag:(Protocol.etag ()) ~ts:(Protocol.now ())
                              rbody
                          in
                          match
                            Storage.doc_replace_conn conn ~db ~coll ~id
                              ~pk:(pk_of_body rbody) ~doc
                          with
                          | Ok (Some doc) ->
                              let et = doc |> member "_etag" |> to_string in
                              result ~code:200 ~body:doc (Some et)
                          | Ok None ->
                              abort
                                (not_found
                                   (Printf.sprintf "Document %s not found" id))
                          | Error e -> abort (db_error e))
                    | Some Delete -> (
                        let id =
                          match rid_opt with
                          | Some id -> id
                          | None ->
                              abort (bad_request "Delete needs resourceId")
                        in
                        if not (check_if_match conn o id) then
                          abort (precondition_failed "If-Match did not match")
                        else
                          match
                            Storage.doc_delete_conn conn ~db ~coll ~id
                              ~pk_header:pk0
                          with
                          | Ok true -> result ~code:204 None
                          | Ok false ->
                              abort
                                (not_found
                                   (Printf.sprintf "Document %s not found" id))
                          | Error e -> abort (db_error e))
                    | Some Read -> (
                        let id =
                          match rid_opt with
                          | Some id -> id
                          | None -> abort (bad_request "Read needs resourceId")
                        in
                        match Storage.doc_get_conn conn ~db ~coll ~id with
                        | Ok (Some doc) ->
                            let et = doc |> member "_etag" |> to_string in
                            result ~code:200 ~body:doc (Some et)
                        | Ok None ->
                            abort
                              (not_found
                                 (Printf.sprintf "Document %s not found" id))
                        | Error e -> abort (db_error e))
                    | Some Patch -> (
                        let id =
                          match rid_opt with
                          | Some id -> id
                          | None -> member "id" rbody |> to_string
                        in
                        if not (check_if_match conn o id) then
                          abort (precondition_failed "If-Match did not match")
                        else
                          let pops =
                            member "operations" rbody |> to_list
                            |> List.map patch_op_of_json
                          in
                          let set_sql = Query.Compile.patch_set pops in
                          let cond_sql =
                            Protocol.member_opt "condition" rbody
                            |> Option.map (fun c ->
                                Query.Compile.compile_where (to_string c))
                          in
                          match
                            Storage.doc_patch_conn conn ~db ~coll ~id
                              ~pk_header:pk0 ~set_sql ~cond_sql
                          with
                          | Ok (Some doc) ->
                              let et = doc |> member "_etag" |> to_string in
                              result ~code:200 ~body:doc (Some et)
                          | Ok None ->
                              abort
                                (precondition_failed
                                   "PATCH condition evaluated to false")
                          | Error e -> abort (db_error e))
                    | None ->
                        abort
                          (bad_request
                             ("unsupported batch operation "
                             ^ (member "operationType" o |> to_string)))
                  in
                  try
                    match
                      Storage.with_txn pool @@ fun conn ->
                      List.fold_left
                        (fun acc o ->
                          match acc with
                          | Error _ as e -> e
                          | Ok acc -> (
                              try
                                let r = run_op conn o in
                                Ok (r :: acc)
                              with Query.Compile.Error e ->
                                abort (bad_request e)))
                        (Ok []) ops
                      |> Result.map List.rev
                    with
                    | Ok results -> respond_json ~status:`OK (`List results)
                    | Error e -> db_error e
                  with Batch_abort resp -> resp))))

let delete_doc pool db coll id req =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource:_ ->
  match Storage.doc_delete pool ~db ~coll ~id ~pk_header:(pk_of_req req) with
  | Error e -> db_error e
  | Ok false -> not_found (Printf.sprintf "Document %s not found" id)
  | Ok true -> respond ~headers:(json_headers []) ~status:`No_content ""

let query_docs pool db coll req body =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource ->
  match parse_body body with
  | Error r -> r
  | Ok json -> (
      match continuation req with
      | Error r -> r
      | Ok after -> (
          let sql_text = member "query" json |> to_string in
          let params =
            member "parameters" json |> to_list
            |> List.filter_map (fun p ->
                match
                  (member "name" p |> to_string_option, member "value" p)
                with
                | Some n, `String v -> Some (param_name n, v)
                | Some n, v -> Some (param_name n, Yojson.Safe.to_string v)
                | _ -> None)
          in
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

let get_pkranges pool db coll =
  require_coll pool db coll @@ fun ~pk_path:_ ~resource ->
  respond_json ~status:`OK (Protocol.pkranges ~coll_rid:(coll_rid_of resource))

(* --- router --- *)

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
        if Option.is_some (header req "a-im") then change_feed pool db coll req
        else list_docs pool db coll req body
    | `POST, [ "dbs"; db; "colls"; coll; "docs" ] ->
        if bool_header req "x-ms-documentdb-isquery" then
          query_docs pool db coll req body
        else if bool_header req "x-ms-cosmos-batch-atomic" then
          batch_docs pool db coll req body
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
    | `GET, [ "dbs"; _db; "users" ] | `GET, [ "dbs"; _db; "colls"; _; "users" ]
      ->
        respond_json ~status:`OK (Protocol.feed ~rid:"" ~member:"Users" [])
    | `GET, [ "dbs"; _db; "colls"; _; "permissions" ] ->
        respond_json ~status:`OK
          (Protocol.feed ~rid:"" ~member:"Permissions" [])
    | `GET, [ "offers" ] ->
        respond_json ~status:`OK (Protocol.feed ~rid:"" ~member:"Offers" [])
    | _ -> not_found "resource not found"
  with
  | Yojson.Json_error e -> bad_request ("invalid JSON: " ^ e)
  | Yojson.Safe.Util.Type_error (e, _) -> bad_request ("invalid JSON: " ^ e)
  | Query.Compile.Error e -> bad_request e
  | Caqti.Error.Exn e -> db_error e
  | e ->
      error ~status:`Internal_server_error ~code:"InternalError"
        ~message:(Printexc.to_string e)
