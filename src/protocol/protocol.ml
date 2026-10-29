open Yojson.Safe.Util
open Ppx_yojson_conv_lib.Yojson_conv

(* free-form embedded JSON (indexingPolicy, partitionKey): the ppx expects a
   yojson_of_<typename> per field type, so alias with an identity serializer *)
module Batch_op = Batch_op

type json = Yojson.Safe.t

let yojson_of_json j = j
let rng = Random.State.make_self_init ()
let rid () = Uuidm.v4_gen rng () |> Uuidm.to_string
let etag () = rid ()
let now () = int_of_float (Unix.gettimeofday ())
let quote_etag e = "\"" ^ e ^ "\""

let unquote_etag s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

type database_resource = {
  id : string;
  rid : string; [@key "_rid"]
  self : string; [@key "_self"]
  etag : string; [@key "_etag"]
  colls : string; [@key "_colls"]
  users : string; [@key "_users"]
  ts : int; [@key "_ts"]
}
[@@deriving yojson_of]

let database_resource ~id ~rid ~etag ~ts =
  yojson_of_database_resource
    {
      id;
      rid;
      self = "dbs/" ^ rid ^ "/";
      etag = quote_etag etag;
      colls = "colls/";
      users = "users/";
      ts;
    }

let default_indexing_policy =
  `Assoc
    [
      ("automatic", `Bool true);
      ("indexingMode", `String "consistent");
      ("includedPaths", `List [ `Assoc [ ("path", `String "/*") ] ]);
      ("excludedPaths", `List []);
    ]

type collection_resource = {
  id : string;
  indexing_policy : json; [@key "indexingPolicy"]
  partition_key : json; [@key "partitionKey"]
  rid : string; [@key "_rid"]
  self : string; [@key "_self"]
  etag : string; [@key "_etag"]
  ts : int; [@key "_ts"]
  docs : string; [@key "_docs"]
  sprocs : string; [@key "_sprocs"]
  triggers : string; [@key "_triggers"]
  udfs : string; [@key "_udfs"]
  conflicts : string; [@key "_conflicts"]
}
[@@deriving yojson_of]

let collection_resource ~id ~db_rid ~rid ~etag ~ts ~indexing_policy
    ~partition_key =
  yojson_of_collection_resource
    {
      id;
      indexing_policy =
        Option.value indexing_policy ~default:default_indexing_policy;
      partition_key =
        partition_key |> Option.value ~default:(`Assoc [ ("paths", `List []) ]);
      rid;
      self = "dbs/" ^ db_rid ^ "/colls/" ^ rid ^ "/";
      etag = quote_etag etag;
      ts;
      docs = "docs/";
      sprocs = "sprocs/";
      triggers = "triggers/";
      udfs = "udfs/";
      conflicts = "conflicts/";
    }

type partition_key_result = {
  paths : string list;
  kind : string;
  version : int;
}
[@@deriving yojson_of]

let partition_key_result_of_paths paths =
  yojson_of_partition_key_result { paths; kind = "Hash"; version = 2 }

type doc_meta = {
  rid : string; [@key "_rid"]
  self : string; [@key "_self"]
  etag : string; [@key "_etag"]
  ts : int; [@key "_ts"]
  attachments : string; [@key "_attachments"]
}
[@@deriving yojson_of]

let doc_meta_json ~db_rid ~coll_rid ~doc_rid ~etag ~ts =
  yojson_of_doc_meta
    {
      rid = doc_rid;
      self = "dbs/" ^ db_rid ^ "/colls/" ^ coll_rid ^ "/docs/" ^ doc_rid ^ "/";
      etag = quote_etag etag;
      ts;
      attachments = "attachments/";
    }

let inject_doc_meta ~db_rid ~coll_rid ~doc_rid ~etag ~ts = function
  | `Assoc fields -> (
      match doc_meta_json ~db_rid ~coll_rid ~doc_rid ~etag ~ts with
      | `Assoc meta -> `Assoc (fields @ meta)
      | _ -> `Assoc fields)
  | other -> other

(* the member key varies per resource kind (Databases, Documents, ...), so a
   fixed record type does not fit — build the envelope as an assoc list *)
let feed ~rid ~member items =
  `Assoc
    [
      ("_rid", `String rid);
      (member, `List items);
      ("_count", `Int (List.length items));
    ]

type error_body = { code : string; message : string } [@@deriving yojson_of]

let error_json ~code ~message = yojson_of_error_body { code; message }

let member_opt name json =
  match member name json with `Null -> None | v -> Some v

(* partition key header value: JSON array ["user1"] or [5]; return canonical
   text form for comparison, as Yojson serialisation of the scalar *)
let pk_value_string = function
  | `String s -> s
  | other -> Yojson.Safe.to_string other

let parse_pk_header s =
  match Yojson.Safe.from_string s with
  | `List (v :: _) -> Some (pk_value_string v)
  | `List [] -> None
  | _ -> None
  | exception _ -> None

(* extract pk value of a doc given pk_path like "/a/b" *)
let pk_path_segments = function
  | "" -> []
  | p -> String.sub p 1 (String.length p - 1) |> String.split_on_char '/'

let pk_of_doc pk_path doc =
  let rec dig segs j =
    match segs with
    | [] -> j
    | s :: rest -> (
        match j with
        | Some (`Assoc a) -> dig rest (List.assoc_opt s a)
        | _ -> None)
  in
  match dig (pk_path_segments pk_path) (Some doc) with
  | Some j -> Some (pk_value_string j)
  | None -> None

type pkrange = {
  id : string;
  min_inclusive : string; [@key "minInclusive"]
  max_exclusive : string; [@key "maxExclusive"]
  rid : string; [@key "_rid"]
  ts : int; [@key "_ts"]
  self : string; [@key "_self"]
  etag : string; [@key "_etag"]
}
[@@deriving yojson_of]

let pkranges ~coll_rid =
  feed ~rid:coll_rid ~member:"PartitionKeyRanges"
    [
      yojson_of_pkrange
        {
          id = "0";
          min_inclusive = "";
          max_exclusive = "FF";
          rid = "0";
          ts = now ();
          self = "";
          etag = "\"0\"";
        };
    ]

let continuation_of_lsn lsn = Base64.encode_exn (Int64.to_string lsn)

let lsn_of_continuation s =
  match Base64.decode s with Ok s -> Int64.of_string_opt s | Error _ -> None

type batch_result = {
  status_code : int; [@key "statusCode"]
  request_charge : float; [@key "requestCharge"]
  e_tag : string option; [@key "eTag"]
  resource_body : json; [@key "resourceBody"]
}
[@@deriving yojson_of]

(* a single entry of a transactional-batch request's "operations" array *)
type batch_operation = {
  operation_type : Batch_op.t;
  resource_body : json;
  resource_id : string option;
  if_match : string option;
}

let batch_operation_of_yojson j =
  {
    operation_type = Batch_op.t_of_yojson (member "operationType" j);
    resource_body = member "resourceBody" j;
    resource_id = member "resourceId" j |> to_string_option;
    if_match = member "ifMatch" j |> to_string_option;
  }

(* "Fri, 23 Oct 2026 20:49:00 GMT" → epoch seconds for If-Modified-Since *)
let months =
  [|
    "Jan";
    "Feb";
    "Mar";
    "Apr";
    "May";
    "Jun";
    "Jul";
    "Aug";
    "Sep";
    "Oct";
    "Nov";
    "Dec";
  |]

let http_date_to_epoch s =
  let ( let* ) = Option.bind in
  match String.split_on_char ' ' (String.trim s) with
  | [ _dow; dom; mon; year; hms; _tz ] -> (
      let* dom = int_of_string_opt dom in
      let* year = int_of_string_opt year in
      let* mo =
        match List.find_index (String.equal mon) (Array.to_list months) with
        | Some i -> Some (i + 1)
        | None -> None
      in
      match String.split_on_char ':' hms with
      | [ h; m; sec ] ->
          let* h = int_of_string_opt h in
          let* m = int_of_string_opt m in
          let* sec = int_of_string_opt sec in
          let open Ptime in
          of_date_time ((year, mo, dom), ((h, m, sec), 0))
          |> Option.map (fun p -> Int64.of_float (to_float_s p))
      | _ -> None)
  | _ -> None
