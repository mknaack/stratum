open Yojson.Safe.Util

let rng = Random.State.make_self_init ()
let rid () = Uuidm.v4_gen rng () |> Uuidm.to_string
let etag () = rid ()
let now () = int_of_float (Unix.gettimeofday ())
let etag_json e = `String ("\"" ^ e ^ "\"")
let quote_etag e = "\"" ^ e ^ "\""

let unquote_etag s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

let database_resource ~id ~rid ~etag ~ts =
  `Assoc
    [
      ("id", `String id);
      ("_rid", `String rid);
      ("_self", `String ("dbs/" ^ rid ^ "/"));
      ("_etag", etag_json etag);
      ("_colls", `String "colls/");
      ("_users", `String "users/");
      ("_ts", `Int ts);
    ]

let default_indexing_policy =
  `Assoc
    [
      ("automatic", `Bool true);
      ("indexingMode", `String "consistent");
      ("includedPaths", `List [ `Assoc [ ("path", `String "/*") ] ]);
      ("excludedPaths", `List []);
    ]

let collection_resource ~id ~db_rid ~rid ~etag ~ts ~indexing_policy
    ~partition_key =
  `Assoc
    [
      ("id", `String id);
      ( "indexingPolicy",
        Option.value indexing_policy ~default:default_indexing_policy );
      ( "partitionKey",
        partition_key |> Option.value ~default:(`Assoc [ ("paths", `List []) ])
      );
      ("_rid", `String rid);
      ("_self", `String ("dbs/" ^ db_rid ^ "/colls/" ^ rid ^ "/"));
      ("_etag", etag_json etag);
      ("_ts", `Int ts);
      ("_docs", `String "docs/");
      ("_sprocs", `String "sprocs/");
      ("_triggers", `String "triggers/");
      ("_udfs", `String "udfs/");
      ("_conflicts", `String "conflicts/");
    ]

let partition_key_result_of_paths paths =
  `Assoc
    [
      ("paths", `List (List.map (fun p -> `String p) paths));
      ("kind", `String "Hash");
      ("version", `Int 2);
    ]

let doc_meta ~db_rid ~coll_rid ~doc_rid ~etag ~ts =
  [
    ("_rid", `String doc_rid);
    ( "_self",
      `String ("dbs/" ^ db_rid ^ "/colls/" ^ coll_rid ^ "/docs/" ^ doc_rid ^ "/")
    );
    ("_etag", etag_json etag);
    ("_ts", `Int ts);
    ("_attachments", `String "attachments/");
  ]

let inject_doc_meta ~db_rid ~coll_rid ~doc_rid ~etag ~ts = function
  | `Assoc fields ->
      let meta = doc_meta ~db_rid ~coll_rid ~doc_rid ~etag ~ts in
      `Assoc (fields @ meta)
  | other -> other

let feed ~rid ~member items =
  `Assoc
    [
      ("_rid", `String rid);
      (member, `List items);
      ("_count", `Int (List.length items));
    ]

let error_json ~code ~message =
  `Assoc [ ("code", `String code); ("message", `String message) ]

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

let pkranges ~coll_rid =
  `Assoc
    [
      ("_rid", `String coll_rid);
      ( "PartitionKeyRanges",
        `List
          [
            `Assoc
              [
                ("id", `String "0");
                ("minInclusive", `String "");
                ("maxExclusive", `String "FF");
                ("_rid", `String "0");
                ("_ts", `Int (now ()));
                ("_self", `String "");
                ("_etag", etag_json "0");
              ];
          ] );
      ("_count", `Int 1);
    ]

let continuation_of_lsn lsn = Base64.encode_exn (Int64.to_string lsn)

let lsn_of_continuation s =
  match Base64.decode s with Ok s -> Int64.of_string_opt s | Error _ -> None
