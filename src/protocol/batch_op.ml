type t = Create | Upsert | Replace | Delete | Read | Patch

let of_string = function
  | "Create" -> Some Create
  | "Upsert" -> Some Upsert
  | "Replace" -> Some Replace
  | "Delete" -> Some Delete
  | "Read" -> Some Read
  | "Patch" -> Some Patch
  | _ -> None

let t_of_yojson j =
  match j with
  | `String s -> (
      match of_string s with
      | Some t -> t
      | None ->
          raise
            (Yojson.Safe.Util.Type_error ("unsupported batch operation " ^ s, j))
      )
  | _ ->
      raise
        (Yojson.Safe.Util.Type_error ("batch operationType must be a string", j))
