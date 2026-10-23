let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let port = ref 8080 in
  let db_url = ref "pgx://postgres:stratum@127.0.0.1:5432/stratum" in
  Arg.parse
    [
      ("--port", Arg.Set_int port, "listen port (default 8080)");
      ("--database-url", Arg.Set_string db_url, "postgres URI (pgx:// scheme)");
    ]
    (fun _ -> ())
    "stratum — Cosmos DB REST API over PostgreSQL";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  match Storage.connect ~sw ~stdenv !db_url with
  | Error e ->
      Logs.err (fun m ->
          m "cannot connect to %s: %s" !db_url (Caqti.Error.show e))
  | Ok pool -> (
      match Storage.migrate pool with
      | Error e ->
          Logs.err (fun m -> m "migration failed: %s" (Caqti.Error.show e))
      | Ok () ->
          let server =
            Cohttp_eio.Server.make
              ~callback:(fun _conn req body -> Server.handle pool req body)
              ()
          in
          let socket =
            Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:64
              (`Tcp (Eio.Net.Ipaddr.V4.any, !port))
          in
          Logs.info (fun m -> m "stratum listening on :%d" !port);
          Cohttp_eio.Server.run
            ~on_error:(fun e ->
              Logs.warn (fun m -> m "connection error: %a" Fmt.exn e))
            socket server)
