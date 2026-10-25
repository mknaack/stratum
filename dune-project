(lang dune 3.17)

(using menhir 2.0)

(name stratum)

(generate_opam_files true)

(package
 (name stratum)
 (synopsis
  "Cosmos DB REST API-compatible document store backed by PostgreSQL")
 (depends
  ocaml
  dune
  eio
  eio_main
  cohttp-eio
  (caqti
   (>= 3.0))
  (caqti-eio
   (>= 3.0))
  (caqti-driver-pgx
   (>= 3.0))
  yojson
  ppx_yojson_conv
  uuidm
  base64
  ptime
  logs
  fmt
  uri
  menhir
  (ppx_expect :with-test)
  (ocamlformat
   (and
    :with-test
    (= 0.28.1)))))
