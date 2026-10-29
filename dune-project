(lang dune 3.17)

(using menhir 2.0)

(name stratum)

(generate_opam_files true)

(maintainers "Morten Knaack <knaack@gmail.com>")

(authors "Morten Knaack <knaack@gmail.com>")

(source
 (github mknaack/stratum))

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
  caqti
  caqti-eio
  caqti-driver-pgx
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
    :with-dev-setup
    (= 0.28.1)))
  (ocaml-lsp-server :with-dev-setup)))
