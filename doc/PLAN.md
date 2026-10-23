# Stratum — implementation plan

## Goal

An OCaml/Eio web service that implements a subset of the Azure Cosmos DB for
NoSQL REST API, with PostgreSQL `jsonb` as the document store. A client using
the Cosmos REST protocol can switch between Azure Cosmos DB and stratum by
changing only the endpoint.

## Scope

**In scope (MVP)**
- Database CRUD: `POST/GET/DELETE /dbs[/{db}]`
- Collection CRUD: `POST/GET/DELETE /dbs/{db}/colls[/{coll}]` (accept and store
  `partitionKey` definition; single fake partition key range)
- Document CRUD: `POST/GET/PUT/DELETE /dbs/{db}/colls/{coll}/docs[/{id}]`
  - upsert via `x-ms-documentdb-is-upsert: true`
  - optimistic concurrency via `ETag` + `If-Match`/`If-None-Match`
    (412 precondition-failed semantics)
  - injected system properties: `id`, `_rid`, `_self`, `_etag`, `_ts`,
    `_attachments`, `_lsn`
  - response envelopes `{"_rid":..., "Documents":[...], "_count":N}` (and the
    `Databases`/`DocumentCollections` analogues for list endpoints)
  - paging via `x-ms-max-item-count` + opaque `x-ms-continuation` cursor
  - `x-ms-documentdb-partitionkey` request header (JSON array string)
- Cosmos SQL query subset via `POST` with
  `Content-Type: application/query+json` and `x-ms-documentdb-isquery: true`:
  projections (`c.f`, `VALUE`), `WHERE` with comparisons/`AND`/`OR`/`NOT`,
  `IN`, `BETWEEN`, `IS_DEFINED`, `ORDER BY`, `TOP`/`OFFSET LIMIT`, parameters
  (`@name` bound from `parameters`), and scalar builtins
  `CONTAINS`/`STARTSWITH`/`ENDSWITH`/`ARRAY_CONTAINS`
- Auth-agnostic: any `Authorization` header accepted (master-key, resource
  token, or AAD bearer all work — enables pure endpoint switching)
- Expected response headers: `x-ms-request-charge` (fixed `1`),
  `x-ms-activity-id`, `etag`, `x-ms-session-token`

**Designed for, implemented after MVP**
- `PATCH` operations (`{"condition":..., "operations":[{op,path,value}]}`,
  `application/json_patch+json`) → `jsonb_set`/`jsonb_delete` paths; false
  condition → 412
- Transactional batch (`POST` + `x-ms-cosmos-batch-atomic`) → single Postgres
  transaction (one partition key, matching Cosmos semantics)
- Change feed (`A-IM: Incremental feed`, `ETag` as continuation, 304 = caught
  up, `If-Modified-Since`) → `WHERE lsn > ?` over the `bigserial` column

**Out of scope (Cosmos-only; stub or reject)**
- RU billing / offers (`/offers` → canned 200)
- Real partitioning / partition splits (one `pkranges` entry; never emit 410)
- Consistency levels & session tokens (single-node Postgres ⇒ effectively
  strong; accept-and-ignore)
- Stored procedures, UDFs, triggers, attachments, conflicts feed, spatial
- Users & permissions (no-op stubs so client calls don't fail)

## Architecture

Single OCaml binary, Eio throughout:

| Role | Package |
|---|---|
| HTTP server | `cohttp-eio` (`Cohttp_eio.Server`, HTTP/1.1) |
| Postgres | `caqti` + `caqti-eio` + `caqti-driver-postgresql` (pooled) |
| JSON | `yojson` (+ `atd` where typed codecs help) |
| Cosmos SQL parser | `menhir` |
| System fields | `uuidm` (`_rid`/`_etag`), `base64` (continuation), `ptime` (`x-ms-date`) |
| Logging | `logs` + `fmt` |
| TLS (optional) | `tls-eio` + `mirage-crypto-rng-eio` |
| Tests | `ppx_expect` |

`Eio_main.run` + domain pool; one fiber per request; caqti calls stay
non-blocking; change-feed waits map to `Eio.Condition`.

### Repo layout

```
src/bin/        stratum CLI: --port --database-url [--tls-cert --tls-key]
src/protocol/   envelopes, system-prop injection, header helpers, error shape
src/storage/    caqti pool, schema migration, CRUD
src/query/      menhir lexer/parser → AST → Postgres SQL fragment compiler
                + in-process evaluator fallback
src/server/     cohttp-eio router + handlers
test/           ppx_expect
doc/            plans and design docs
docker/         compose: postgres:16 (+ stratum when built)
```

### Storage schema

```sql
CREATE TABLE collections (
  db text, coll text, pk_path text, rid text,
  PRIMARY KEY (db, coll));

CREATE TABLE docs (
  db text, coll text, id text,
  pk   text,                       -- doc #>> pk_path, denormalized
  doc  jsonb,                      -- incl. injected system props
  lsn  bigserial,                  -- continuation + future change feed
  ts   bigint,                     -- _ts
  etag uuid DEFAULT gen_random_uuid(),
  PRIMARY KEY (db, coll, id));
CREATE INDEX docs_pk ON docs (db, coll, pk);
```

### Query pipeline

1. `menhir` grammar parses the Cosmos SQL subset → typed AST.
2. Compiler lowers AST to a parameterized Postgres `WHERE`/`ORDER BY` fragment:
   `c.f` → `doc #> '{f}'`, comparisons → `=`, `<>`, `<`, `>`, `@>`-style
   containment; `STARTSWITH`/`CONTAINS`/`ENDSWITH` → `LIKE`;
   `ARRAY_CONTAINS` → `jsonb_exists`; `@p` → `$n` placeholders.
3. Constructs outside the subset fall back to in-process evaluation over
   scanned rows (correct but O(collection)); push-down expands over time.
4. Continuation = base64 opaque cursor (`db|coll|last_lsn`); stateless.

## Phases

0. **Scaffold** — repo skeleton: `dune-project`, `stratum.opam` with the dep
   list above, `bin/` hello server, `docker compose up postgres`, CI
   (build + expect tests + postgres service).
1. **Storage + protocol** — schema/migration, system-prop injection, etag
   semantics, envelope shaping; expect tests.
2. **REST CRUD** — dbs/colls/docs handlers incl. upsert, replace, delete,
   list+continuation; 404/409/412 error paths; smoke-tested end-to-end with
   the `azure-cosmos-db` OCaml client pointed at stratum.
3. **Queries** — menhir subset → SQL, parameters, `ORDER BY`/`TOP`, paging;
   fallback evaluator for unsupported shapes.
4. **Conformance** — run `azure-cosmos-db`'s integration suite with
   `AZURE_COSMOS_ENDPOINT`/`AZURE_COSMOS_KEY` pointed at stratum; fix gaps
   until the basic document suite passes.
5. **Extensions (later sessions)** — patch, atomic batch, change feed,
   optional master-key HMAC verification, TLS.

## Open decisions

- **TLS**: serve plain HTTP and document TLS termination at a reverse proxy
  (recommended; simplest ops story) vs `tls-eio` self-signed built in. The
  client may also need an `http://` endpoint path for local runs.
- **Query fallback**: accept O(collection) evaluation for unsupported
  constructs, or reject with a clear `x-ms-substatus` error? Leaning to
  fallback — protocol completeness beats performance early on.

## Testing

- `ppx_expect` throughout: parser AST snapshots, SQL fragment compilation,
  envelope/system-prop shaping, error bodies, storage behavior against real
  Postgres (compose service in CI).
- Conformance: `azure-cosmos-db`'s own integration suite run against stratum —
  the definition of "endpoint-swap compatible".
