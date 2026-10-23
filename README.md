# stratum

A Cosmos DB for NoSQL REST API-compatible document store backed by PostgreSQL.

Clients speaking the Cosmos REST protocol (e.g.
[`azure-cosmos-db`](https://github.com/mknaack/azure-cosmos-db)) can point at
stratum instead of Azure by changing only the endpoint.

See [PLAN.md](PLAN.md) for the implementation plan.
