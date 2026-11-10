# Docker deployment with Dokploy

The production image is built from `weapp2/Futmondo/Dockerfile`. The older
`weapp2/docker` directory describes a separate development container and has no
Dockerfile in this checkout.

## Dokploy application settings

Connect this repository and its intended branch, then choose the Dockerfile build
type with these paths relative to the repository root:

- Dockerfile Path: `weapp2/Futmondo/Dockerfile`
- Docker Context Path: `weapp2/Futmondo`
- Docker Build Stage: leave empty.
- Replicas: start with one; scaling needs a separate session-routing review.

In the application's Domains tab, set Host to your actual main domain (for example
`yourdomain.com`), Path to `/`, Container Port to `3838`, HTTPS enabled, and
certificate provider to Let's Encrypt. Leave Internal Path empty and Strip Path
disabled. Point the domain's DNS A record (`@` at most providers) to the Contabo
VM's public IPv4 address. DNS records contain no port or URL path. Ensure existing
AAAA records, if any, also lead to this server or remove obsolete records. Ports
80 and 443 must be reachable for the default HTTP/HTTPS configuration; do not
publish port 3838 separately. Configure the Dokploy panel on a separate hostname.

`shiny-server.conf` serves `/srv/shiny-server/futmondo` directly at `/` using
`app_dir`, so the app is available at `https://yourdomain.com/` with no `/myapp/`
prefix. Commit/push changes, then deploy. Domain settings for Dokploy Applications
are applied without a rebuild; Dockerfile or Shiny configuration changes require
a deployment.

## Configuration and persistence

Set the existing server-only environment variables from the README in Dokploy's
application Environment tab, including the database URL and server-role key.
Never use Docker build arguments, image layers, or repository files for secrets.
The image retains Rocker's `/init` startup, which makes runtime environment
variables available to Shiny's R processes. Keep remembered login disabled until
the HTTPS cookie flow has been checked on the deployed hostname; preserve the
same encryption key across deployments if the feature is enabled later.

The existing database remains the durable store for reusable API observations,
context, timestamps, and completeness metadata. Container memory and files are
not durable caches. This deployment does not migrate or reset the database,
install scheduled workers, or alter factual calculations. Apply any required
existing database migrations separately as documented in the README.

## Build and verify

From the repository root:

```sh
docker build -f weapp2/Futmondo/Dockerfile -t futmondo:local weapp2/Futmondo
docker run --rm -p 127.0.0.1:3838:3838 futmondo:local
```

Open `http://127.0.0.1:3838/` to check the login page. Test deployment first with no
real account operations. In Dokploy, verify the public homepage, a Shiny WebSocket
connection, navigation after a private test login, and HTTPS before directing
normal users to the new deployment. Inspect application logs for missing packages
or startup errors; never publish private request payloads or credentials.

R is pinned to 4.6.1 to match the current manifest. Runtime packages are installed
from the base image's configured CRAN repository at build time; this is not an
exact restoration of every manifest package version. No application dependency
has been added. The `.dockerignore` allowlist excludes legacy projects, secrets,
HAR captures, source backups, tests, internal documentation, and development
scripts from the build context and deployed app.

Dependency installation precedes the code copy so normal source updates reuse
the dependency layer. Build concurrency defaults to two jobs; set `NCPUS=1` as a
non-secret build argument if the VM has limited RAM. This configuration adds no
API requests, polling, database queries, or reactive redraws. Actual image build
cost, server memory, concurrent-session behavior, and hosted cookie flow must be
measured on the target server.
