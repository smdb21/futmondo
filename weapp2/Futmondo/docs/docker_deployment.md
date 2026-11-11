# Docker deployment with Dokploy

The production image is built from `weapp2/Futmondo/Dockerfile`. The older
`weapp2/docker` directory describes a separate development container and has no
Dockerfile in this checkout.

## Dokploy application settings

Connect this repository and select branch `2.0` (or another branch containing
this Dockerfile). Configure both sections of the General tab, saving each:

- Provider Build Path: `/weapp2/Futmondo`.
- Build Type: Dockerfile.
- Dockerfile Path: `Dockerfile`.
- Docker Context Path: `.`.
- Docker Build Stage: leave empty.
- Replicas: start with one; scaling needs a separate session-routing review.

Dockerfile Path and Docker Context Path are relative to the Provider Build Path,
not always to the repository root. With the settings above, do not repeat
`weapp2/Futmondo` or use the old `weapp2/docker` directory in either field.
In the Environment tab, turn off **Create Environment File** (if offered), save,
and keep the application runtime environment variables configured there. This app
uses runtime variables and does not need a generated build-time `.env` file.

If cloning succeeds but deployment reports `cannot create .../.env: Directory
nonexistent`, the failure is before the Docker image build. A path such as
`code/weapp2/Futmondo/weapp2/docker/.env` indicates that the provider build directory
has been combined with a Dockerfile path for the old development directory.
Correct the paths above and redeploy; creating the missing nested directory would
leave the incorrect Dockerfile selection in place. Official examples use the
same subdirectory Build Path plus local Dockerfile convention:
https://docs.dokploy.com/docs/core/deno

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

## Mobile reconnection: retain the live session for 30 minutes

The Docker build customizes Shiny Server's existing-session reconnect window to
1,800 seconds, with a matching 1,800,000 ms browser retry window. No environment
variable or database migration is needed. Redeploy the application in Dokploy to
build the updated image; restarting the old image does not install this change.
The deploy itself closes sessions running in the old container.

When switching tabs or locking a phone breaks its connection, Shiny Server keeps
the same R session for up to 30 minutes after detecting the disconnection. If the
same page reconnects within the window, its login, selections and reactive state
can continue. If the phone discards/reloads the page, the container restarts, the
R process fails, or the window expires, that live state is lost. Remembered login
is a separate feature for restoring access to a new session.

This is a narrowly guarded upstream-source customization, not an
`app_idle_timeout` change. That directive only keeps an R process with no
connections warm and does not extend a disconnected session. The build helper
`scripts/patch-shiny-reconnect.cjs` patches the installed server's robust-connection
constructor and browser options in `R/SockJSAdapter.R`. The build runs
`test/test_shiny_reconnect.cjs` against the installed upstream code and fails if
its expected source contracts change. Build helpers/tests are removed from their
temporary location before application files are copied into the image. Keep this
check when updating the Rocker image or Shiny Server. See Posit's
[existing-session reconnection explanation](https://shiny.posit.co/r/articles/improve/reconnecting/).

Retaining a disconnected session retains its in-memory data and existing reactive
work. Shiny Server buffers unacknowledged outputs until reconnect acknowledgement
or expiry; it has no built-in byte cap on that buffer. The existing one-second
round countdown can queue approximately 1,800 updates in this window, and existing
notification timers can continue. This adds no new polling schedule but can
increase total CPU, requests and memory compared with releasing a session after
15 seconds. Existing browser reconnect attempts also continue longer, using the
upstream retry backoff while the browser is awake. Monitor memory and concurrent
sessions on the VM, especially with multiple disconnected phones. Successful
reusable API observations still belong
in the existing durable database; reconnection memory is not a persistent cache.

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
HAR captures, source backups, internal documentation, and development files from
the deployed app. Only the named reconnect build helper and its focused test are
also permitted in the build context; they are not deployed application files.

Dependency installation precedes the code copy so normal source updates reuse
the dependency layer. Build concurrency defaults to two jobs; set `NCPUS=1` as a
non-secret build argument if the VM has limited RAM. The reconnection change adds
no new reactive timers or polling but extends the lifetime of existing session
work and buffered outputs, as described above. Actual image build cost, server
memory, concurrent-session behavior, and hosted cookie flow must be measured on
the target server.


## Opt-in Python acquisition service

The updated Docker image also installs pinned Python dependencies and starts `/etc/services.d/futmondo-worker` under the existing `/init` supervisor as the unprivileged `shiny` user. No extra port, cron or Dokploy application is required. Apply the additive worker migration before setting the separate runtime encryption key and enabling accounts. The service starts automatically but never enrolls a user without consent. See [configuration, sizing, lifecycle and rollback](python_worker.md). A full Docker build/start remains a required target-host check; local interpreter/protocol tests do not replace it.
