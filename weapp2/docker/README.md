# docker_shiny (ProteoBrowser)

YatiriBio's Shiny application Docker image with Bioconductor 3.23, ProteoBrowser dependencies, and R packages for visualization and data analysis.

### Vertex AI Setup for opencode Agents

To utilize Google Vertex AI capabilities within your `opencode` environment in this container, you need to configure your Google Cloud credentials:

1. **Locate your GCP Service Account credentials JSON file** on your host machine (e.g., `aml-sensitivity-landscape-8e9c3da54265.json`).
2. **Copy and rename** this JSON file to the root of your mapped workspace directory on your host (which maps to `/home/rstudio/workspace` in the container).
3. Ensure it is named exactly:
   ```
   /home/rstudio/workspace/vertex_ai_credentials.json
   ```

This configuration aligns with the environment settings:
- `GOOGLE_APPLICATION_CREDENTIALS`: `/home/rstudio/workspace/vertex_ai_credentials.json`
- `GOOGLE_CLOUD_PROJECT`: `aml-sensitivity-landscape`
- `VERTEX_LOCATION`: `global`

## Quick Start

### Prerequisites
- Docker and Docker Compose
- GitHub Personal Access Token (required for YBComponents installation)

### Build the image

#### Local build (with GitHub token)

```bash
docker build \
  --build-arg GITHUB_TOKEN=$(gh auth token) \
  -t yatiribio/proteobrowser:bioc_3.23 .
```

#### Building on Docker Hub / CI/CD

## Setting Up GitHub Actions for Automated Builds

This guide walks you through setting up a GitHub Actions workflow to automatically build and push the image to Docker Hub.

### Step 1: Set up GitHub Secrets

1. Go to your GitHub repository → **Settings** → **Secrets and variables** → **Actions**
2. Click **New repository secret** and add:
   - **Name:** `DOCKER_USERNAME` | **Value:** Your Docker Hub username
   - **Name:** `DOCKER_PASSWORD` | **Value:** Your Docker Hub Personal Access Token (NOT your password)
   - **Name:** `PAT_GITHUB` | **Value:** Your GitHub Personal Access Token with `repo` scope

**How to create these tokens:**
- **Docker Hub PAT:** https://app.docker.com/settings/personal-access-tokens → Generate new token → Copy
- **GitHub PAT:** https://github.com/settings/tokens → Generate new token (classic) → Select `repo` scope → Copy

**Note:** GitHub reserves secret names starting with `GITHUB_`, so we use `PAT_GITHUB` instead.

### Step 2: Create the GitHub Actions Workflow

The `.github/workflows/build-docker-image.yml` is already configured in the repository. It:
- Automatically triggers on commits to `main` or `bioc_*` branches
- Uses `PAT_GITHUB` secret to install YBComponents from GitHub
- Tags images dynamically: `main` → `latest`, `bioc_3.23` → `bioc_3.23`

If you need to manually trigger or view the workflow:

1. Go to **Actions** tab → **Build and Push Docker Image**
2. Click **Run workflow** to trigger manually (optional)

The workflow file uses:
```yaml
build-args: |
  GITHUB_TOKEN=${{ secrets.PAT_GITHUB }}
```

This securely passes your token as a build argument without exposing it in logs or Docker Hub.

### Step 3: Understand the Workflow

| Component | Behavior |
|-----------|----------|
| **Branch triggers** | `main` (tag: `latest`), `bioc_*` branches (tag: `bioc_3.23`, etc.) |
| **Path filters** | Only rebuild if `Dockerfile` or `scripts/` changes |
| **Manual trigger** | `workflow_dispatch` — run manually from Actions tab |
| **Secrets** | Uses `DOCKER_USERNAME`, `DOCKER_PASSWORD`, `PAT_GITHUB` |
| **Build args** | Passes `GITHUB_TOKEN=${{ secrets.PAT_GITHUB }}` securely |
| **Caching** | GitHub Actions layer cache for faster builds |

### Step 4: Run the Workflow

**Automatically:**
- Whenever you push to `main` or a `bioc_*` branch, the workflow automatically runs
- GitHub builds and pushes to Docker Hub with the correct tag

**Manually:**
- Go to **Actions** → **Build and Push Docker Image** → **Run workflow** → Select branch → **Run workflow**
- Check the logs in real-time

**Tag mapping:**
```
main branch     → proteobrowser:latest
bioc_3.23       → proteobrowser:bioc_3.23
bioc_3.24       → proteobrowser:bioc_3.24
```

### Step 5: Verify the Build

1. Go to **Actions** tab in your GitHub repository
2. Click the latest workflow run to see logs
3. Check [Docker Hub](https://hub.docker.com/r/your-username/proteobrowser) to confirm the image was pushed
4. Pull and test locally:
   ```bash
   docker pull your-username/proteobrowser:bioc_3.23
   docker run -it your-username/proteobrowser:bioc_3.23 R --version
   ```

### Workflow Tips

- **Skip a build:** Add `[skip ci]` to your commit message: `git commit -m "Update docs [skip ci]"`
- **Debug logs:** Expand job steps in the Actions tab to see detailed build output
- **GitHub Actions cache:** The workflow caches Docker layers, speeding up subsequent builds
- **Schedule builds:** Add `schedule:` trigger to rebuild weekly:
  ```yaml
  schedule:
    - cron: '0 0 * * 0'  # Weekly on Sunday
  ```

### Troubleshooting

| Issue | Solution |
|-------|----------|
| **Auth failed** | Verify `DOCKER_PASSWORD` is a PAT, not your Docker Hub password |
| **GITHUB_TOKEN error** | Ensure GitHub PAT has `repo` scope |
| **Image not pushed** | Check `DOCKER_USERNAME` matches your Docker Hub username |
| **Build times out** | Increase `timeout-minutes` in workflow or optimize Dockerfile |
| **Permission denied** | Verify GitHub PAT has sufficient scopes (`repo`, `write:packages`) |

### Advanced: Matrix Builds

To build multiple images or architectures:

```yaml
strategy:
  matrix:
    image: [bioc_3.23, bioc_3.24]

with:
  tags: ${{ secrets.DOCKER_USERNAME }}/proteobrowser:${{ matrix.image }}
  build-args: BIOCONDUCTOR_VERSION=${{ matrix.image }}
```

### Run with docker-compose

```bash
docker compose up -d
```

The Shiny application will be available at the configured port (see `docker-compose.yml`).

### Run with docker directly

```bash
docker run \
  -p 3838:3838 \
  -v $(pwd)/app:/srv/shiny-server/app \
  yatiribio/proteobrowser:bioc_3.23
```

## Using in VSCode/Positron Dev Containers

The `.devcontainer/devcontainer.json` is configured to build the image from the Dockerfile and automatically pass the GitHub token.

### Setup

1. **Export your GitHub token** (or use `gh auth token`):

```bash
export GITHUB_TOKEN=$(gh auth token)
```

2. **In VSCode/Positron**, open the folder and:
   - Click "Reopen in Container" (or "Rebuild and Reopen" if you've made changes)
   - The container will automatically read `GITHUB_TOKEN` from your environment and pass it during the build
   - This happens automatically via `${localEnv:GITHUB_TOKEN}` in the devcontainer.json

### Alternative: Set GITHUB_TOKEN in `.env`

If you prefer not to export the token each time, create a `.env` file in the workspace root:

```bash
GITHUB_TOKEN=your_github_token_here
```

VSCode will automatically load it when building the container. (Note: **Do not commit `.env`** to git!)

### What happens

- VSCode/Positron reads the Dockerfile from the parent directory
- Passes `GITHUB_TOKEN` as a build argument: `docker build --build-arg GITHUB_TOKEN=$GITHUB_TOKEN`
- Builds the image and opens the container
- **The token is never stored in the image** — only used during build to fetch YBComponents from GitHub
## Image Details

### Base
- **Bioconductor version**: 3.23 (`yatiribio_docker_base:bioc_3.23`)
- **OS**: Ubuntu 24.04 LTS (noble)

### Installed Tools & Languages
Inherits from `yatiribio_docker_base`, plus:
- **npm**: Node Package Manager (for JavaScript dependencies)
- **SQLite3**: Lightweight database
- **Development libraries**: `libltdl7`, `libltdl-dev` (for dynamic linking)

### R Packages

**Shiny & Dashboards:**
- `shiny`, `shiny.telemetry`, `shinydashboard`, `shinydashboardPlus`
- `shinyWidgets`, `shinycssloaders`, `shinybusy`, `shinyjs`, `shinyBS`, `shinyalert`, `shinyvalidate`
- `shinytest`, `shiny.tailwind`, `shinyFiles`, `shinydisconnect`

**Data Tables & Reactivity:**
- `reactable`, `rhandsontable`, `pins`, `waiter`, `reactlog`

**Visualization:**
- `ggstatsplot`, `ggpmisc`, `GGally`, `patchwork`, `ggnewscale`
- `plotly`, `echarts4r`, `apexcharter`, `networkD3`, `visNetwork`
- `DiagrammeR`, `RCircos`, `ggpolypath`

**Data Analysis & Statistics:**
- `ActivePathways`, `clusterProfiler`, `enrichplot`, `pROC`
- `factoextra`, `gmodels`, `fossil`, `zoo`, `Metrics`, `lineup`
- `RColorBrewer`, `colourpicker`, `sparkline`, `venn`, `rpart.plot`
- `DescTools`, `vcd`, `rcompanion`, `survminer`
- `FNN`, `mclust`, `aricode`, `mirai`

**Data Formats:**
- `xtable`, `markdown`, `RJSONIO`, `rvest`, `selectr`, `feather`, `nanoparquet`

**Bioconductor Packages:**
- Statistics: `MSstatsTMT`, `MSstatsPTM`, `MSstatsSampleSize`, `IHW`
- Omics: `mixOmics`, `clusterProfiler`, `ReactomePA`, `enrichplot`
- Annotation: `org.Hs.eg.db`, `KEGGREST`, `DO.db`, `blacksheepr`
- Data: `DOSE`, `TCGA` suite (`RTCGAToolbox`, `cBioPortalData`, `curatedTCGAData`, `TCGAutils`)
- Infrastructure: `MultiAssayExperiment`, `RaggedExperiment`, `SummarizedExperiment`
- Sequence: `Biostrings`, `GenomicFeatures`, `GenomicDataCommons`, `GenomicAlignments`, `XVector`, `GenomeInfoDb`, `GenomeInfoDbData`
- Database: `AnnotationHub`, `ExperimentHub`
- Utilities: `BiocBaseUtils`, `BiocIO`, `HDF5Array`, `zlibbioc`, `AnVIL`

**YatiriBio Custom Packages:**
- `YBComponents` (installed via GitHub with secure token handling)
- `HDO.db` (installed via GitHub)

### Python Packages
See `scripts/requirements.txt` for the full list. Typically includes:
- `plotly`, `kaleido` (for interactive plotting and static export)
- `pandas`, `numpy`, and other data science packages

### System Dependencies
Inherits all bioinformatics tools from `yatiribio_docker_base`, plus:
- `npm` — Node Package Manager
- `libltdl` libraries — Required for certain R package C bindings
- `sqlite3` — Lightweight relational database

## Security

### GitHub Token Handling

The Dockerfile no longer exposes credentials in plaintext. Instead:

1. **At build time**, pass your token as a build argument:
   ```bash
   docker build --build-arg GITHUB_TOKEN=$GITHUB_TOKEN -t ... .
   ```

2. **The token is used only during build** to install `YBComponents` from GitHub and is **not stored** in the final image.

3. **Best practice**: Use `gh auth token` to automatically get your current GitHub CLI token:
   ```bash
   docker build --build-arg GITHUB_TOKEN=$(gh auth token) -t ... .
   ```

### Creating a GitHub Token

If you don't have a Personal Access Token (PAT):

1. Go to [github.com/settings/tokens](https://github.com/settings/tokens)
2. Click "Generate new token" → "Generate new token (classic)"
3. Give it a name (e.g., "Docker Build")
4. Select scopes: `repo` (full control of private repositories)
5. Click "Generate token" and copy it
6. Store it securely (e.g., in `.env` or pass via `gh auth login`)

## Configuration

### RStudio Settings
The image includes all settings from `yatiribio_docker_base`:
- GitHub Copilot enabled
- Default CRAN mirror set to Posit Package Manager

### Docker Compose
See `docker-compose.yml` for volume mounts and port mappings.

## Troubleshooting

- **"Could not find YBComponents"**: Ensure `GITHUB_TOKEN` is passed at build time and has `repo` scope access.
- **"Permission denied" for /srv/shiny-server**: Ensure the volume mount has correct permissions (e.g., owned by rstudio user).
- **Image build fails**: Check that all system libraries are correctly installed. Run with `--progress=plain` for detailed output:
  ```bash
  docker build --progress=plain --build-arg GITHUB_TOKEN=$GITHUB_TOKEN -t ... .
  ```

## Image Size

The Dockerfile has been optimized for minimal image size:
- Consolidated `apt-get update/install` with same-layer cleanup (est. **50–100 MB** savings)
- Merged `install2.r` calls to reduce layer overhead
- Added `--no-install-recommends` and `--no-cache-dir` flags
- **Inherited optimizations** from `yatiribio_docker_base` (300–700 MB savings)

## Project Context

For detailed architecture, version history, and known issues, see `CONTEXT.md` in the parent repository.
