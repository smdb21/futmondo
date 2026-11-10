FROM rocker/shiny:4.6.1

# Keep dependency installation cached when only application code changes.
# Limit build concurrency for smaller VPS instances.
ARG NCPUS=2
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev libssl-dev libsodium-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/* \
    && install2.r --error --skipinstalled -n "${NCPUS}" \
        curl data.table digest dplyr htmltools htmlwidgets httr jsonlite \
        later openssl plotly promises reactable scales shiny \
        shinydashboard shinydashboardPlus sodium waiter xml2 \
    && rm -rf /tmp/downloaded_packages

WORKDIR /srv/shiny-server/futmondo
COPY --chown=shiny:shiny . ./
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838
# Retain Rocker's startup so Dokploy runtime environment variables reach R.
CMD ["/init"]
