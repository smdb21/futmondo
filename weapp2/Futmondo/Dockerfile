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

# Customize the installed OSS server before copying frequently changing app code.
# Validate its actual upstream source and protocol; fail the build on incompatibility.
COPY scripts/patch-shiny-reconnect.cjs /tmp/futmondo-build/scripts/
COPY test/test_shiny_reconnect.cjs /tmp/futmondo-build/test/
RUN /opt/shiny-server/bin/node /tmp/futmondo-build/scripts/patch-shiny-reconnect.cjs /opt/shiny-server \
    && /opt/shiny-server/bin/node /tmp/futmondo-build/test/test_shiny_reconnect.cjs /opt/shiny-server \
    && Rscript -e 'invisible(parse(file="/opt/shiny-server/R/SockJSAdapter.R"))' \
    && rm -rf /tmp/futmondo-build

WORKDIR /srv/shiny-server/futmondo
COPY --chown=shiny:shiny *.R player_columns_to_hide.txt ./
COPY --chown=shiny:shiny Modules/ ./Modules/
COPY --chown=shiny:shiny www/ ./www/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838
# Retain Rocker's startup so Dokploy runtime environment variables reach R.
CMD ["/init"]
