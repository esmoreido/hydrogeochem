# syntax=docker/dockerfile:1

# rocker/shiny-verse = Ubuntu 24.04 + R 4.6.1 + shiny-server + tidyverse.
# Rprofile.site already points at Posit Package Manager (Linux binaries) and
# sets HTTPUserAgent so those binaries are actually served. Do NOT overwrite it.
FROM rocker/shiny-verse:4.6.1

# System libs for R packages that compile against them:
#   libsodium-dev  shinyauthr -> sodium (CRAN SystemRequirements: libsodium >= 1.0.3)
#   libssl-dev     googlesheets4 -> gargle/httr2 -> openssl
#                  (NOT in rocker's install_tidyverse.sh dep list; no-op if present)
RUN apt-get update \
 && apt-get install -y --no-install-recommends libsodium-dev libssl-dev \
 && rm -rf /var/lib/apt/lists/*

# Listen on 8180 (shiny-server default is 3838).
RUN sed -i 's/listen 3838;/listen 8180;/' /etc/shiny-server/shiny-server.conf

# R packages, via plain install.packages(). Installed BEFORE COPY so editing
# app.R doesn't rebuild this layer.
#
# rocker/shiny-verse ALREADY ships tidyverse, devtools, rmarkdown, arrow, DBI,
# RSQLite, readxl, lubridate, RColorBrewer (via scales) -- see
# scripts/install_tidyverse.sh -- so install.packages() will skip/ignore them.
#
# Kept on ONE line: Docker only continues a RUN on a trailing '\', it knows
# nothing about shell/R quoting.
#
# install.packages() only WARNS on failure, so the trailing stopifnot() turns a
# broken layer into a failed build instead of an image that boots into
# "there is no package called ...". requireNamespace() also loads each
# namespace, so a missing system lib (libsodium, libssl) fails here too.
RUN R -e "p <- c('shinyauthr','shinyjs','shinydashboard','shinyWidgets','shinycssloaders','DT','plotly','dygraphs','googlesheets4','readxl','xts','lubridate','RColorBrewer'); install.packages(p, Ncpus = parallel::detectCores()); stopifnot(all(vapply(p, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))))" \
 && rm -rf /tmp/Rtmp*

# shiny-server serves ONLY /srv/shiny-server. App lands at /hydrogeochem/.
# .secrets/, *.rds and user_base.csv are gitignored but NOT dockerignored --
# see .dockerignore. They must be present.
COPY . /srv/shiny-server/hydrogeochem

# run_as user is `shiny`. It must be able to read the app AND write
# chem_data.rds (Обновить данные) and .secrets/ (gargle token rotation).
RUN chown -R shiny:shiny /srv/shiny-server/hydrogeochem \
 && chmod -R u+rwX,go+rX /srv/shiny-server/hydrogeochem

EXPOSE 8180

# Inherited from the base image. Do NOT replace with `Rscript app.R`.
CMD ["/init"]
