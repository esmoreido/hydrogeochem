# syntax=docker/dockerfile:1

# rocker/shiny-verse = Ubuntu 24.04 + R 4.6.1 + shiny-server + tidyverse.
# Rprofile.site already points at Posit Package Manager (Linux binaries) and
# sets HTTPUserAgent so those binaries are actually served. Do NOT overwrite it.
FROM rocker/shiny-verse:4.6.1

# shinyauthr -> sodium -> libsodium. Absent from the base image.
RUN apt-get update \
 && apt-get install -y --no-install-recommends libsodium-dev \
 && rm -rf /var/lib/apt/lists/*

# Listen on 8180 (shiny-server default is 3838).
RUN sed -i 's/listen 3838;/listen 8180;/' /etc/shiny-server/shiny-server.conf

# R packages. Installed BEFORE COPY so editing app.R doesn't rebuild this layer.
# NB: each R expression is kept on ONE line on purpose. Docker's parser knows
# nothing about R string literals -- it only continues a RUN on a trailing '\',
# so a multi-line R string without one gets split and the next line is read as
# a fresh instruction ("unknown instruction: 'tidyverse'").
RUN R -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform[['pkgType']], R.Version()[['os']], R.Version()[['arch']]))"

RUN R -e "pak::pkg_install(c('tidyverse','shinyauthr','shinyjs','shinydashboard','shinyWidgets','shinycssloaders','DT','plotly','dygraphs','googlesheets4','readxl','xts','lubridate','RColorBrewer'))" \
 && rm -rf /tmp/Rtmp* /root/.cache/R

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
