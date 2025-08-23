FROM rocker/shiny-verse

COPY . /hydrogeochem
WORKDIR /hydrogeochem

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform[['pkgType']], R.Version()[['os']], R.Version()[['arch']]))"
RUN R -e "pak::pkg_install('renv@1.1.5')"
RUN R -e "renv::restore()"

EXPOSE 8180
CMD Rscript /hydrogeochem/app.R