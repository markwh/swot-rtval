FROM rocker/geospatial

MAINTAINER Mark Hagemann (mark.hagemann@gmail.com)

# Add shiny (see https://github.com/rocker-org/rocker/issues/235#issuecomment-299036810)
RUN export ADD=shiny && bash /etc/cont-init.d/add

## Install packages from CRAN
RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    shinyFiles \
    plotly \
    viridisLite \
    ## install Github packages
    ## https://stackoverflow.com/a/43215405
    && installGithub.r markwh/rivertile \ 
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## assume shiny app is in build folder /shiny
COPY ./shiny/ /srv/shiny-server/shiny/
COPY ./data/ /srv/shiny-server/data/
