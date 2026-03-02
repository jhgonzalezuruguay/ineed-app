FROM rocker/shiny:4.3.1

# Dependencias del sistema completas para sf, terra, lme4
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdal-bin \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libnlopt-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libabsl-dev \
    build-essential \
    gfortran \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R
RUN R -e "install.packages(c( \
'shiny', \
'shinydashboard', \
'dplyr', \
'tidyr', \
'data.table', \
'lme4', \
'lmerTest', \
'performance', \
'ggplot2', \
'plotly', \
'sf', \
'rnaturalearth', \
'rnaturalearthdata', \
'DT', \
'scales' \
), repos='https://cloud.r-project.org/')"

COPY . /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
