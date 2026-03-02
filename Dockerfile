FROM rocker/shiny:4.3.1

# Instalar dependencias del sistema necesarias para sf, lme4, etc.
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdal-bin \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    build-essential \
    gfortran \
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

# Copiar archivos de la app
COPY . /srv/shiny-server/

# Permisos
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
