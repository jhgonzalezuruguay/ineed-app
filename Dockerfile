FROM rocker/shiny:4.3.1

# Instalar dependencias del sistema para sf
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R necesarios
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
), repos='https://cloud.r-project.org')"

# Copiar archivos al contenedor
COPY . /srv/shiny-server/

# Permisos
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
