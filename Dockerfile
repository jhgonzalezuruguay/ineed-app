FROM rocker/shiny:4.3.2

# Instalar dependencias del sistema necesarias
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R
RUN R -e "install.packages(c('shinydashboard','dplyr','tidyr','data.table','lme4','lmerTest',
      'performance',
      'ggplot2',
      'plotly',
      'sf',
      'rnaturalearth',
      'rnaturalearthdata',
      'DT',
      'scales'), repos='https://cloud.r-project.org/')"

# Copiar app
COPY app.R /srv/shiny-server/

# Exponer puerto
EXPOSE 3838

# Usar shiny-server (ya viene configurado)
CMD ["/usr/bin/shiny-server"]
