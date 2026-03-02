FROM rocker/r-ver:4.3.1

# Instalar dependencias del sistema necesarias
RUN apt-get update && apt-get install -y \
    build-essential \
    gfortran \
    g++ \
    cmake \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libnlopt-dev \
    libglpk-dev \
    liblapack-dev \
    libblas-dev \
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
    'DT', \
    'scales' \
    ), repos='https://cloud.r-project.org', Ncpus=2)"

WORKDIR /app
COPY . /app

EXPOSE 10000

CMD R -e "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"
