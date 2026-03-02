FROM rocker/r-ver:4.3.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libnlopt-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

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
    ), repos='https://cloud.r-project.org')"

WORKDIR /app
COPY . /app

EXPOSE 10000

CMD R -e "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"
