FROM rocker/r2u:jammy

LABEL org.opencontainers.image.authors="Emilie Chafai <emilie.chafai@univ-lyon2.fr>, Lise Vaudor <lise.vaudor@ens-lyon.fr>, Samuel Dunesme <samuel.dunesme@ens-lyon.fr>"
LABEL org.opencontainers.image.source="https://github.com/emiliec08/JACOB"
LABEL org.opencontainers.image.documentation="https://github.com/emiliec08/JACOB"
LABEL org.opencontainers.image.description="An application about collective gardens."

RUN locale-gen fr_FR.UTF-8

RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("osmdata")'
RUN Rscript -e 'install.packages("sf")'
RUN Rscript -e 'install.packages("leaflet")'
RUN Rscript -e 'install.packages("DT")'
RUN Rscript -e 'install.packages("shinyjs")'
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("RPostgres")'
RUN Rscript -e 'install.packages("glue")'

RUN mkdir /app
ADD . /app
WORKDIR /app

EXPOSE 3841

RUN groupadd -g 1010 app && useradd -c 'app' -u 1010 -g 1010 -m -d /home/app -s /sbin/nologin app
USER app

CMD  ["R", "-e", "shiny::runApp('.', port=3841, host='0.0.0.0')"]