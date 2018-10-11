FROM rocker/verse:3.5.0

USER rstudio

COPY packages.R /tmp/packages.R

RUN Rscript /tmp/packages.R

USER root

ENV TZ=America/New_York

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
